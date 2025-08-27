# Early Intervention System pipeline in R using tidyverse and tidymodels

library(tidyverse)
library(tidymodels)
library(fastshap)

# Load feature engineering helpers
source("R/features.R")

#' Load raw incident data
#'
#' @param path Path to a CSV file containing officer incidents. The file is
#'   expected to include columns identifying the officer, event type, and any
#'   additional attributes used for feature engineering.
#'   A binary `outcome` flag is expected for modelling.
#' @return A tibble with the raw data.
load_data <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

# -----------------------------------------------------------------------------
# Modelling ------------------------------------------------------------------
# -----------------------------------------------------------------------------

#' Train a model using tidymodels with optional XGBoost engine.
#'
#' @param features Tibble of engineered features.
#' @param labels Numeric vector of binary outcomes per officer.
#' @param model Character; either "logistic" (default) or "xgboost".
#' @return List containing the fitted model, evaluation metrics, confusion matrix,
#'   SHAP values (if available), and a global importance summary.
train_model <- function(features, labels, model = c("logistic", "xgboost")) {
  model <- match.arg(model)

  data <- features %>% mutate(outcome = labels)

  set.seed(123)
  split <- initial_split(data, prop = 0.8, strata = outcome)
  train_data <- training(split)
  test_data  <- testing(split)

  recipe <- recipe(outcome ~ ., data = train_data) %>%
    update_role(officer_id, new_role = "id") %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors())

  model_spec <- if (model == "xgboost") {
    boost_tree() %>%
      set_engine("xgboost") %>%
      set_mode("classification")
  } else {
    logistic_reg() %>%
      set_engine("glm")
  }

  workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(model_spec)

  fitted <- workflow %>% fit(train_data)

  prob_preds <- predict(fitted, test_data, type = "prob")
  preds <- prob_preds %>%
    bind_cols(test_data %>% select(officer_id, outcome))

  # Determine probability threshold that maximises accuracy while prioritising
  # sensitivity (minimising false negatives)
  thresh_seq <- seq(0.01, 0.99, by = 0.01)
  perf <- map_dfr(thresh_seq, function(th) {
    class_pred <- factor(ifelse(prob_preds$.pred_1 >= th, 1, 0), levels = c(0, 1))
    metrics <- yardstick::metric_set(accuracy, sens)(
      tibble(outcome = test_data$outcome, pred = class_pred),
      truth = outcome, estimate = pred
    )
    tibble(
      threshold = th,
      accuracy = metrics$.estimate[metrics$.metric == "accuracy"],
      sens = metrics$.estimate[metrics$.metric == "sens"]
    )
  })

  best_thresh <- perf %>% arrange(desc(accuracy), desc(sens)) %>% slice(1) %>% pull(threshold)

  preds <- preds %>%
    mutate(
      .pred_class = factor(ifelse(prob_preds$.pred_1 >= best_thresh, 1, 0), levels = c(0, 1))
    )

  metrics_tbl <- yardstick::metrics(preds, truth = outcome, estimate = .pred_class)
  confusion <- yardstick::conf_mat(preds, truth = outcome, estimate = .pred_class)

  shap_vals <- tryCatch({
    engine <- extract_fit_engine(fitted)
    X <- test_data %>% select(-outcome, -officer_id)
    pred_fun <- function(object, newdata) {
      if (inherits(object, "xgb.Booster")) {
        stats::predict(object, as.matrix(newdata))
      } else {
        stats::predict(object, newdata, type = "response")
      }
    }
    fastshap::explain(engine, X = X, pred_wrapper = pred_fun) %>%
      mutate(officer_id = test_data$officer_id)
  }, error = function(e) NULL)

  shap_summary <- tryCatch({
    summarize_shap(shap_vals)
  }, error = function(e) NULL)

  list(
    model = fitted,
    threshold = best_thresh,
    metrics = metrics_tbl,
    confusion = confusion,
    shap = shap_vals,
    shap_summary = shap_summary
  )
}

#' Summarise absolute SHAP contributions for global feature importance.
#'
#' @param shap_vals Tibble returned by `fastshap::explain()` with an `officer_id` column.
#' @return Tibble of mean absolute SHAP value per feature ordered from most to least important.
summarize_shap <- function(shap_vals) {
  if (is.null(shap_vals) || nrow(shap_vals) == 0) {
    return(NULL)
  }
  shap_vals %>%
    pivot_longer(-officer_id, names_to = "feature", values_to = "shap") %>%
    group_by(feature) %>%
    summarise(mean_abs_shap = mean(abs(shap), na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_abs_shap))
}

#' Plot top SHAP feature importances.
#'
#' @param shap_vals Tibble of SHAP values including `officer_id`.
#' @param top_n Number of features to display.
#' @return A `ggplot` object showing mean absolute SHAP value per feature.
plot_shap_importance <- function(shap_vals, top_n = 20) {
  summary <- summarize_shap(shap_vals)
  if (is.null(summary)) {
    return(NULL)
  }
  summary %>%
    slice_head(n = top_n) %>%
    ggplot(aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap)) +
    geom_col() +
    coord_flip() +
    labs(
      x = "Feature",
      y = "Mean |SHAP|",
      title = "SHAP feature importance"
    ) +
    theme_minimal()
}

#' Run the full pipeline given a path to raw data.
#'
#' @param path Path to the CSV file of incidents.
#' @param model Character; "logistic" or "xgboost".
#' @return List containing the trained model, metrics, confusion matrix and SHAP values.
run_pipeline <- function(path, model = c("logistic", "xgboost")) {
  raw <- load_data(path)
  features <- build_features(raw)
  labels <- raw %>%
    group_by(officer_id) %>%
    summarise(outcome = max(outcome), .groups = "drop") %>%
    pull(outcome)
  train_model(features, labels, model = model)
}

