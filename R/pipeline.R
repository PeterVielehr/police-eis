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
#'   and SHAP values (if available).
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

  preds <- fitted %>%
    predict(test_data, type = "prob") %>%
    bind_cols(predict(fitted, test_data, type = "class")) %>%
    bind_cols(test_data %>% select(officer_id, outcome))

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

  list(model = fitted, metrics = metrics_tbl, confusion = confusion, shap = shap_vals)
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

