# Helpers for generating and storing daily predictions and assigning risk tiers

library(tidyverse)
library(yardstick)
source("R/features.R")

#' Generate predictions for new data and append them to a SQLite database
#'
#' @param model Fitted workflow produced by `train_model()`
#' @param new_raw Tibble of raw incident data for the prediction date
#' @param db_path Path to the SQLite database file
#' @return Tibble of officer predictions that were stored
predict_and_store <- function(model, new_raw, db_path = "data/eis_predictions.sqlite") {
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("DBI and RSQLite packages are required for database storage")
  }
  features <- build_features(new_raw)
  probs <- predict(model, features, type = "prob")$.pred_1
  preds <- tibble(
    officer_id = features$officer_id,
    prediction_date = Sys.Date(),
    risk_prob = probs
  )
  save_predictions_db(preds, db_path)
  preds
}

#' Append prediction rows to a SQLite database
#'
#' @param preds Tibble with `officer_id`, `prediction_date`, and `risk_prob`
#' @param db_path Path to the SQLite database file
save_predictions_db <- function(preds, db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "predictions", preds, append = TRUE, row.names = FALSE)
}

#' Categorise officers into risk tiers using historical predictions
#'
#' The risk score combines the mean predicted probability over the last year
#' with the upward trend in scores.  Thresholds for high and moderate risk can
#' be supplied directly or, if historical outcomes are provided, are chosen to
#' maximise accuracy while minimising false negatives.
#'
#' @param db_path Path to the SQLite database populated by `predict_and_store()`
#' @param window_days Number of trailing days to consider when computing scores
#' @param labels Optional tibble with columns `officer_id` and binary `outcome`
#'   used to derive optimal thresholds.
#' @param high_threshold Optional numeric cutoff for high risk classification.
#' @param moderate_threshold Optional cutoff for moderate risk. If `NULL`, it is
#'   determined from the data in the same manner as `high_threshold`.
#' @return Tibble of officers with risk scores and categories
categorize_risk <- function(db_path, window_days = 365, labels = NULL,
                            high_threshold = NULL, moderate_threshold = NULL) {
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("DBI and RSQLite packages are required for database storage")
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  preds <- DBI::dbReadTable(con, "predictions") %>%
    mutate(prediction_date = as.Date(prediction_date)) %>%
    filter(prediction_date >= Sys.Date() - window_days)

  stats <- preds %>%
    group_by(officer_id) %>%
    summarise(
      mean_prob = mean(risk_prob, na.rm = TRUE),
      trend = if (n() > 1) coef(lm(risk_prob ~ as.numeric(prediction_date)))[2] else 0,
      .groups = "drop"
    ) %>%
    mutate(
      mean_z = as.numeric(scale(mean_prob)),
      trend_z = as.numeric(scale(trend)),
      risk_score = mean_z + trend_z
    )

  if (!is.null(labels)) {
    stats <- stats %>% left_join(labels, by = "officer_id")
  }

  derive_thresh <- function(data, max_val) {
    rng <- seq(min(data$risk_score, na.rm = TRUE), max_val, length.out = 100)
    perf <- map_dfr(rng, function(th) {
      cls <- factor(ifelse(data$risk_score >= th, 1, 0), levels = c(0, 1))
      metrics <- yardstick::metric_set(accuracy, sens)(
        tibble(outcome = data$outcome, pred = cls),
        truth = outcome, estimate = pred
      )
      tibble(
        threshold = th,
        accuracy = metrics$.estimate[metrics$.metric == "accuracy"],
        sens = metrics$.estimate[metrics$.metric == "sens"]
      )
    })
    perf %>% arrange(desc(accuracy), desc(sens)) %>% slice(1) %>% pull(threshold)
  }

  if (is.null(high_threshold) && !is.null(stats$outcome)) {
    high_threshold <- derive_thresh(stats, max(stats$risk_score, na.rm = TRUE))
  }
  if (is.null(moderate_threshold) && !is.null(stats$outcome)) {
    remaining <- stats %>% filter(risk_score < high_threshold)
    moderate_threshold <- derive_thresh(remaining, high_threshold)
  }

  stats %>%
    mutate(
      risk_category = case_when(
        risk_score >= high_threshold ~ "high",
        risk_score >= moderate_threshold ~ "moderate",
        TRUE ~ "low"
      )
    ) %>%
    select(officer_id, risk_score, risk_category)
}

