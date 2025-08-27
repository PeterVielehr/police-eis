# Early Intervention System pipeline in R using tidyverse and tidymodels

library(tidyverse)
library(tidymodels)
library(fastshap)
source("R/features.R")

#' Load raw incident data
#'
#' @param path Path to a CSV file containing officer incidents.  The file is
#'   expected to include columns identifying the officer, event type, and any
#'   additional attributes used for feature engineering (for example
#'   `event_datetime`, `suspension_type`, `incident_type`, `shift_type`, etc.).
#'   A binary `outcome` flag is expected for modelling.
#' @return A tibble with the raw data.
load_data <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

# -----------------------------------------------------------------------------
# Feature engineering helpers -------------------------------------------------
# -----------------------------------------------------------------------------

# Each function below translates the feature engineering blocks implemented in
# the original Python pipeline into tidyverse operations.  The functions assume
# that the relevant columns exist in the input data and return one row per
# officer with the engineered features for that block.

build_incidents_features <- function(df, max_date) {
  suspensions <- df %>%
    filter(event_type == "suspension") %>%
    mutate(suspension_type = coalesce(suspension_type, "unknown")) %>%
    count(officer_id, suspension_type, name = "n") %>%
    pivot_wider(names_from = suspension_type, values_from = n,
                names_prefix = "susp_", values_fill = 0)

  suspension_hours <- df %>%
    filter(event_type == "suspension") %>%
    mutate(suspension_type = coalesce(suspension_type, "unknown")) %>%
    group_by(officer_id, suspension_type) %>%
    summarise(hours = sum(hours, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = suspension_type, values_from = hours,
                names_prefix = "susp_hours_", values_fill = 0)

  interventions <- df %>%
    filter(event_type == "intervention") %>%
    mutate(intervention_type = coalesce(intervention_type, "unknown")) %>%
    count(officer_id, intervention_type, name = "n") %>%
    pivot_wider(names_from = intervention_type, values_from = n,
                names_prefix = "intervention_", values_fill = 0)

  allegations <- df %>%
    filter(event_type == "allegation") %>%
    count(officer_id, name = "all_allegations")

  incidents_of_type <- df %>%
    filter(event_type == "incident") %>%
    mutate(incident_type = coalesce(incident_type, "unknown")) %>%
    count(officer_id, incident_type, name = "n") %>%
    pivot_wider(names_from = incident_type, values_from = n,
                names_prefix = "incident_", values_fill = 0)

  complaints_source <- df %>%
    filter(event_type == "complaint") %>%
    mutate(source = coalesce(source, "unknown")) %>%
    count(officer_id, source, name = "n") %>%
    pivot_wider(names_from = source, values_from = n,
                names_prefix = "complaint_source_", values_fill = 0)

  last_alleg <- df %>%
    filter(event_type == "allegation") %>%
    group_by(officer_id) %>%
    summarise(
      days_since_last_allegation = as.numeric(max_date - max(event_datetime)),
      .groups = "drop"
    )

  list(suspensions, suspension_hours, interventions, allegations,
       incidents_of_type, complaints_source, last_alleg) %>%
    reduce(full_join, by = "officer_id") %>%
    replace(is.na(.), 0)
}

build_compliments_features <- function(df) {
  df %>%
    filter(event_type == "compliment") %>%
    count(officer_id, name = "compliments")
}

build_shifts_features <- function(df) {
  shift_counts <- df %>%
    filter(event_type == "shift") %>%
    mutate(shift_type = coalesce(shift_type, "unknown")) %>%
    count(officer_id, shift_type, name = "n") %>%
    pivot_wider(names_from = shift_type, values_from = n,
                names_prefix = "shift_", values_fill = 0)

  hours_per_shift <- df %>%
    filter(event_type == "shift") %>%
    group_by(officer_id) %>%
    summarise(avg_shift_hours = mean(shift_hours, na.rm = TRUE),
              .groups = "drop")

  full_join(shift_counts, hours_per_shift, by = "officer_id") %>%
    replace(is.na(.), 0)
}

build_arrests_features <- function(df) {
  arrests <- df %>%
    filter(event_type == "arrest") %>%
    count(officer_id, name = "arrests")

  arrests_type <- df %>%
    filter(event_type == "arrest") %>%
    mutate(arrest_type = coalesce(arrest_type, "unknown")) %>%
    count(officer_id, arrest_type, name = "n") %>%
    pivot_wider(names_from = arrest_type, values_from = n,
                names_prefix = "arrest_type_", values_fill = 0)

  arrests_dow <- df %>%
    filter(event_type == "arrest") %>%
    mutate(dow = lubridate::wday(event_datetime, label = TRUE)) %>%
    count(officer_id, dow, name = "n") %>%
    pivot_wider(names_from = dow, values_from = n,
                names_prefix = "arrest_dow_", values_fill = 0)

  suspects_race <- df %>%
    filter(event_type == "arrest") %>%
    mutate(suspect_race = coalesce(suspect_race, "unknown")) %>%
    count(officer_id, suspect_race, name = "n") %>%
    pivot_wider(names_from = suspect_race, values_from = n,
                names_prefix = "suspect_race_", values_fill = 0)

  suspects_ethnicity <- df %>%
    filter(event_type == "arrest") %>%
    mutate(suspect_ethnicity = coalesce(suspect_ethnicity, "unknown")) %>%
    count(officer_id, suspect_ethnicity, name = "n") %>%
    pivot_wider(names_from = suspect_ethnicity, values_from = n,
                names_prefix = "suspect_eth_", values_fill = 0)

  crime_type <- df %>%
    filter(event_type == "arrest") %>%
    mutate(crime_type = coalesce(crime_type, "unknown")) %>%
    count(officer_id, crime_type, name = "n") %>%
    pivot_wider(names_from = crime_type, values_from = n,
                names_prefix = "crime_type_", values_fill = 0)

  list(arrests, arrests_type, arrests_dow, suspects_race,
       suspects_ethnicity, crime_type) %>%
    reduce(full_join, by = "officer_id") %>%
    replace(is.na(.), 0)
}

build_traffic_stop_features <- function(df) {
  traffic <- df %>%
    filter(event_type == "traffic_stop")

  base_counts <- traffic %>%
    count(officer_id, name = "traffic_stops")

  search <- traffic %>%
    group_by(officer_id) %>%
    summarise(
      traffic_with_search = sum(search_conducted == TRUE, na.rm = TRUE),
      traffic_with_uof = sum(use_of_force == TRUE, na.rm = TRUE),
      traffic_with_arrest = sum(arrest_made == TRUE, na.rm = TRUE),
      traffic_with_injury = sum(injury == TRUE, na.rm = TRUE),
      traffic_with_officer_injury = sum(officer_injury == TRUE, na.rm = TRUE),
      traffic_with_search_request = sum(search_requested == TRUE, na.rm = TRUE),
      .groups = "drop"
    )

  by_race <- traffic %>%
    mutate(driver_race = coalesce(driver_race, "unknown")) %>%
    count(officer_id, driver_race, name = "n") %>%
    pivot_wider(names_from = driver_race, values_from = n,
                names_prefix = "ts_race_", values_fill = 0)

  by_type <- traffic %>%
    mutate(stop_type = coalesce(stop_type, "unknown")) %>%
    count(officer_id, stop_type, name = "n") %>%
    pivot_wider(names_from = stop_type, values_from = n,
                names_prefix = "ts_type_", values_fill = 0)

  list(base_counts, search, by_race, by_type) %>%
    reduce(full_join, by = "officer_id") %>%
    replace(is.na(.), 0)
}

build_characteristics_features <- function(df) {
  char <- df %>%
    select(officer_id, education, military, academy_score, rank) %>%
    distinct()

  education <- char %>%
    mutate(education = coalesce(education, "unknown"), value = 1) %>%
    select(officer_id, education, value) %>%
    pivot_wider(names_from = education, values_from = value,
                names_prefix = "edu_", values_fill = 0)

  military <- char %>%
    transmute(officer_id, military = as.integer(coalesce(military, FALSE)))

  academy <- char %>%
    group_by(officer_id) %>%
    summarise(academy_score = mean(academy_score, na.rm = TRUE),
              .groups = "drop")

  rank <- char %>%
    mutate(rank = coalesce(rank, "unknown"), value = 1) %>%
    select(officer_id, rank, value) %>%
    pivot_wider(names_from = rank, values_from = value,
                names_prefix = "rank_", values_fill = 0)

  list(education, military, academy, rank) %>%
    reduce(full_join, by = "officer_id") %>%
    replace(is.na(.), 0)
}

build_employment_features <- function(df) {
  df %>%
    filter(event_type == "employment") %>%
    group_by(officer_id) %>%
    summarise(
      outside_employment_hours = sum(hours, na.rm = TRUE),
      outside_employment_income = sum(income, na.rm = TRUE),
      .groups = "drop"
    )
}

build_demographic_features <- function(df) {
  df %>%
    filter(event_type == "demographic") %>%
    group_by(officer_id) %>%
    summarise(
      Arrests311Call = mean(`311_calls`, na.rm = TRUE),
      Arrests311Requests = mean(`311_requests`, na.rm = TRUE),
      PopulationDensity = mean(population_density, na.rm = TRUE),
      AgeOfResidents = mean(age_of_residents, na.rm = TRUE),
      BlackPopulation = mean(black_population, na.rm = TRUE),
      HouseholdIncome = mean(household_income, na.rm = TRUE),
      EmploymentRate = mean(employment_rate, na.rm = TRUE),
      VacantLandArea = mean(vacant_land_area, na.rm = TRUE),
      VoterParticipation = mean(voter_participation, na.rm = TRUE),
      AgeOfDeath = mean(age_of_death, na.rm = TRUE),
      HousingDensity = mean(housing_density, na.rm = TRUE),
      NuisanceViolations = mean(nuisance_violations, na.rm = TRUE),
      ViolentCrimeRate = mean(violent_crime_rate, na.rm = TRUE),
      PropertyCrimeRate = mean(property_crime_rate, na.rm = TRUE),
      SidewalkAvailability = mean(sidewalk_availability, na.rm = TRUE),
      Foreclosures = mean(foreclosures, na.rm = TRUE),
      DisorderCallRate = mean(disorder_call_rate, na.rm = TRUE),
      .groups = "drop"
    )
}

# -----------------------------------------------------------------------------
# Master feature builder ------------------------------------------------------
# -----------------------------------------------------------------------------

#' Build per-officer features by combining all feature blocks.
#'
#' @param df Tibble returned by `load_data`.
#' @return Tibble of engineered features summarised at the officer level.
build_features <- function(df) {
  max_date <- max(df$event_datetime, na.rm = TRUE)

  incidents <- build_incidents_features(df, max_date)
  compliments <- build_compliments_features(df)
  shifts <- build_shifts_features(df)
  arrests <- build_arrests_features(df)
  traffic <- build_traffic_stop_features(df)
  characteristics <- build_characteristics_features(df)
  employment <- build_employment_features(df)
  demographic <- build_demographic_features(df)
  bodycam <- build_bodycam_features(df)
  peer_context <- build_peer_context_features(df)

  list(incidents, compliments, shifts, arrests, traffic,
       characteristics, employment, demographic, bodycam, peer_context) %>%
    reduce(full_join, by = "officer_id") %>%
    replace(is.na(.), 0)
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

