# Feature engineering helpers for the R EIS pipeline

library(tidyverse)

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
      .groups = "drop",
    )

  list(suspensions, suspension_hours, interventions, allegations,
       incidents_of_type, complaints_source, last_alleg) %>%
    reduce(full_join, by = "officer_id") %>%
    replace(is.na(.), 0)
}

#' Detailed complaint outcome metrics including sustained and unsustained counts
#'
#' @param df Feature engineering tibble
#' @param max_date Maximum event date in the dataset for computing recency
#' @return Tibble with complaint outcome counts and recency info
build_complaint_outcome_features <- function(df, max_date) {
  complaints <- df %>% filter(event_type == "complaint")

  if (nrow(complaints) == 0) {
    return(tibble(officer_id = unique(df$officer_id)))
  }

  by_outcome <- complaints %>%
    mutate(outcome = coalesce(outcome, "unknown")) %>%
    count(officer_id, outcome, name = "n") %>%
    pivot_wider(names_from = outcome, values_from = n,
                names_prefix = "complaint_outcome_", values_fill = 0)

  origin_outcome <- complaints %>%
    mutate(outcome = coalesce(outcome, "unknown"),
           source = coalesce(source, "unknown")) %>%
    count(officer_id, source, outcome) %>%
    mutate(col = paste0("complaint_", source, "_", outcome)) %>%
    select(officer_id, col, n) %>%
    pivot_wider(names_from = col, values_from = n, values_fill = 0)

  last_completed <- complaints %>%
    filter(!is.na(date_of_judgment)) %>%
    group_by(officer_id) %>%
    summarise(days_since_last_completed_allegation = as.numeric(max_date - max(date_of_judgment)),
              .groups = "drop")

  list(by_outcome, origin_outcome, last_completed) %>%
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
      .groups = "drop",
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

#' Aggregate use-of-force metrics, including unjustified force counts,
#' temporal trends, and disproportionate force ratios
#'
#' @param df Tibble of all events
#' @return Tibble summarising use-of-force attributes per officer
build_use_of_force_features <- function(df) {
  uof <- df %>% filter(event_type == "use_of_force")

  if (nrow(uof) == 0) {
    return(tibble(officer_id = unique(df$officer_id)))
  }

  max_date <- max(df$event_datetime, na.rm = TRUE)

  uof_type <- uof %>%
    mutate(use_of_force_type = coalesce(use_of_force_type, "unknown")) %>%
    count(officer_id, use_of_force_type, name = "n") %>%
    pivot_wider(names_from = use_of_force_type, values_from = n,
                names_prefix = "uof_type_", values_fill = 0)

  unjustified_type <- uof %>%
    filter(number_of_unjustified_allegations > 0) %>%
    mutate(use_of_force_type = coalesce(use_of_force_type, "unknown")) %>%
    count(officer_id, use_of_force_type, name = "n") %>%
    pivot_wider(names_from = use_of_force_type, values_from = n,
                names_prefix = "uof_unjust_", values_fill = 0)

  unjust_intervention <- uof %>%
    filter(number_of_unjustified_allegations > 0) %>%
    mutate(intervention_type = coalesce(intervention_type, "unknown")) %>%
    count(officer_id, intervention_type, name = "n") %>%
    pivot_wider(names_from = intervention_type, values_from = n,
                names_prefix = "uof_unjust_intervention_", values_fill = 0)

  suspect_injury <- uof %>%
    group_by(officer_id) %>%
    summarise(uof_with_suspect_injury = sum(suspect_injury == TRUE, na.rm = TRUE),
              .groups = "drop")

  temporal <- uof %>%
    mutate(days_ago = as.numeric(max_date - event_datetime)) %>%
    group_by(officer_id) %>%
    summarise(
      uof_last_90_days = sum(days_ago <= 90, na.rm = TRUE),
      uof_prev_90_days = sum(days_ago > 90 & days_ago <= 180, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(uof_change_90_days = uof_last_90_days - uof_prev_90_days)

  disproportionate <- uof %>%
    group_by(officer_id) %>%
    summarise(
      uof_resist = sum(in_response_to_resisting_arrest == TRUE, na.rm = TRUE),
      uof_no_resist = sum(in_response_to_resisting_arrest == FALSE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      uof_disproportionate = if_else(
        (uof_resist + uof_no_resist) > 0,
        uof_no_resist / (uof_resist + uof_no_resist),
        NA_real_
      )
    )

  list(uof_type, unjustified_type, unjust_intervention, suspect_injury,
       temporal, disproportionate) %>%
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
      .groups = "drop",
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
      .groups = "drop",
    )
}

#' Count incidents where peers have recent complaints or use of force
#'
#' @param df Tibble of all events
#' @param window_days Number of days defining "recent"
#' @return Tibble with peer context counts per officer
build_peer_context_features <- function(df, window_days = 90) {
  id_col <- if ("incident_id" %in% names(df)) "incident_id" else if ("event_id" %in% names(df)) "event_id" else NULL
  if (is.null(id_col)) {
    return(tibble(officer_id = unique(df$officer_id)))
  }

  incidents <- df %>%
    filter(event_type == "incident" & !is.na(.data[[id_col]])) %>%
    mutate(event_id = .data[[id_col]]) %>%
    select(event_id, officer_id, event_datetime)

  if (nrow(incidents) == 0) {
    return(tibble(officer_id = unique(df$officer_id)))
  }

  last_events <- df %>%
    filter(event_type %in% c("complaint", "use_of_force")) %>%
    group_by(officer_id, event_type) %>%
    summarise(last_event = max(event_datetime), .groups = "drop") %>%
    pivot_wider(names_from = event_type, values_from = last_event,
                names_prefix = "last_")

  incidents_peers <- incidents %>%
    left_join(last_events, by = "officer_id")

  peer_flags <- incidents_peers %>%
    group_by(event_id) %>%
    group_modify(~{
      n <- nrow(.x)
      map_dfr(seq_len(n), function(i) {
        others <- .x[-i, ]
        diff_comp <- as.numeric(difftime(.x$event_datetime[i], others$last_complaint, units = "days"))
        diff_uof <- as.numeric(difftime(.x$event_datetime[i], others$last_use_of_force, units = "days"))
        tibble(
          officer_id = .x$officer_id[i],
          peer_recent_complaint = any(!is.na(diff_comp) & diff_comp >= 0 & diff_comp <= window_days),
          peer_recent_uof = any(!is.na(diff_uof) & diff_uof >= 0 & diff_uof <= window_days)
        )
      })
    }) %>%
    ungroup() %>%
    group_by(officer_id) %>%
    summarise(
      incidents_with_peer_recent_complaint = sum(peer_recent_complaint, na.rm = TRUE),
      incidents_with_peer_recent_uof = sum(peer_recent_uof, na.rm = TRUE),
      .groups = "drop"
    )

  peer_flags
}

build_dispatch_time_features <- function(df) {
  dispatch <- df %>% filter(event_type == "dispatch")

  if (nrow(dispatch) == 0) {
    return(tibble(officer_id = unique(df$officer_id)))
  }

  times <- dispatch %>%
    mutate(
      minute = lubridate::minute(event_datetime),
      hour = lubridate::hour(event_datetime),
      dow = lubridate::wday(event_datetime, label = TRUE),
      quarter = lubridate::quarter(event_datetime),
      month = lubridate::month(event_datetime),
      year = lubridate::year(event_datetime)
    )

  minute_feat <- times %>%
    count(officer_id, minute) %>%
    pivot_wider(names_from = minute, values_from = n,
                names_prefix = "dispatch_minute_", values_fill = 0)

  hour_feat <- times %>%
    count(officer_id, hour) %>%
    pivot_wider(names_from = hour, values_from = n,
                names_prefix = "dispatch_hour_", values_fill = 0)

  dow_feat <- times %>%
    count(officer_id, dow) %>%
    pivot_wider(names_from = dow, values_from = n,
                names_prefix = "dispatch_dow_", values_fill = 0)

  quarter_feat <- times %>%
    count(officer_id, quarter) %>%
    pivot_wider(names_from = quarter, values_from = n,
                names_prefix = "dispatch_q_", values_fill = 0)

  month_feat <- times %>%
    count(officer_id, month) %>%
    pivot_wider(names_from = month, values_from = n,
                names_prefix = "dispatch_month_", values_fill = 0)

  year_feat <- times %>%
    count(officer_id, year) %>%
    pivot_wider(names_from = year, values_from = n,
                names_prefix = "dispatch_year_", values_fill = 0)

  list(minute_feat, hour_feat, dow_feat, quarter_feat, month_feat, year_feat) %>%
    reduce(full_join, by = "officer_id") %>%
    replace(is.na(.), 0)
}

#' Build per-officer features by combining all feature blocks.
#'
#' @param df Tibble returned by `load_data`.
#' @return Tibble of engineered features summarised at the officer level.
build_features <- function(df) {
  max_date <- max(df$event_datetime, na.rm = TRUE)

  incidents <- build_incidents_features(df, max_date)
  complaint_outcomes <- build_complaint_outcome_features(df, max_date)
  compliments <- build_compliments_features(df)
  shifts <- build_shifts_features(df)
  arrests <- build_arrests_features(df)
  traffic <- build_traffic_stop_features(df)
  uof <- build_use_of_force_features(df)
  peer_context <- build_peer_context_features(df)
  dispatch_time <- build_dispatch_time_features(df)
  characteristics <- build_characteristics_features(df)
  employment <- build_employment_features(df)
  demographic <- build_demographic_features(df)

  list(incidents, complaint_outcomes, compliments, shifts, arrests, traffic,
       uof, peer_context, dispatch_time, characteristics, employment, demographic) %>%
    reduce(full_join, by = "officer_id") %>%
    replace(is.na(.), 0)
}

