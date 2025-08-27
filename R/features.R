# Feature engineering utilities

#' Body-worn camera features
#'
#' For each officer, compute body-worn camera activation rates along with
#' counts of manual shutoffs and incidents where footage is missing.
#'
#' @param df Tibble returned by `load_data`. Must include `officer_id`,
#'   `event_type`, `bodycam_on`, `manual_shutoff`, and `missing_footage`.
#' @return Tibble with one row per officer and bodycam features.
build_bodycam_features <- function(df) {
  df %>%
    filter(event_type == "bodycam") %>%
    group_by(officer_id) %>%
    summarise(
      bodycam_activation_rate = mean(bodycam_on == TRUE, na.rm = TRUE),
      bodycam_manual_shutoffs = sum(manual_shutoff == TRUE, na.rm = TRUE),
      bodycam_missing_footage = sum(missing_footage == TRUE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    replace(is.na(.), 0)
}

#' Peer context features
#' 
#' For each officer, count the number of incidents co-assigned with colleagues
#' who have recent complaints or uses of force. Optionally compute network
#' centrality measures (degree and betweenness) on the co-assignment graph.
#'
#' @param df Tibble returned by `load_data`. Must include `incident_id`,
#'   `officer_id`, `event_type`, and `event_datetime`.
#' @param lookback_days Numeric window defining "recent" in days.
#' @return Tibble with one row per officer and peer context features.
build_peer_context_features <- function(df, lookback_days = 90) {
  incidents <- df %>%
    filter(event_type == "incident") %>%
    select(incident_id, officer_id, event_datetime) %>%
    distinct()

  if (nrow(incidents) == 0) {
    return(tibble(officer_id = df$officer_id %>% unique()) %>%
             mutate(co_flagged_complaint = 0,
                    co_flagged_use_of_force = 0,
                    peer_degree = 0,
                    peer_betweenness = 0))
  }

  flag_events <- df %>%
    filter(event_type %in% c("complaint", "use_of_force")) %>%
    select(officer_id, flag_type = event_type, flag_datetime = event_datetime)

  pairs <- incidents %>%
    rename(officer_id_main = officer_id,
           incident_datetime = event_datetime) %>%
    inner_join(incidents, by = "incident_id", suffix = c("_main", "_colleague")) %>%
    filter(officer_id_main != officer_id_colleague)

  flagged_pairs <- pairs %>%
    left_join(flag_events, by = c("officer_id_colleague" = "officer_id")) %>%
    filter(!is.na(flag_datetime),
           flag_datetime <= incident_datetime,
           flag_datetime >= incident_datetime - lubridate::days(lookback_days))

  co_flag_counts <- flagged_pairs %>%
    group_by(officer_id_main, flag_type) %>%
    summarise(n = n_distinct(incident_id), .groups = "drop") %>%
    pivot_wider(names_from = flag_type, values_from = n,
                names_prefix = "co_flagged_", values_fill = 0)

  if (!requireNamespace("igraph", quietly = TRUE) || nrow(pairs) == 0) {
    centrality <- tibble(officer_id = unique(incidents$officer_id),
                         peer_degree = 0,
                         peer_betweenness = 0)
  } else {
    edges <- pairs %>%
      transmute(
        from = pmin(officer_id_main, officer_id_colleague),
        to = pmax(officer_id_main, officer_id_colleague)
      ) %>%
      distinct()

    g <- igraph::graph_from_data_frame(edges, directed = FALSE)

    centrality <- tibble(
      officer_id = igraph::V(g)$name,
      peer_degree = igraph::degree(g),
      peer_betweenness = igraph::betweenness(g, normalized = TRUE)
    )
  }

  incidents %>%
    distinct(officer_id) %>%
    left_join(co_flag_counts, by = c("officer_id" = "officer_id_main")) %>%
    left_join(centrality, by = "officer_id") %>%
    replace(is.na(.), 0)
}
