# peak_functions.R
# Functions for detecting candidate flight-activity peaks


detect_candidate_peaks <- function(data) {
  
  data <- data %>%
    arrange(mid_time) %>%
    mutate(
      activity = total_count / trap_days,
      observation_index = row_number()
    )
  
  
  # Identify runs of exactly equal activity -------------------
  
  same_as_previous <-
    data$activity ==
    dplyr::lag(data$activity)
  
  same_as_previous[is.na(same_as_previous)] <- FALSE
  
  data$plateau_id <- cumsum(
    !same_as_previous
  )
  
  
  # Summarise each single point or flat plateau ---------------
  
  peak_groups <- data %>%
    group_by(plateau_id) %>%
    summarise(
      Year =
        first(Year),
      
      Site =
        first(Site),
      
      activity =
        first(activity),
      
      plateau_start_index =
        min(observation_index),
      
      plateau_end_index =
        max(observation_index),
      
      plateau_n =
        n(),
      
      plateau_start_time =
        min(mid_time),
      
      plateau_end_time =
        max(mid_time),
      
      mid_time =
        mean(mid_time),
      
      .groups = "drop"
    )
  
  
  # Activity immediately outside each group -------------------
  
  peak_groups <- peak_groups %>%
    mutate(
      previous_activity =
        lag(activity),
      
      next_activity =
        lead(activity),
      
      rise =
        activity - previous_activity,
      
      drop =
        activity - next_activity,
      
      is_candidate_peak =
        !is.na(previous_activity) &
        !is.na(next_activity) &
        activity > previous_activity &
        activity > next_activity,
      
      local_prominence =
        pmin(
          rise,
          drop
        ),
      
      relative_prominence =
        local_prominence / activity,
      
      series_max_activity =
        max(activity, na.rm = TRUE),
      
      relative_to_max =
        activity / series_max_activity,
      
      mid_date =
        as.Date(
          floor(mid_time),
          origin = "1970-01-01"
        ),
      
      series_start_time =
        min(data$mid_time, na.rm = TRUE),
      
      series_end_time =
        max(data$mid_time, na.rm = TRUE),
      
      days_from_start =
        mid_time - series_start_time,
      
      days_to_end =
        series_end_time - mid_time,
      
      edge_distance =
        pmin(
          days_from_start,
          days_to_end
        ),
      
      observations_before =
        plateau_start_index - 1,
      
      observations_after =
        nrow(data) - plateau_end_index,
      
      observations_to_edge =
        pmin(
          observations_before,
          observations_after
        )
    )
  
  
  peak_groups
}
