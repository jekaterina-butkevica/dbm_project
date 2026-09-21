# peak_functions.R
# Functions for detecting candidate DBM flight peaks

detect_candidate_peaks <- function(data) {
  
  result <- data %>%
    arrange(mid_time) %>%
    mutate(
      
      # Standardized observed activity
      activity = total_count / trap_days,
      
      # Activity at neighbouring assessments
      previous_activity = lag(activity),
      next_activity = lead(activity),
      
      # Change before and after each observation
      rise = activity - previous_activity,
      fall = next_activity - activity,
      
      # Candidate local maximum
      is_candidate_peak =
        rise > 0 &
        fall < 0,
      
      # Size of decline after the candidate peak
      drop = activity - next_activity,
      
      # Conservative local prominence
      local_prominence = pmin(
        rise,
        drop
      ),
      
      # Prominence relative to peak height
      relative_prominence =
        local_prominence / activity,
      
      # Series-level activity context
      series_max_activity =
        max(activity, na.rm = TRUE),
      
      relative_to_max =
        activity / series_max_activity,
      
      # Position within the observed series
      series_start_time =
        min(mid_time, na.rm = TRUE),
      
      series_end_time =
        max(mid_time, na.rm = TRUE),
      
      days_from_start =
        mid_time - series_start_time,
      
      days_to_end =
        series_end_time - mid_time,
      
      # Distance to the nearest series boundary
      edge_distance =
        pmin(
          days_from_start,
          days_to_end
        ),
      
      # Position in the sequence of observations
      observation_index = row_number(),
      
      n_observations = n(),
      
      observations_before =
        observation_index - 1,
      
      observations_after =
        n_observations - observation_index,
      
      # Number of observations available on the weaker side
      observations_to_edge =
        pmin(
          observations_before,
          observations_after
        )
    )
  
  return(result)
}
