# ============================================================
# pairing_functions.R
# Functions for generating candidate DBM peak pairs
# ============================================================


# Šeit cenšos vienas Year_plus_Site sērijas ietvaros izveidot visus iespējamos pīķu pārus
generate_peak_pairs <- function(data) {
  
  data <- data %>%
    arrange(mid_time)
  
  pairs <- tidyr::crossing(
    peak_1 = seq_len(nrow(data)),
    peak_2 = seq_len(nrow(data))
  ) %>%
    filter(
      peak_2 > peak_1
    ) %>%
    mutate(
      
      # Exact midpoint time
      peak_1_time = data$mid_time[peak_1],
      peak_2_time = data$mid_time[peak_2],
      
      # Calendar date for daily meteorological data
      peak_1_date = as.Date(
        floor(peak_1_time),
        origin = "1970-01-01"
      ),
      
      peak_2_date = as.Date(
        floor(peak_2_time),
        origin = "1970-01-01"
      ),
      
      # Exact interval between peak midpoints
      generation_days =
        peak_2_time - peak_1_time,
      
      peak_1_strength =
        data$peak_strength[peak_1],
      
      peak_2_strength =
        data$peak_strength[peak_2],
      
      peak_1_prominence =
        data$relative_prominence[peak_1],
      
      peak_2_prominence =
        data$relative_prominence[peak_2],
      
      peak_1_relative_to_max =
        data$relative_to_max[peak_1],
      
      peak_2_relative_to_max =
        data$relative_to_max[peak_2],
      
      peak_1_edge =
        data$observations_to_edge[peak_1],
      
      peak_2_edge =
        data$observations_to_edge[peak_2],
      
      pair_peak_strength = pmin(
        peak_1_strength,
        peak_2_strength
      ),
      
      pair_edge_support = pmin(
        peak_1_edge,
        peak_2_edge
      )
    )
  
  return(pairs)
}
