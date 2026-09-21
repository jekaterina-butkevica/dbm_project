# pairing_functions.R
# Functions for generating candidate DBM generation pairs


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
      
      # Exact midpoint times
      peak_1_time =
        data$mid_time[peak_1],
      
      peak_2_time =
        data$mid_time[peak_2],
      
      # Calendar dates used for daily meteorological data
      peak_1_date = as.Date(
        floor(peak_1_time),
        origin = "1970-01-01"
      ),
      
      peak_2_date = as.Date(
        floor(peak_2_time),
        origin = "1970-01-01"
      ),
      
      # Exact interval length between peaks
      generation_days =
        peak_2_time -
        peak_1_time,
      
      # Peak diagnostics
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
      
      # Conservative pair-level diagnostics
      pair_min_prominence =
        pmin(
          peak_1_prominence,
          peak_2_prominence
        ),
      
      pair_min_relative_to_max =
        pmin(
          peak_1_relative_to_max,
          peak_2_relative_to_max
        ),
      
      pair_edge_support =
        pmin(
          peak_1_edge,
          peak_2_edge
        )
    )
  
  return(pairs)
}
