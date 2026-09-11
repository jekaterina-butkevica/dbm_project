# ============================================================
# pairing_functions.R
# Functions for generating candidate DBM peak pairs
# ============================================================


# Šeit cenšos vienas Year_plus_Site sērijas ietvaros izveidot visus iespējamos pīķu pārus
generate_peak_pairs <- function(data) {
  
  data <- data %>%
    arrange(mid_date)
  
  pairs <- tidyr::crossing(
    peak_1 = seq_len(nrow(data)),
    peak_2 = seq_len(nrow(data))
  ) %>%
    filter(
      peak_2 > peak_1
    ) %>%
    mutate(
      peak_1_date = data$mid_date[peak_1],
      peak_2_date = data$mid_date[peak_2],
      
      generation_days = as.numeric(
        peak_2_date - peak_1_date
      ),
      
      peak_1_strength = data$peak_strength[peak_1],
      peak_2_strength = data$peak_strength[peak_2],
      
      peak_1_edge = data$observations_to_edge[peak_1],
      peak_2_edge = data$observations_to_edge[peak_2],
      
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
