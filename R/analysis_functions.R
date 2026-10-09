# analysis_functions.R
# Main reusable DBM analysis workflow


analyse_dbm_series <- function(
    moth_data,
    meteo_data,
    parameters
) {
  
  # Validate input data ---------------------------------------
  
  validate_moth_data(
    moth_data
  )
  
  validate_meteo_data(
    meteo_data
  )
  
  
  # Detect candidate peaks -----------------------------------
  
  all_peaks <- moth_data %>%
    group_by(
      Year_plus_Site
    ) %>%
    group_modify(
      ~ detect_candidate_peaks(.x)
    ) %>%
    ungroup()
  
  
  candidate_peaks <- all_peaks %>%
    filter(
      is_candidate_peak
    ) %>%
    select(
      Year_plus_Site,
      Year,
      Site,
      mid_date,
      mid_time,
      activity,
      relative_prominence,
      relative_to_max,
      observations_before,
      observations_after,
      observations_to_edge,
      edge_distance
    )
  
  
  # Generate peak pairs ---------------------------------------
  
  all_pairs <- candidate_peaks %>%
    group_by(
      Year_plus_Site
    ) %>%
    group_modify(
      ~ generate_peak_pairs(.x)
    ) %>%
    ungroup()
  
  
  candidate_pairs <- all_pairs %>%
    filter(
      generation_days >=
        parameters$generation_min_days,
      
      generation_days <=
        parameters$generation_max_days
    )
  
  
  # Calculate degree-days --------------------------------------
  
  dd_grid <- lapply(
    parameters$Tbase_values,
    function(tb) {
      
      dd_results <- lapply(
        seq_len(nrow(candidate_pairs)),
        function(i) {
          
          calculate_pair_degree_days(
            pair_row = candidate_pairs[i, ],
            meteo_data = meteo_data,
            Tbase = tb
          )
        }
      ) %>%
        bind_rows()
      
      bind_cols(
        candidate_pairs,
        dd_results
      )
    }
  ) %>%
    bind_rows()
  
  
  # Stop if temperature data are incomplete --------------------
  
  if (any(dd_grid$n_missing_temp > 0)) {
    stop(
      "Degree-day calculation contains missing temperature data."
    )
  }
  
  
  # Add pair-quality diagnostics -------------------------------
  
  pair_candidates <- add_pair_quality(
    dd_grid
  )
  
  
  # Select best pair for each Tbase ----------------------------
  
  selected_all_tbase <- select_pairs_from_K_profile(
    pair_candidates =
      pair_candidates,
    
    K_profile =
      parameters$K_profile
  )
  
  
  # Consensus across Tbase scenarios ---------------------------
  
  pair_consensus <- summarise_pair_consensus(
    selected_all_tbase
  )
  
  
  # Apply fixed acceptance thresholds --------------------------
  
  pair_consensus <- apply_pair_acceptance(
    consensus =
      pair_consensus,
    
    error_upper_fence =
      parameters$error_upper_fence,
    
    stability_lower_fence =
      parameters$stability_lower_fence
  )
  
  
  # Prepare accepted generation pairs ---------------------------
  
  accepted_pairs <- candidate_pairs %>%
    mutate(
      pair_id = paste(
        peak_1,
        peak_2,
        sep = "-"
      )
    ) %>%
    inner_join(
      pair_consensus %>%
        filter(
          accepted_pair
        ) %>%
        select(
          Year_plus_Site,
          modal_pair,
          modal_pair_fraction,
          median_error,
          median_pair_quality
        ),
      by = "Year_plus_Site"
    ) %>%
    filter(
      pair_id == modal_pair
    )
  
  
  # Calculate mean temperature and predicted generation duration -
  
  accepted_pairs <- accepted_pairs %>%
    rowwise() %>%
    mutate(
      
      mean_temperature = mean(
        meteo_data %>%
          filter(
            Year_plus_Site == .env$Year_plus_Site,
            Date > .env$peak_1_date,
            Date <= .env$peak_2_date
          ) %>%
          pull(Taverage),
        na.rm = TRUE
      ),
      
      predicted_development_rate =
        parameters$generation_model_parameters$intercept +
        parameters$generation_model_parameters$slope *
        mean_temperature,
      
      predicted_generation_days =
        1 / predicted_development_rate
    ) %>%
    ungroup()
  
  # Return results --------------------------------------------
  
  list(
    candidate_peaks =
      candidate_peaks,
    
    candidate_pairs =
      candidate_pairs,
    
    degree_days =
      dd_grid,
    
    selected_pairs =
      selected_all_tbase,
    
    pair_consensus =
      pair_consensus,
  
    accepted_pairs =
      accepted_pairs
    )
}

