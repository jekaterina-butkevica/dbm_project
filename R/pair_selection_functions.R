# pair_selection_functions.R
# Functions for DBM generation-pair selection


# Pair quality ----------------------------------------------------

add_pair_quality <- function(data) {
  
  data %>%
    mutate(
      edge_score = pmin(
        pair_edge_support / 5,
        1
      ),
      
      pair_quality_score = (
        pair_min_prominence +
          pair_min_relative_to_max +
          edge_score
      ) / 3
    ) %>%
    group_by(
      Tbase,
      Year_plus_Site
    ) %>%
    mutate(
      quality_pair_weight =
        pair_quality_score /
        sum(pair_quality_score)
    ) %>%
    ungroup()
}


# Weighted median ------------------------------------------------

weighted_median <- function(x, w) {
  
  keep <-
    !is.na(x) &
    !is.na(w) &
    w > 0
  
  x <- x[keep]
  w <- w[keep]
  
  ord <- order(x)
  
  x <- x[ord]
  w <- w[ord]
  
  cumulative_weight <-
    cumsum(w) / sum(w)
  
  x[
    which(
      cumulative_weight >= 0.5
    )[1]
  ]
}


# Iterative estimation of thermal requirement K -----------------------------

estimate_K_iterative <- function(
    data,
    max_iter = 50,
    tolerance = 0.01
) {
  
  K_current <- weighted_median(
    x = data$degree_days,
    w = data$quality_pair_weight
  )
  
  for (i in seq_len(max_iter)) {
    
    selected <- data %>%
      mutate(
        thermal_relative_error =
          abs(
            degree_days - K_current
          ) /
          K_current
      ) %>%
      arrange(
        Year_plus_Site,
        thermal_relative_error,
        desc(pair_quality_score),
        peak_1,
        peak_2
      ) %>%
      group_by(
        Year_plus_Site
      ) %>%
      slice(1) %>%
      ungroup()
    
    K_new <- median(
      selected$degree_days
    )
    
    if (
      abs(
        K_new - K_current
      ) < tolerance
    ) {
      break
    }
    
    K_current <- K_new
  }
  
  list(
    K = K_new,
    selected_pairs = selected,
    iterations = i
  )
}


# Select best pair for each Tbase using fixed K profile ---------------------

select_pairs_from_K_profile <- function(
    pair_candidates,
    K_profile
) {
  
  pair_candidates %>%
    left_join(
      K_profile,
      by = "Tbase"
    ) %>%
    mutate(
      thermal_relative_error =
        abs(
          degree_days - K
        ) /
        K
    ) %>%
    arrange(
      Tbase,
      Year_plus_Site,
      thermal_relative_error,
      desc(pair_quality_score),
      peak_1,
      peak_2
    ) %>%
    group_by(
      Tbase,
      Year_plus_Site
    ) %>%
    slice(1) %>%
    ungroup()
}


# Consensus pair across Tbase scenarios --------------------------------

summarise_pair_consensus <- function(
    selected_pairs
) {
  
  n_tbase_scenarios <-
    n_distinct(
      selected_pairs$Tbase
    )
  
  selected_pairs %>%
    mutate(
      pair_id = paste(
        peak_1,
        peak_2,
        sep = "-"
      )
    ) %>%
    group_by(
      Year_plus_Site
    ) %>%
    mutate(
      modal_pair = names(
        sort(
          table(pair_id),
          decreasing = TRUE
        )
      )[1]
    ) %>%
    filter(
      pair_id == modal_pair
    ) %>%
    summarise(
      modal_pair =
        first(modal_pair),
      
      modal_pair_count =
        n(),
      
      modal_pair_fraction =
        modal_pair_count /
        n_tbase_scenarios,
      
      median_error =
        median(
          thermal_relative_error
        ),
      
      max_error =
        max(
          thermal_relative_error
        ),
      
      median_generation_days =
        median(
          generation_days
        ),
      
      median_pair_quality =
        median(
          pair_quality_score
        ),
      
      .groups = "drop"
    )
}


# Apply fixed acceptance thresholds ------------------------------------

apply_pair_acceptance <- function(
    consensus,
    error_upper_fence,
    stability_lower_fence
) {
  
  consensus %>%
    mutate(
      reliable_pair =
        median_error <=
        error_upper_fence,
      
      stable_pair =
        modal_pair_fraction >=
        stability_lower_fence,
      
      accepted_pair =
        reliable_pair &
        stable_pair
    )
}
