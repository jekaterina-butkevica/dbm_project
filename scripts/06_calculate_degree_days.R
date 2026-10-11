# 06_calculate_degree_days.R
# Calculate degree-days across candidate Tbase values
# for algorithm calibration and sensitivity analysis


# Packages ----------------------------------------------------

library(dplyr)
library(tidyr)


# Load data ---------------------------------------------------

candidate_generation_pairs <- readRDS(
  "data/processed/candidate_generation_pairs.rds"
)

meteo_analysis <- readRDS(
  "data/processed/meteo_analysis.rds"
)


# Load functions ----------------------------------------------

source("R/temperature_functions.R")


# Base-temperature scenarios ---------------------------------

Tbase_values <- 0:12


# Calculate degree-days for all candidate pairs ---------------

dd_grid <- lapply(
  Tbase_values,
  function(tb) {
    
    dd_results <- lapply(
      seq_len(nrow(candidate_generation_pairs)),
      function(i) {
        
        calculate_pair_degree_days(
          pair_row = candidate_generation_pairs[i, ],
          meteo_data = meteo_analysis,
          Tbase = tb
        )
      }
    ) %>%
      bind_rows()
    
    bind_cols(
      candidate_generation_pairs,
      dd_results
    )
  }
) %>%
  bind_rows()


# Check for missing temperature data --------------------------

if (any(dd_grid$n_missing_temp > 0)) {
  stop(
    "Degree-day calculation contains missing temperature data."
  )
}

if (any(
  abs(
    dd_grid$temperature_coverage_days -
    dd_grid$expected_interval_days
  ) > 1e-8
)) {
  
  stop(
    "Degree-day calculation does not fully cover one or more peak intervals."
  )
}

# Save --------------------------------------------------------

saveRDS(
  dd_grid,
  "data/processed/candidate_pairs_degree_days.rds"
)


if (file.exists(
  "data/processed/candidate_pairs_degree_days.rds"
)) {
  cat(
    'Fails "data/processed/candidate_pairs_degree_days.rds" ir izveidots.\n'
  )
}
