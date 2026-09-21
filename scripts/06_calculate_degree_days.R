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