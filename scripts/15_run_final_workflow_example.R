# 15_run_final_workflow_example.R
# Example: run the final reusable DBM workflow on new data


# Packages ----------------------------------------------------

library(dplyr)
library(tidyr)


# Load reusable functions ------------------------------------

source("R/input_validation_functions.R")
source("R/peak_functions.R")
source("R/pairing_functions.R")
source("R/temperature_functions.R")
source("R/pair_selection_functions.R")
source("R/analysis_functions.R")


# Load final algorithm parameters -----------------------------

final_algorithm_parameters <- readRDS(
  "data/processed/final_algorithm_parameters.rds"
)


# Load new input data -----------------------------------------

# Replace these example paths with the actual new datasets.

new_moth_data <- readRDS(
  "data/new/new_moth_analysis.rds"
)

new_meteo_data <- readRDS(
  "data/new/new_meteo_analysis.rds"
)


# Run final workflow ------------------------------------------

result <- analyse_dbm_series(
  moth_data = new_moth_data,
  meteo_data = new_meteo_data,
  parameters = final_algorithm_parameters
)


# Main outputs ------------------------------------------------

result$pair_consensus

result$accepted_pairs

