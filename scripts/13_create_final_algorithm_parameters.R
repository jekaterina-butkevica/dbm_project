# 13_create_final_algorithm_parameters.R
# Combine all fixed parameters required by the reusable DBM workflow


# Load training-derived pair-selection parameters ------------

pair_selection_parameters <- readRDS(
  "data/processed/pair_selection_parameters.rds"
)


# Load final generation-model parameters ---------------------

generation_model_parameters <- readRDS(
  "data/processed/final_generation_model_parameters.rds"
)


# Create final algorithm parameter object --------------------

final_algorithm_parameters <- list(
  
  generation_min_days = 18,
  
  generation_max_days = 50,
  
  Tbase_values =
    pair_selection_parameters$Tbase_values,
  
  K_profile =
    pair_selection_parameters$K_profile,
  
  error_upper_fence =
    pair_selection_parameters$error_upper_fence,
  
  stability_lower_fence =
    pair_selection_parameters$stability_lower_fence,
  
  generation_model_parameters =
    generation_model_parameters
)


# Save --------------------------------------------------------

saveRDS(
  final_algorithm_parameters,
  "data/processed/final_algorithm_parameters.rds"
)


if (file.exists(
  "data/processed/final_algorithm_parameters.rds"
)) {
  cat(
    'Fails "data/processed/final_algorithm_parameters.rds" ir izveidots.\n'
  )
}





# 14_test_final_workflow.R
# Test reusable DBM workflow against historical pipeline


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


# Load data ---------------------------------------------------

moth_analysis <- readRDS(
  "data/processed/moth_analysis.rds"
)

meteo_analysis <- readRDS(
  "data/processed/meteo_analysis.rds"
)

final_algorithm_parameters <- readRDS(
  "data/processed/final_algorithm_parameters.rds"
)


# Run reusable workflow ---------------------------------------

workflow_result <- analyse_dbm_series(
  moth_data = moth_analysis,
  meteo_data = meteo_analysis,
  parameters = final_algorithm_parameters
)