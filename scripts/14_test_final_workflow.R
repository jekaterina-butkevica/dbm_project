# 14_test_final_workflow.R
# Validate reusable DBM workflow against held-out test series


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

train_test_split <- readRDS(
  "data/processed/train_test_split.rds"
)

historical_test_pairs <- readRDS(
  "data/processed/accepted_generation_pairs_test.rds"
)


# Run reusable workflow ---------------------------------------

workflow_result <- analyse_dbm_series(
  moth_data = moth_analysis,
  meteo_data = meteo_analysis,
  parameters = final_algorithm_parameters
)


# Restrict comparison to held-out test series ----------------

test_series <- train_test_split %>%
  filter(
    split == "test"
  ) %>%
  pull(
    Year_plus_Site
  )


wrapper_test_pairs <- workflow_result$accepted_pairs %>%
  filter(
    Year_plus_Site %in% test_series
  ) %>%
  select(
    Year_plus_Site,
    peak_1,
    peak_2
  ) %>%
  distinct()


historical_test_pairs <- historical_test_pairs %>%
  select(
    Year_plus_Site,
    peak_1,
    peak_2
  ) %>%
  distinct()


# Compare results ---------------------------------------------

test_pair_comparison <- full_join(
  wrapper_test_pairs %>%
    mutate(
      wrapper = TRUE
    ),
  
  historical_test_pairs %>%
    mutate(
      historical = TRUE
    ),
  
  by = c(
    "Year_plus_Site",
    "peak_1",
    "peak_2"
  )
) %>%
  mutate(
    wrapper = if_else(
      is.na(wrapper),
      FALSE,
      wrapper
    ),
    
    historical = if_else(
      is.na(historical),
      FALSE,
      historical
    ),
    
    same_result =
      wrapper &
      historical
  )


# Validation check --------------------------------------------

if (!all(test_pair_comparison$same_result)) {
  stop(
    "Reusable workflow does not reproduce held-out test-pair selection."
  )
}


cat(
  "Reusable workflow reproduces all accepted held-out test pairs.\n"
)

