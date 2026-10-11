# 09_fit_generation_model.R
# Fit generation duration-temperature model on training data


# Packages ----------------------------------------------------

library(dplyr)


# Load data ---------------------------------------------------

accepted_generation_pairs_train <- readRDS(
  "data/processed/accepted_generation_pairs_train.rds"
)

meteo_analysis <- readRDS(
  "data/processed/meteo_analysis.rds"
)

source("R/temperature_functions.R")

# Prepare generation-model data -------------------------------


generation_model_data <- accepted_generation_pairs_train %>%
  rowwise() %>%
  mutate(
    mean_temperature =
      calculate_interval_mean_temperature(
        series_id = Year_plus_Site,
        start_time = peak_1_time,
        end_time = peak_2_time,
        meteo_data = meteo_analysis
      ),
    
    development_rate =
      1 / generation_days
  ) %>%
  ungroup()


# Fit linear development-rate model ---------------------------

generation_lm <- lm(
  development_rate ~ mean_temperature,
  data = generation_model_data
)


# Estimate thermal parameters ---------------------------------

intercept <- coef(generation_lm)[1]
slope <- coef(generation_lm)[2]

Tbase_est <- -intercept / slope
K_est <- 1 / slope


generation_model_parameters <- list(
  
  intercept = as.numeric(
    intercept
  ),
  
  slope = as.numeric(
    slope
  ),
  
  Tbase = as.numeric(
    Tbase_est
  ),
  
  K = as.numeric(
    K_est
  )
)


# Save --------------------------------------------------------

saveRDS(
  generation_lm,
  "data/processed/generation_model_train.rds"
)

saveRDS(
  generation_model_parameters,
  "data/processed/generation_model_parameters.rds"
)

saveRDS(
  generation_model_data,
  "data/processed/generation_model_data_train.rds"
)


if (file.exists(
  "data/processed/generation_model_train.rds"
)) {
  cat(
    'Fails "data/processed/generation_model_train.rds" ir izveidots.\n'
  )
}

if (file.exists(
  "data/processed/generation_model_parameters.rds"
)) {
  cat(
    'Fails "data/processed/generation_model_parameters.rds" ir izveidots.\n'
  )
}

if (file.exists(
  "data/processed/generation_model_data_train.rds"
)) {
  cat(
    'Fails "data/processed/generation_model_data_train.rds" ir izveidots.\n'
  )
}

