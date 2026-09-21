# 11_refit_final_model.R
# Refit final generation model after independent validation


# Packages ----------------------------------------------------

library(dplyr)


# Load accepted intervals ------------------------------------

accepted_train <- readRDS(
  "data/processed/accepted_generation_pairs_train.rds"
)

accepted_test <- readRDS(
  "data/processed/accepted_generation_pairs_test.rds"
)

meteo_analysis <- readRDS(
  "data/processed/meteo_analysis.rds"
)


# Combine accepted train and test intervals ---------------------------------

final_generation_pairs <- bind_rows(
  accepted_train %>%
    mutate(
      dataset = "train"
    ),
  
  accepted_test %>%
    mutate(
      dataset = "test"
    )
)


# Prepare final model data --------------------------------------------

final_model_data <- final_generation_pairs %>%
  rowwise() %>%
  mutate(
    mean_temperature = mean(
      meteo_analysis %>%
        filter(
          Year_plus_Site == .env$Year_plus_Site,
          Date > .env$peak_1_date,
          Date <= .env$peak_2_date
        ) %>%
        pull(Taverage),
      na.rm = TRUE
    ),
    
    development_rate =
      1 / generation_days
  ) %>%
  ungroup()


# Fit final model ---------------------------------------------------

final_generation_lm <- lm(
  development_rate ~ mean_temperature,
  data = final_model_data
)


final_intercept <-
  coef(final_generation_lm)[1]

final_slope <-
  coef(final_generation_lm)[2]

final_Tbase <-
  -final_intercept / final_slope

final_K <-
  1 / final_slope


# Bootstrap uncertainty for final parameters -----------------------------

set.seed(2026)

n_boot <- 2000

final_series <- unique(
  final_model_data$Year_plus_Site
)


boot_results <- lapply(
  seq_len(n_boot),
  function(i) {
    
    sampled_series <- sample(
      final_series,
      size = length(final_series),
      replace = TRUE
    )
    
    boot_data <- lapply(
      sampled_series,
      function(s) {
        
        final_model_data %>%
          filter(
            Year_plus_Site == s
          )
      }
    ) %>%
      bind_rows()
    
    boot_model <- lm(
      development_rate ~ mean_temperature,
      data = boot_data
    )
    
    boot_intercept <-
      coef(boot_model)[1]
    
    boot_slope <-
      coef(boot_model)[2]
    
    tibble(
      Tbase =
        -boot_intercept / boot_slope,
      
      K =
        1 / boot_slope
    )
  }
) %>%
  bind_rows()



# Final model parameters ---------------------------------------------

final_model_parameters <- tibble(
  
  intercept =
    as.numeric(final_intercept),
  
  slope =
    as.numeric(final_slope),
  
  Tbase =
    as.numeric(final_Tbase),
  
  K =
    as.numeric(final_K),
  
  R_squared =
    summary(final_generation_lm)$r.squared,
  
  Tbase_boot_median =
    median(
      boot_results$Tbase
    ),
  
  Tbase_boot_lower =
    quantile(
      boot_results$Tbase,
      0.025
    ),
  
  Tbase_boot_upper =
    quantile(
      boot_results$Tbase,
      0.975
    ),
  
  K_boot_median =
    median(
      boot_results$K
    ),
  
  K_boot_lower =
    quantile(
      boot_results$K,
      0.025
    ),
  
  K_boot_upper =
    quantile(
      boot_results$K,
      0.975
    )
)


# Save ---------------------------------------------------------------------

saveRDS(
  final_generation_pairs,
  "data/processed/final_generation_pairs.rds"
)

saveRDS(
  final_model_data,
  "data/processed/final_generation_model_data.rds"
)

saveRDS(
  final_generation_lm,
  "data/processed/final_generation_model.rds"
)

saveRDS(
  final_model_parameters,
  "data/processed/final_generation_model_parameters.rds"
)

saveRDS(
  boot_results,
  "data/processed/final_generation_model_bootstrap.rds"
)


if (file.exists(
  "data/processed/final_generation_pairs.rds"
)) {
  cat(
    'Fails "data/processed/final_generation_pairs.rds" ir izveidots.\n'
  )
}

if (file.exists(
  "data/processed/final_generation_model_data.rds"
)) {
  cat(
    'Fails "data/processed/final_generation_model_data.rds" ir izveidots.\n'
  )
}

if (file.exists(
  "data/processed/final_generation_model.rds"
)) {
  cat(
    'Fails "data/processed/final_generation_model.rds" ir izveidots.\n'
  )
}

if (file.exists(
  "data/processed/final_generation_model_parameters.rds"
)) {
  cat(
    'Fails "data/processed/final_generation_model_parameters.rds" ir izveidots.\n'
  )
}

if (file.exists(
  "data/processed/final_generation_model_bootstrap.rds"
)) {
  cat(
    'Fails "data/processed/final_generation_model_bootstrap.rds" ir izveidots.\n'
  )
}