# ============================================================
# 11_refit_final_model.R
# Refit final generation model after independent validation
# ============================================================

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


# ============================================================
# Combine accepted train and test intervals
# ============================================================

final_generation_pairs <- bind_rows(
  accepted_train %>%
    mutate(dataset = "train"),
  
  accepted_test %>%
    mutate(dataset = "test")
)


final_generation_pairs %>%
  count(dataset)

final_generation_pairs %>%
  summarise(
    n_pairs = n(),
    n_series = n_distinct(Year_plus_Site)
  )





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


final_generation_lm <- lm(
  development_rate ~ mean_temperature,
  data = final_model_data
)

summary(final_generation_lm)

coef(final_generation_lm)



final_intercept <-
  coef(final_generation_lm)[1]

final_slope <-
  coef(final_generation_lm)[2]

final_Tbase <-
  -final_intercept / final_slope

final_K <-
  1 / final_slope

final_Tbase
final_K




# ============================================================
# Bootstrap uncertainty for final parameters
# ============================================================

set.seed(2026)

n_boot <- 2000

boot_results <- vector(
  "list",
  n_boot
)

for (i in seq_len(n_boot)) {
  
  sampled_series <- sample(
    final_model_data$Year_plus_Site,
    size = nrow(final_model_data),
    replace = TRUE
  )
  
  boot_data <- final_model_data %>%
    slice(
      match(
        sampled_series,
        Year_plus_Site
      )
    )
  
  boot_model <- lm(
    development_rate ~ mean_temperature,
    data = boot_data
  )
  
  boot_intercept <- coef(boot_model)[1]
  boot_slope <- coef(boot_model)[2]
  
  boot_results[[i]] <- tibble(
    Tbase = -boot_intercept / boot_slope,
    K = 1 / boot_slope
  )
}

boot_results <- bind_rows(
  boot_results
)


boot_results %>%
  summarise(
    Tbase_median = median(Tbase),
    Tbase_lower = quantile(Tbase, 0.025),
    Tbase_upper = quantile(Tbase, 0.975),
    
    K_median = median(K),
    K_lower = quantile(K, 0.025),
    K_upper = quantile(K, 0.975)
  )


sum(
  boot_results$K < 0
)

sum(
  boot_results$Tbase < 0
)


# ============================================================
# Save final model results
# ============================================================

final_model_parameters <- tibble(
  Tbase = final_Tbase,
  K = final_K,
  R_squared = summary(final_generation_lm)$r.squared,
  
  Tbase_boot_median = median(boot_results$Tbase),
  Tbase_boot_lower = quantile(boot_results$Tbase, 0.025),
  Tbase_boot_upper = quantile(boot_results$Tbase, 0.975),
  
  K_boot_median = median(boot_results$K),
  K_boot_lower = quantile(boot_results$K, 0.025),
  K_boot_upper = quantile(boot_results$K, 0.975)
)


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

final_model_parameters
