# 09_fit_generation_model.R
# Fit generation duration-temperature relationship



# Pakotnes ----
library(dplyr)



#data ----

accepted_generation_pairs_train <- readRDS(
  "data/processed/accepted_generation_pairs_train.rds"
)

meteo_analysis <- readRDS(
  "data/processed/meteo_analysis.rds"
)




generation_model_data <- accepted_generation_pairs_train %>%
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


generation_model_data %>%
  summarise(
    n_pairs = n(),
    n_series = n_distinct(Year_plus_Site),
    min_days = min(generation_days),
    max_days = max(generation_days),
    min_temperature = min(mean_temperature),
    max_temperature = max(mean_temperature)
  )

plot(
  generation_model_data$mean_temperature,
  generation_model_data$generation_days,
  xlab = "Mean temperature (°C)",
  ylab = "Generation duration (days)",
  main = "Accepted generation pairs: training data"
)


plot(
  generation_model_data$mean_temperature,
  generation_model_data$development_rate,
  xlab = "Mean temperature (°C)",
  ylab = "Development rate (1/day)",
  main = "Development rate vs temperature"
)

generation_lm <- lm(
  development_rate ~ mean_temperature,
  data = generation_model_data
)

summary(generation_lm)

coef(generation_lm)


intercept <- coef(generation_lm)[1]
slope <- coef(generation_lm)[2]

Tbase_est <- -intercept / slope
K_est <- 1 / slope

Tbase_est
K_est



# Check whether the temperature-development relationship
# shows evidence of curvature

generation_lm_quadratic <- lm(
  development_rate ~
    mean_temperature +
    I(mean_temperature^2),
  data = generation_model_data
)

summary(generation_lm_quadratic)

anova(
  generation_lm,
  generation_lm_quadratic
)

AIC(
  generation_lm,
  generation_lm_quadratic
)


plot(
  generation_model_data$mean_temperature,
  generation_model_data$development_rate,
  xlab = "Mean temperature (°C)",
  ylab = "Development rate (1/day)",
  main = "Development rate vs temperature"
)

abline(
  generation_lm,
  lwd = 2
)

# Mūsu novērotajā temperatūru diapazonā un ar pašreizējiem datiem nav 
# konstatējama nelinearitāte, kas attaisnotu sarežģītāku modeli.


# Bootstrap uncertainty for Tbase and K ----

set.seed(2026)

n_boot <- 2000

boot_results <- lapply(
  seq_len(n_boot),
  function(i) {
    
    sampled_series <- sample(
      generation_model_data$Year_plus_Site,
      size = nrow(generation_model_data),
      replace = TRUE
    )
    
    boot_data <- lapply(
      sampled_series,
      function(s) {
        
        generation_model_data %>%
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
    
    a <- coef(boot_model)[1]
    b <- coef(boot_model)[2]
    
    tibble(
      intercept = a,
      slope = b,
      Tbase = -a / b,
      K = 1 / b
    )
  }
) %>%
  bind_rows()


summary(
  boot_results$Tbase
)

summary(
  boot_results$K
)


boot_ci <- boot_results %>%
  summarise(
    Tbase_lower =
      quantile(Tbase, 0.025),
    
    Tbase_median =
      median(Tbase),
    
    Tbase_upper =
      quantile(Tbase, 0.975),
    
    K_lower =
      quantile(K, 0.025),
    
    K_median =
      median(K),
    
    K_upper =
      quantile(K, 0.975)
  )

boot_ci


sum(
  boot_results$slope <= 0
)


#Temperatūras efekts ir stabils, jo nevienā no 2000 bootstrap paraugiem slīpums 
#nebija negatīvs, bet precīza Tbase vērtība ir samērā nenoteikta. Tas saskan ar 
#to, ka paaudžu intervāli galvenokārt atrodas daudz siltākā temperatūras diapazonā.



# Leave-one-series-out validation of generation model

generation_loocv <- lapply(
  generation_model_data$Year_plus_Site,
  function(validation_series) {
    
    fold_train <- generation_model_data %>%
      filter(
        Year_plus_Site != validation_series
      )
    
    fold_validation <- generation_model_data %>%
      filter(
        Year_plus_Site == validation_series
      )
    
    fold_model <- lm(
      development_rate ~ mean_temperature,
      data = fold_train
    )
    
    predicted_rate <- predict(
      fold_model,
      newdata = fold_validation
    )
    
    predicted_days <-
      1 / predicted_rate
    
    fold_validation %>%
      transmute(
        Year_plus_Site,
        observed_days = generation_days,
        predicted_days =
          as.numeric(predicted_days),
        
        error_days =
          predicted_days - observed_days,
        
        absolute_error_days =
          abs(error_days),
        
        relative_error =
          absolute_error_days /
          observed_days
      )
  }
) %>%
  bind_rows()





generation_loocv %>%
  summarise(
    n_series = n(),
    
    MAE_days =
      mean(absolute_error_days),
    
    median_AE_days =
      median(absolute_error_days),
    
    RMSE_days =
      sqrt(
        mean(error_days^2)
      ),
    
    median_relative_error =
      median(relative_error),
    
    mean_relative_error =
      mean(relative_error)
  )

generation_loocv %>%
  arrange(
    desc(absolute_error_days)
  ) %>%
  print(n = Inf)


generation_model_parameters <- list(
  intercept = as.numeric(
    coef(generation_lm)[1]
  ),
  
  slope = as.numeric(
    coef(generation_lm)[2]
  ),
  
  Tbase = as.numeric(
    Tbase_est
  ),
  
  K = as.numeric(
    K_est
  ),
  
  Tbase_bootstrap_95 = c(
    lower = as.numeric(
      boot_ci$Tbase_lower
    ),
    upper = as.numeric(
      boot_ci$Tbase_upper
    )
  ),
  
  K_bootstrap_95 = c(
    lower = as.numeric(
      boot_ci$K_lower
    ),
    upper = as.numeric(
      boot_ci$K_upper
    )
  )
)


# ============================================================
# Sensitivity analysis: peak-only selected pairs
# Same 22 series as in the main model
# ============================================================

main_model_series <-
  accepted_generation_pairs_train %>%
  pull(Year_plus_Site)


baseline_sensitivity_data <-
  pair_candidates_train %>%
  filter(
    Tbase == 0,
    Year_plus_Site %in% main_model_series
  ) %>%
  semi_join(
    baseline_selected_pairs,
    by = c(
      "Year_plus_Site",
      "peak_1",
      "peak_2"
    )
  ) %>%
  mutate(
    mean_temperature =
      degree_days / n_temp_days,
    
    development_rate =
      1 / generation_days
  )


baseline_sensitivity_lm <- lm(
  development_rate ~ mean_temperature,
  data = baseline_sensitivity_data
)


summary(
  baseline_sensitivity_lm
)


baseline_intercept <-
  coef(baseline_sensitivity_lm)[1]

baseline_slope <-
  coef(baseline_sensitivity_lm)[2]


baseline_Tbase <-
  -baseline_intercept / baseline_slope

baseline_K <-
  1 / baseline_slope


baseline_Tbase
baseline_K






# ============================================================
# Leave-one-series-out stability of Tbase and K
# ============================================================

parameter_loocv <- lapply(
  unique(generation_model_data$Year_plus_Site),
  function(validation_series) {
    
    fold_train <- generation_model_data %>%
      filter(
        Year_plus_Site != validation_series
      )
    
    fold_model <- lm(
      development_rate ~ mean_temperature,
      data = fold_train
    )
    
    a <- coef(fold_model)[1]
    b <- coef(fold_model)[2]
    
    tibble(
      omitted_series = validation_series,
      intercept = a,
      slope = b,
      Tbase = -a / b,
      K = 1 / b
    )
  }
) %>%
  bind_rows()


parameter_loocv %>%
  summarise(
    Tbase_min = min(Tbase),
    Tbase_median = median(Tbase),
    Tbase_max = max(Tbase),
    
    K_min = min(K),
    K_median = median(K),
    K_max = max(K)
  )


parameter_loocv %>%
  arrange(Tbase) %>%
  print(n = Inf)



# ============================================================
# Temperature range of accepted generation intervals
# ============================================================

generation_temperature_diagnostics <-
  generation_model_data %>%
  rowwise() %>%
  mutate(
    min_daily_temperature = min(
      meteo_analysis %>%
        filter(
          Year_plus_Site == .env$Year_plus_Site,
          Date > .env$peak_1_date,
          Date <= .env$peak_2_date
        ) %>%
        pull(Taverage),
      na.rm = TRUE
    )
  ) %>%
  ungroup()


generation_temperature_diagnostics %>%
  summarise(
    min_mean_temperature = min(mean_temperature),
    median_mean_temperature = median(mean_temperature),
    max_mean_temperature = max(mean_temperature),
    
    min_daily_temperature =
      min(min_daily_temperature),
    
    n_below_6 =
      sum(min_daily_temperature <= 6),
    
    n_below_8 =
      sum(min_daily_temperature <= 8),
    
    n_below_10 =
      sum(min_daily_temperature <= 10),
    
    n_below_12 =
      sum(min_daily_temperature <= 12)
  )



generation_temperature_diagnostics %>%
  select(
    Year_plus_Site,
    generation_days,
    mean_temperature,
    min_daily_temperature
  ) %>%
  arrange(min_daily_temperature) %>%
  print(n = Inf)











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

