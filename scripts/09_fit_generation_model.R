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

