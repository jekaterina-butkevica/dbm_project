# ============================================================
# 11_prepare_final_results.R
# Prepare final tables and figures
# ============================================================

library(dplyr)
library(ggplot2)

# Load final training model data -----------------------------

generation_model_data <- readRDS(
  "data/processed/generation_model_data_train.rds"
)

generation_model <- readRDS(
  "data/processed/generation_model_train.rds"
)

generation_model_parameters <- readRDS(
  "data/processed/generation_model_parameters.rds"
)


# ============================================================
# Figure 1. Development rate vs mean temperature
# ============================================================

model_r2 <- summary(
  generation_model
)$r.squared


figure_development_rate <- ggplot(
  generation_model_data,
  aes(
    x = mean_temperature,
    y = development_rate
  )
) +
  geom_point(
    size = 2.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    x = "Mean temperature (°C)",
    y = "Development rate (1/day)",
    
    title =
      "Temperature-dependent development rate of Plutella xylostella",
    
    subtitle = paste0(
      "Tbase = ",
      round(
        generation_model_parameters$Tbase,
        2
      ),
      " °C; K = ",
      round(
        generation_model_parameters$K,
        1
      ),
      " DD; R² = ",
      round(
        model_r2,
        3
      )
    ),
    
    caption = paste0(
      "Tbase 95% bootstrap interval: ",
      round(
        generation_model_parameters$
          Tbase_bootstrap_95["lower"],
        2
      ),
      "–",
      round(
        generation_model_parameters$
          Tbase_bootstrap_95["upper"],
        2
      ),
      " °C. Tbase is extrapolated below the observed ",
      "mean-temperature range."
    )
  ) +
  theme_classic()

figure_development_rate





# ============================================================
# Figure 2. Generation duration vs mean temperature
# ============================================================

temperature_grid <- tibble(
  mean_temperature = seq(
    min(generation_model_data$mean_temperature),
    max(generation_model_data$mean_temperature),
    length.out = 200
  )
)

temperature_grid <- temperature_grid %>%
  mutate(
    predicted_rate = predict(
      generation_model,
      newdata = temperature_grid
    ),
    
    predicted_days =
      1 / predicted_rate
  )


figure_generation_duration <- ggplot() +
  geom_point(
    data = generation_model_data,
    aes(
      x = mean_temperature,
      y = generation_days
    ),
    size = 2.5
  ) +
  geom_line(
    data = temperature_grid,
    aes(
      x = mean_temperature,
      y = predicted_days
    ),
    linewidth = 1
  ) +
  labs(
    x = "Mean temperature (°C)",
    y = "Generation duration (days)",
    title = "Temperature-dependent generation duration of Plutella xylostella",
    subtitle = paste0(
      "Tbase = ",
      round(generation_model_parameters$Tbase, 2),
      " °C; K = ",
      round(generation_model_parameters$K, 1),
      " DD"
    )
  ) +
  theme_classic()

figure_generation_duration
