# ============================================================
# 03_fit_flight_curves.R
# Fit DBM flight activity curves
# ============================================================

# Pakotnes ----

library(dplyr)
library(ggplot2)
library(mgcv)


# Load data ---------------------------------------------------

moth_analysis <- readRDS(
  "data/processed/moth_analysis.rds"
)


# Load functions ----------------------------------------------

source("R/smoothing_functions.R")
<<<<<<< HEAD


# Select one test series --------------------------------------

test_series <- moth_analysis %>%
  filter(
    Year_plus_Site == "2021Dignajas"
  )
test_series




# Fit the model
test_gam <- fit_gam_flight_curve(
  data = test_series,
  k = 6
)

summary(test_gam)

gam.check(test_gam)





test_prediction <- predict_gam_flight_curve(
  model = test_gam,
  data = test_series
)

head(test_prediction)

ggplot() +
  geom_point(
    data = test_series,
    aes(
      x = as.Date(mid_time, origin = "1970-01-01"),
      y = total_count / trap_days
    )
  ) +
  geom_line(
    data = test_prediction,
    aes(
      x = Date,
      y = estimated_activity
    ),
    linewidth = 1
  ) +
  labs(
    title = "2021Dignajas",
    x = "Date",
    y = "Estimated moth catch per trap-day"
  ) +
  theme_minimal()




gam_k6 <- fit_gam_flight_curve(
  data = test_series,
  k = 6
)

gam_k8 <- fit_gam_flight_curve(
  data = test_series,
  k = 8
)

gam_k10 <- fit_gam_flight_curve(
  data = test_series,
  k = 10
)

gam_k12 <- fit_gam_flight_curve(
  data = test_series,
  k = 12
)


pred_k6 <- predict_gam_flight_curve(
  gam_k6,
  test_series
) %>%
  mutate(k = "k = 6")

pred_k8 <- predict_gam_flight_curve(
  gam_k8,
  test_series
) %>%
  mutate(k = "k = 8")

pred_k10 <- predict_gam_flight_curve(
  gam_k10,
  test_series
) %>%
  mutate(k = "k = 10")

pred_k12 <- predict_gam_flight_curve(
  gam_k12,
  test_series
) %>%
  mutate(k = "k = 12")



pred_compare <- bind_rows(
  pred_k6,
  pred_k8,
  pred_k10,
  pred_k12
)


ggplot() +
  geom_point(
    data = test_series,
    aes(
      x = as.Date(mid_time, origin = "1970-01-01"),
      y = total_count / trap_days
    )
  ) +
  geom_line(
    data = pred_compare,
    aes(
      x = Date,
      y = estimated_activity
    ),
    linewidth = 1
  ) +
  facet_wrap(~ k, ncol = 2) +
  labs(
    title = "2021Dignajas: comparison of GAM smoothness",
    x = "Date",
    y = "Estimated moth catch per trap-day"
  ) +
  theme_minimal()



# LOESS
loess_03 <- fit_loess_flight_curve(
  test_series,
  span = 0.3
)

loess_04 <- fit_loess_flight_curve(
  test_series,
  span = 0.4
)

loess_05 <- fit_loess_flight_curve(
  test_series,
  span = 0.5
)


#prognoze
pred_loess_03 <- predict_loess_flight_curve(
  loess_03,
  test_series
) %>%
  mutate(span = "span = 0.3")

pred_loess_04 <- predict_loess_flight_curve(
  loess_04,
  test_series
) %>%
  mutate(span = "span = 0.4")

pred_loess_05 <- predict_loess_flight_curve(
  loess_05,
  test_series
) %>%
  mutate(span = "span = 0.5")

pred_loess_compare <- bind_rows(
  pred_loess_03,
  pred_loess_04,
  pred_loess_05
)


ggplot() +
  geom_point(
    data = test_series,
    aes(
      x = as.Date(mid_time, origin = "1970-01-01"),
      y = total_count / trap_days
    )
  ) +
  geom_line(
    data = pred_loess_compare,
    aes(
      x = Date,
      y = estimated_activity
    ),
    linewidth = 1
  ) +
  facet_wrap(~ span, ncol = 1) +
  labs(
    title = "2021Dignajas: LOESS comparison",
    x = "Date",
    y = "Moth catch per trap-day"
  ) +
  theme_minimal()


# Testēju mazākas vērtības uz vairākām paraugkopam

test_series_ids <- c(
  "2021Dignajas"
  # te vēlāk pievienosim:
  # "XXXX...",
  # "XXXX...",
  # "XXXX..."
)

span_values <- c(
  0.20,
  0.25,
  0.30,
  0.35,
  0.40
)

loess_comparison <- bind_rows(
  lapply(test_series_ids, function(series_id) {
    
    series_data <- moth_analysis %>%
      filter(Year_plus_Site == series_id)
    
    bind_rows(
      lapply(span_values, function(span_value) {
        
        model <- fit_loess_flight_curve(
          data = series_data,
          span = span_value
        )
        
        prediction <- predict_loess_flight_curve(
          model = model,
          data = series_data
        )
        
        prediction %>%
          mutate(
            Year_plus_Site = series_id,
            span = span_value
          )
      })
    )
  })
)

fit_loess_flight_curve(
  data = test_series,
  span = 0.25
)

fit_loess_flight_curve(
  data = test_series,
  span = 0.275
)

fit_loess_flight_curve(
  data = test_series,
  span = 0.30
)


# ============================================================
# Development conclusion
# ============================================================

# GAM captures the broad seasonal activity pattern well,
# but may smooth over shorter local flight waves.
#
# LOESS with smaller span preserves more local structure,
# but very small span values (approximately < 0.30 in the
# tested 2021Dignajas series) become unstable or nearly
# interpolate individual observations.
#
# Therefore candidate peaks will initially be detected from
# observed standardized activity, while smoothed curves will
# later be used to assess peak shape, prominence and stability.
=======
>>>>>>> e72e7b70c2c5f8a8852adcb81c7d3d10d293e6e7
