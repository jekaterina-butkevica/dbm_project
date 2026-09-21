# ============================================================
# 12_estimate_peak_uncertainty.R
# Estimate uncertainty in peak dates and generation duration
# ============================================================

library(dplyr)

final_generation_pairs <- readRDS(
  "data/processed/final_generation_pairs.rds"
)

moth_analysis <- readRDS(
  "data/processed/moth_analysis.rds"
)



get_peak_window <- function(series_id, peak_date, moth_data) {
  
  series_dates <- moth_data %>%
    filter(
      Year_plus_Site == series_id
    ) %>%
    arrange(Date) %>%
    pull(Date)
  
  previous_date <- max(
    series_dates[
      series_dates < peak_date
    ],
    na.rm = TRUE
  )
  
  next_date <- min(
    series_dates[
      series_dates > peak_date
    ],
    na.rm = TRUE
  )
  
  tibble(
    lower_date = previous_date,
    peak_date = peak_date,
    upper_date = next_date
  )
}


peak_uncertainty <- final_generation_pairs %>%
  rowwise() %>%
  mutate(
    
    peak_1_lower = get_peak_window(
      Year_plus_Site,
      peak_1_date,
      moth_analysis
    )$lower_date,
    
    peak_1_upper = get_peak_window(
      Year_plus_Site,
      peak_1_date,
      moth_analysis
    )$upper_date,
    
    peak_2_lower = get_peak_window(
      Year_plus_Site,
      peak_2_date,
      moth_analysis
    )$lower_date,
    
    peak_2_upper = get_peak_window(
      Year_plus_Site,
      peak_2_date,
      moth_analysis
    )$upper_date
  ) %>%
  ungroup()



peak_uncertainty <- peak_uncertainty %>%
  mutate(
    
    generation_days_min =
      as.numeric(
        peak_2_lower - peak_1_upper
      ),
    
    generation_days_max =
      as.numeric(
        peak_2_upper - peak_1_lower
      ),
    
    peak_1_window_days =
      as.numeric(
        peak_1_upper - peak_1_lower
      ),
    
    peak_2_window_days =
      as.numeric(
        peak_2_upper - peak_2_lower
      )
  )



peak_uncertainty %>%
  select(
    Year_plus_Site,
    peak_1_date,
    peak_1_lower,
    peak_1_upper,
    peak_2_date,
    peak_2_lower,
    peak_2_upper,
    generation_days,
    generation_days_min,
    generation_days_max
  )


peak_uncertainty %>%
  summarise(
    median_peak_1_window =
      median(peak_1_window_days, na.rm = TRUE),
    
    median_peak_2_window =
      median(peak_2_window_days, na.rm = TRUE),
    
    median_generation_uncertainty =
      median(
        generation_days_max -
          generation_days_min,
        na.rm = TRUE
      )
  )


# ============================================================
# Save peak-date uncertainty results
# ============================================================

saveRDS(
  peak_uncertainty,
  "data/processed/final_peak_uncertainty.rds"
)

peak_uncertainty_summary <- peak_uncertainty %>%
  summarise(
    median_peak_1_window =
      median(peak_1_window_days, na.rm = TRUE),
    
    median_peak_2_window =
      median(peak_2_window_days, na.rm = TRUE),
    
    median_generation_uncertainty =
      median(
        generation_days_max -
          generation_days_min,
        na.rm = TRUE
      )
  )

saveRDS(
  peak_uncertainty_summary,
  "data/processed/final_peak_uncertainty_summary.rds"
)

peak_uncertainty_summary
