# 12_estimate_peak_uncertainty.R
# Estimate observation-window uncertainty in peak timing
# and generation duration


# Packages ----------------------------------------------------

library(dplyr)


# Load data ---------------------------------------------------

final_generation_pairs <- readRDS(
  "data/processed/final_generation_pairs.rds"
)

moth_analysis <- readRDS(
  "data/processed/moth_analysis.rds"
)


# Function to recover the trap interval represented by a peak -----------

get_peak_window <- function(
    series_id,
    peak_time,
    moth_data
) {
  
  peak_row <- moth_data %>%
    filter(
      Year_plus_Site == series_id,
      mid_time == peak_time
    )
  
  tibble(
    lower_date = peak_row$previous_date,
    upper_date = peak_row$Date
  )
}


# Peak timing windows ------------------------------------------------------

peak_uncertainty <- final_generation_pairs %>%
  rowwise() %>%
  mutate(
    
    peak_1_lower = get_peak_window(
      Year_plus_Site,
      peak_1_time,
      moth_analysis
    )$lower_date,
    
    peak_1_upper = get_peak_window(
      Year_plus_Site,
      peak_1_time,
      moth_analysis
    )$upper_date,
    
    peak_2_lower = get_peak_window(
      Year_plus_Site,
      peak_2_time,
      moth_analysis
    )$lower_date,
    
    peak_2_upper = get_peak_window(
      Year_plus_Site,
      peak_2_time,
      moth_analysis
    )$upper_date
  ) %>%
  ungroup()


# Generation-duration uncertainty ------------------------------------------

peak_uncertainty <- peak_uncertainty %>%
  mutate(
    
    generation_days_min =
      as.numeric(
        peak_2_lower -
          peak_1_upper
      ),
    
    generation_days_max =
      as.numeric(
        peak_2_upper -
          peak_1_lower
      ),
    
    peak_1_window_days =
      as.numeric(
        peak_1_upper -
          peak_1_lower
      ),
    
    peak_2_window_days =
      as.numeric(
        peak_2_upper -
          peak_2_lower
      )
  )


# Summary -----------------------------------------------------------

peak_uncertainty_summary <- peak_uncertainty %>%
  summarise(
    
    median_peak_1_window =
      median(
        peak_1_window_days,
        na.rm = TRUE
      ),
    
    median_peak_2_window =
      median(
        peak_2_window_days,
        na.rm = TRUE
      ),
    
    median_generation_uncertainty =
      median(
        generation_days_max -
          generation_days_min,
        na.rm = TRUE
      )
  )


# Save ------------------------------

saveRDS(
  peak_uncertainty,
  "data/processed/final_peak_uncertainty.rds"
)

saveRDS(
  peak_uncertainty_summary,
  "data/processed/final_peak_uncertainty_summary.rds"
)


if (file.exists(
  "data/processed/final_peak_uncertainty.rds"
)) {
  cat(
    'Fails "data/processed/final_peak_uncertainty.rds" ir izveidots.\n'
  )
}

if (file.exists(
  "data/processed/final_peak_uncertainty_summary.rds"
)) {
  cat(
    'Fails "data/processed/final_peak_uncertainty_summary.rds" ir izveidots.\n'
  )
}