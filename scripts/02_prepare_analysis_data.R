# 02_prepare_analysis_data.R
# Prepare assessment-level moth data for modelling



# Packages ----------------------------------------------------

library(dplyr)


# Load corrected data -----------------------------------------

moth_corrected <- readRDS(
  "data/processed/moth_corrected.rds"
)

meteo_corrected <- readRDS(
  "data/processed/meteo_corrected.rds"
)



# Prepare moth assessment data -------------------------------


# One raw row = one trap at one assessment.
# Aggregate all traps within the same site-year and date.

moth_assessment <- moth_corrected %>%
  group_by(
    Year_plus_Site,
    Year,
    Site,
    Date
  ) %>%
  summarise(
    n_traps = n(),
    total_count = sum(
      Diamondback_count,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    Year_plus_Site,
    Date
  )


# Calculate intervals between assessments --------------------

moth_assessment <- moth_assessment %>%
  group_by(
    Year_plus_Site
  ) %>%
  arrange(
    Date,
    .by_group = TRUE
  ) %>%
  mutate(
    previous_date =
      lag(Date),
    
    interval_days =
      as.numeric(
        Date - previous_date
      )
  ) %>%
  ungroup()


# Calculate interval midpoint --------------------------------

moth_assessment <- moth_assessment %>%
  mutate(
    mid_date =
      previous_date +
      interval_days / 2,
    
    mid_time =
      as.numeric(previous_date) +
      interval_days / 2
  )


# Calculate sampling effort ----------------------------------

moth_assessment <- moth_assessment %>%
  mutate(
    trap_days =
      n_traps *
      interval_days
  )


# Create analysis dataset ------------------------------------

moth_analysis <- moth_assessment %>%
  filter(
    !is.na(interval_days),
    interval_days > 0,
    !is.na(trap_days)
  )


# Save moth data ---------------------------------------------

saveRDS(
  moth_assessment,
  "data/processed/moth_assessment.rds"
)

saveRDS(
  moth_analysis,
  "data/processed/moth_analysis.rds"
)



# Prepare meteorological data ---------------------------------


meteo_analysis <- meteo_corrected %>%
  select(
    Year_plus_Site,
    Year,
    Site,
    Date,
    Taverage
  ) %>%
  arrange(
    Year_plus_Site,
    Date
  )


# Save meteorological data -----------------------------------

saveRDS(
  meteo_analysis,
  "data/processed/meteo_analysis.rds"
)


if (file.exists(
  "data/processed/moth_assessment.rds"
)) {
  cat(
    'Fails "data/processed/moth_assessment.rds" ir izveidots.\n'
  )
}

if (file.exists(
  "data/processed/moth_analysis.rds"
)) {
  cat(
    'Fails "data/processed/moth_analysis.rds" ir izveidots.\n'
  )
}

if (file.exists(
  "data/processed/meteo_analysis.rds"
)) {
  cat(
    'Fails "data/processed/meteo_analysis.rds" ir izveidots.\n'
  )
}