# 04_detect_peaks.R
# Detect candidate DBM flight peaks


# Packages ----------------------------------------------------

library(dplyr)


# Load data ---------------------------------------------------

moth_analysis <- readRDS(
  "data/processed/moth_analysis.rds"
)


# Load functions ----------------------------------------------

source("R/peak_functions.R")


# Detect candidate peaks in all series ------------------------

all_candidate_peaks <- moth_analysis %>%
  group_by(Year_plus_Site) %>%
  group_modify(
    ~ detect_candidate_peaks(.x)
  ) %>%
  ungroup()


candidate_peaks_only <- all_candidate_peaks %>%
  filter(is_candidate_peak)


# Prepare candidate peak table -------------------------------

peak_summary <- candidate_peaks_only %>%
  select(
    Year_plus_Site,
    Year,
    Site,
    mid_date,
    mid_time,
    activity,
    relative_prominence,
    relative_to_max,
    observations_before,
    observations_after,
    observations_to_edge,
    edge_distance
  ) %>%
  arrange(
    Year_plus_Site,
    mid_time
  )


# Save --------------------------------------------------------

saveRDS(
  peak_summary,
  "data/processed/candidate_peaks.rds"
)


if (file.exists("data/processed/candidate_peaks.rds")) {
  cat(
    'Fails "data/processed/candidate_peaks.rds" ir izveidots.\n'
  )
}