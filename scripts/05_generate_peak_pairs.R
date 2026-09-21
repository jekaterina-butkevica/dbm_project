# 05_generate_peak_pairs.R
# Generate candidate DBM peak pairs


# Packages ----------------------------------------------------

library(dplyr)
library(tidyr)


# Load data ---------------------------------------------------

candidate_peaks <- readRDS(
  "data/processed/candidate_peaks.rds"
)


# Load functions ----------------------------------------------

source("R/pairing_functions.R")


# Generate all peak pairs -------------------------------------

all_peak_pairs <- candidate_peaks %>%
  group_by(Year_plus_Site) %>%
  group_modify(
    ~ generate_peak_pairs(.x)
  ) %>%
  ungroup()


# Broad biological time window -------------------------------

candidate_generation_pairs <- all_peak_pairs %>%
  filter(
    generation_days >= 18,
    generation_days <= 50
  )


# Save --------------------------------------------------------

saveRDS(
  all_peak_pairs,
  "data/processed/all_peak_pairs.rds"
)

saveRDS(
  candidate_generation_pairs,
  "data/processed/candidate_generation_pairs.rds"
)


if (file.exists("data/processed/all_peak_pairs.rds")) {
  cat(
    'Fails "data/processed/all_peak_pairs.rds" ir izveidots.\n'
  )
}

if (file.exists("data/processed/candidate_generation_pairs.rds")) {
  cat(
    'Fails "data/processed/candidate_generation_pairs.rds" ir izveidots.\n'
  )
}