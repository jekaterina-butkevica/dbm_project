# 05_generate_peak_pairs.R
# Generate all candidate peak pairs 


# Packages ----------------------------------------------------

library(dplyr)
library(tidyr)


# Load data ---------------------------------------------------

candidate_peaks <- readRDS(
  "data/processed/candidate_peaks.rds"
)


# Load functions ----------------------------------------------

source("R/pairing_functions.R")



# viena serija

test_peaks <- candidate_peaks %>%
  filter(
    Year_plus_Site == "2021Dignajas"
  )

test_pairs <- generate_peak_pairs(
  test_peaks
)

test_pairs
nrow(test_pairs)






# Generate peak pairs for all series

all_peak_pairs <- candidate_peaks %>%
  group_by(Year_plus_Site) %>%
  group_modify(
    ~ generate_peak_pairs(.x)
  ) %>%
  ungroup()

nrow(all_peak_pairs)
summary(all_peak_pairs$generation_days)



all_peak_pairs %>%
  arrange(generation_days) %>%
  select(
    Year_plus_Site,
    peak_1_date,
    peak_2_date,
    generation_days,
    pair_peak_strength,
    pair_edge_support
  ) %>%
  head(30)



# ============================================================
# Broad biological time window
# ============================================================

candidate_generation_pairs <- all_peak_pairs %>%
  filter(
    generation_days >= 18,
    generation_days <= 50
  )


nrow(candidate_generation_pairs)
summary(candidate_generation_pairs$generation_days)

candidate_generation_pairs %>%
  arrange(generation_days) %>%
  select(
    Year_plus_Site,
    peak_1_date,
    peak_2_date,
    generation_days,
    pair_peak_strength,
    pair_edge_support
  ) %>%
  print(n = 40)



candidate_generation_pairs %>%
  filter(
    Year_plus_Site == "2021Dignajas"
  ) %>%
  select(
    peak_1,
    peak_2,
    generation_days,
    peak_1_prominence,
    peak_2_prominence,
    peak_1_relative_to_max,
    peak_2_relative_to_max,
    pair_min_prominence,
    pair_min_relative_to_max
  )

# ============================================================
# Save candidate peak pairs
# ============================================================

saveRDS(
  all_peak_pairs,
  "data/processed/all_peak_pairs.rds"
)

saveRDS(
  candidate_generation_pairs,
  "data/processed/candidate_generation_pairs.rds"
)
file.exists("data/processed/all_peak_pairs.rds")
file.exists("data/processed/candidate_generation_pairs.rds")

