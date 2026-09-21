# ============================================================
# 11_refit_final_model.R
# Refit final generation model after independent validation
# ============================================================

library(dplyr)

# Load accepted intervals ------------------------------------

accepted_train <- readRDS(
  "data/processed/accepted_generation_pairs_train.rds"
)

accepted_test <- readRDS(
  "data/processed/accepted_generation_pairs_test.rds"
)

meteo_analysis <- readRDS(
  "data/processed/meteo_analysis.rds"
)


# ============================================================
# Combine accepted train and test intervals
# ============================================================

final_generation_pairs <- bind_rows(
  accepted_train %>%
    mutate(dataset = "train"),
  
  accepted_test %>%
    mutate(dataset = "test")
)


final_generation_pairs %>%
  count(dataset)

final_generation_pairs %>%
  summarise(
    n_pairs = n(),
    n_series = n_distinct(Year_plus_Site)
  )