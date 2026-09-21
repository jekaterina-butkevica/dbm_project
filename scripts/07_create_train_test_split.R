# 07_create_train_test_split.R
# Create independent training and test series


# Packages ----------------------------------------------------

library(dplyr)


# Load data ---------------------------------------------------

candidate_generation_pairs <- readRDS(
  "data/processed/candidate_generation_pairs.rds"
)

moth_assessment <- readRDS(
  "data/processed/moth_assessment.rds"
)


# Prepare series metadata ------------------------------------

series_metadata <- moth_assessment %>%
  distinct(
    Year_plus_Site,
    Year,
    Site
  )


# Keep only series with candidate generation pairs ------------

series_split <- candidate_generation_pairs %>%
  distinct(
    Year_plus_Site
  ) %>%
  left_join(
    series_metadata,
    by = "Year_plus_Site"
  )


# Create reproducible train/test split ------------------------

set.seed(2026)

series_split <- series_split %>%
  group_by(Year) %>%
  mutate(
    random_order = runif(n())
  ) %>%
  arrange(
    Year,
    random_order
  ) %>%
  mutate(
    n_in_year = n(),
    
    n_test_in_year = pmax(
      1,
      round(0.20 * n_in_year)
    ),
    
    split = if_else(
      row_number() <= n_test_in_year,
      "test",
      "train"
    )
  ) %>%
  ungroup() %>%
  select(
    Year_plus_Site,
    Year,
    Site,
    split
  )


# Save --------------------------------------------------------

saveRDS(
  series_split,
  "data/processed/train_test_split.rds"
)


if (file.exists("data/processed/train_test_split.rds")) {
  cat(
    'Fails "data/processed/train_test_split.rds" ir izveidots.\n'
  )
}