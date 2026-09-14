# 07_create_train_test_split.R
# Create independent training and test series

library(dplyr)

candidate_generation_pairs <- readRDS(
  "data/processed/candidate_generation_pairs.rds"
)

moth_assessment <- readRDS(
  "data/processed/moth_assessment.rds"
)

# Metadata for each Year_plus_Site
series_metadata <- moth_assessment %>%
  distinct(
    Year_plus_Site,
    Year,
    Site
  )

# Keep only series that actually have candidate generation pairs
series_split <- candidate_generation_pairs %>%
  distinct(
    Year_plus_Site
  ) %>%
  left_join(
    series_metadata,
    by = "Year_plus_Site"
  )



series_split
nrow(series_split)

sum(is.na(series_split$Year))
sum(is.na(series_split$Site))



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




series_split %>%
  count(split)

series_split %>%
  count(
    Year,
    split
  )


series_split %>%
  arrange(
    split,
    Year,
    Site
  ) %>%
  print(n = Inf)



saveRDS(
  series_split,
  "data/processed/train_test_split.rds"
)
