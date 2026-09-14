# 07_select_generation_pairs.R
# Compare candidate generation pairs


library(dplyr)


# Load data ---------------------------------------------------

dd_grid <- readRDS(
  "data/processed/candidate_pairs_degree_days.rds"
)


train_test_split <- readRDS(
  "data/processed/train_test_split.rds"
)



# Pair quality components ----------


pair_candidates <- dd_grid %>%
  mutate(
    edge_score = pmin(
      pair_edge_support / 5,
      1
    )
  )


pair_candidates <- pair_candidates %>%
  mutate(
    pair_quality_score = (
      pair_min_prominence +
        pair_min_relative_to_max +
        edge_score
    ) / 3
  )







# Quality weights within each series -------------------


pair_candidates <- pair_candidates %>%
  group_by(
    Tbase,
    Year_plus_Site
  ) %>%
  mutate(
    quality_pair_weight =
      pair_quality_score /
      sum(pair_quality_score)
  ) %>%
  ungroup()



train_series <- train_test_split %>%
  filter(
    split == "train"
  ) %>%
  pull(
    Year_plus_Site
  )





pair_candidates_train <- pair_candidates %>%
  filter(
    Year_plus_Site %in% train_series
  )





pair_candidates_train %>%
  summarise(
    n_rows = n(),
    n_unique_pairs = n_distinct(
      paste(
        Year_plus_Site,
        peak_1,
        peak_2
      )
    ),
    n_series = n_distinct(Year_plus_Site),
    n_Tbase = n_distinct(Tbase)
  )









train_pairs <- pair_candidates_train %>%
  select(
    Year_plus_Site,
    peak_1,
    peak_2,
    peak_1_time,
    peak_2_time,
    peak_1_date,
    peak_2_date,
    generation_days,
    pair_min_prominence,
    pair_min_relative_to_max,
    pair_edge_support,
    edge_score,
    pair_quality_score,
    quality_pair_weight
  ) %>%
  distinct()


train_pairs %>%
  summarise(
    n_pairs = n(),
    n_series = n_distinct(Year_plus_Site)
  )














#pair_candidates       = pilnie dati, tikai starpposma objekts
#pair_candidates_train = DD analīze training datos
#train_pairs           = 79 unikālie training kandidātpāri

# ============================================================
# Inspect degree-day distributions
# ============================================================
dd_distribution_summary <- pair_candidates_train %>%
  filter(
    Tbase %in% c(0, 4, 8, 10)
  ) %>%
  group_by(Tbase) %>%
  summarise(
    q10 = quantile(
      degree_days,
      0.10
    ),
    
    q25 = quantile(
      degree_days,
      0.25
    ),
    
    median = median(
      degree_days
    ),
    
    q75 = quantile(
      degree_days,
      0.75
    ),
    
    q90 = quantile(
      degree_days,
      0.90
    ),
    
    .groups = "drop"
  )

dd_distribution_summary





cor(
  pairs_8$generation_days,
  pairs_8$degree_days
)









