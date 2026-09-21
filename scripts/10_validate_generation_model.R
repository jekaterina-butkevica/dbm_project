# 10_validate_generation_model.R
# Final validation on held-out test series


# Packages ----------------------------------------------------

library(dplyr)


# Load data ---------------------------------------------------

dd_grid <- readRDS(
  "data/processed/candidate_pairs_degree_days.rds"
)

train_test_split <- readRDS(
  "data/processed/train_test_split.rds"
)

pair_selection_parameters <- readRDS(
  "data/processed/pair_selection_parameters.rds"
)

generation_model <- readRDS(
  "data/processed/generation_model_train.rds"
)


# Prepare candidate pairs ----------------------------------------------

pair_candidates <- dd_grid %>%
  mutate(
    edge_score = pmin(
      pair_edge_support / 5,
      1
    ),
    
    pair_quality_score = (
      pair_min_prominence +
        pair_min_relative_to_max +
        edge_score
    ) / 3
  ) %>%
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


# Test series ------------------------------------------------------------

test_series <- train_test_split %>%
  filter(
    split == "test"
  ) %>%
  pull(
    Year_plus_Site
  )


pair_candidates_test <- pair_candidates %>%
  filter(
    Year_plus_Site %in% test_series
  )


# Training-derived K profile ----------------------------------------

training_K_profile <-
  pair_selection_parameters$K_profile %>%
  rename(
    K_train = K
  )


# Select best candidate pair in each test series ------------------------
# for each Tbase scenario

test_selected_all_tbase <- pair_candidates_test %>%
  left_join(
    training_K_profile,
    by = "Tbase"
  ) %>%
  mutate(
    thermal_relative_error =
      abs(degree_days - K_train) /
      K_train
  ) %>%
  arrange(
    Tbase,
    Year_plus_Site,
    thermal_relative_error,
    desc(pair_quality_score),
    peak_1,
    peak_2
  ) %>%
  group_by(
    Tbase,
    Year_plus_Site
  ) %>%
  slice(1) %>%
  ungroup()


n_tbase_scenarios <- n_distinct(
  test_selected_all_tbase$Tbase
)


# Consensus pair across Tbase scenarios ------------------------------

test_consensus <- test_selected_all_tbase %>%
  mutate(
    pair_id = paste(
      peak_1,
      peak_2,
      sep = "-"
    )
  ) %>%
  group_by(
    Year_plus_Site
  ) %>%
  mutate(
    modal_pair = names(
      sort(
        table(pair_id),
        decreasing = TRUE
      )
    )[1]
  ) %>%
  filter(
    pair_id == modal_pair
  ) %>%
  summarise(
    modal_pair = first(
      modal_pair
    ),
    
    modal_pair_count = n(),
    
    modal_pair_fraction =
      modal_pair_count /
      n_tbase_scenarios,
    
    median_error = median(
      thermal_relative_error
    ),
    
    max_error = max(
      thermal_relative_error
    ),
    
    median_generation_days = median(
      generation_days
    ),
    
    median_pair_quality = median(
      pair_quality_score
    ),
    
    .groups = "drop"
  )


# Apply training-derived acceptance thresholds ---------------------------

test_consensus <- test_consensus %>%
  mutate(
    reliable_pair =
      median_error <=
      pair_selection_parameters$error_upper_fence,
    
    stable_pair =
      modal_pair_fraction >=
      pair_selection_parameters$stability_lower_fence,
    
    accepted_pair =
      reliable_pair &
      stable_pair
  )


# Prediction validation on accepted test pairs ----------------------------

accepted_test_pairs <- pair_candidates_test %>%
  mutate(
    pair_id = paste(
      peak_1,
      peak_2,
      sep = "-"
    )
  ) %>%
  filter(
    Tbase == 0
  ) %>%
  inner_join(
    test_consensus %>%
      filter(
        accepted_pair
      ) %>%
      select(
        Year_plus_Site,
        modal_pair
      ),
    by = "Year_plus_Site"
  ) %>%
  filter(
    pair_id == modal_pair
  ) %>%
  mutate(
    mean_temperature =
      degree_days /
      n_temp_days,
    
    observed_days =
      generation_days,
    
    predicted_rate =
      predict(
        generation_model,
        newdata = tibble(
          mean_temperature =
            mean_temperature
        )
      ),
    
    predicted_days =
      1 / predicted_rate,
    
    error_days =
      predicted_days -
      observed_days,
    
    absolute_error_days =
      abs(error_days),
    
    relative_error =
      absolute_error_days /
      observed_days
  )


# Validation summary ---------------------------------------------------

final_validation_summary <- tibble(
  
  n_test_series =
    n_distinct(
      test_consensus$Year_plus_Site
    ),
  
  n_accepted_series =
    sum(
      test_consensus$accepted_pair
    ),
  
  acceptance_rate =
    mean(
      test_consensus$accepted_pair
    ),
  
  MAE_days =
    mean(
      accepted_test_pairs$absolute_error_days
    ),
  
  RMSE_days =
    sqrt(
      mean(
        accepted_test_pairs$error_days^2
      )
    ),
  
  median_relative_error =
    median(
      accepted_test_pairs$relative_error
    ),
  
  mean_relative_error =
    mean(
      accepted_test_pairs$relative_error
    )
)


# Save -------------------------------------------------------

saveRDS(
  test_consensus,
  "data/processed/test_pair_validation.rds"
)

saveRDS(
  accepted_test_pairs,
  "data/processed/accepted_generation_pairs_test.rds"
)

saveRDS(
  final_validation_summary,
  "data/processed/final_validation_summary.rds"
)


if (file.exists(
  "data/processed/test_pair_validation.rds"
)) {
  cat(
    'Fails "data/processed/test_pair_validation.rds" ir izveidots.\n'
  )
}

if (file.exists(
  "data/processed/accepted_generation_pairs_test.rds"
)) {
  cat(
    'Fails "data/processed/accepted_generation_pairs_test.rds" ir izveidots.\n'
  )
}

if (file.exists(
  "data/processed/final_validation_summary.rds"
)) {
  cat(
    'Fails "data/processed/final_validation_summary.rds" ir izveidots.\n'
  )
}