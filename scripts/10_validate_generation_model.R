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

meteo_analysis <- readRDS(
  "data/processed/meteo_analysis.rds"
)

# Load functions ----------------------------------------------

source(
  "R/pair_selection_functions.R"
)



# Prepare candidate pairs -----------------------------------


pair_candidates <- add_pair_quality(
  dd_grid
)



# Test series ----------------------------------------------


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



# Select best pair for each Tbase ---------------------------
# using training-derived K profile


test_selected_all_tbase <- select_pairs_from_K_profile(
  pair_candidates =
    pair_candidates_test,
  
  K_profile =
    pair_selection_parameters$K_profile
)



# Consensus pair across Tbase scenarios ----------------------


test_consensus <- summarise_pair_consensus(
  test_selected_all_tbase
)



# Apply training-derived acceptance thresholds ---------------


test_consensus <- apply_pair_acceptance(
  consensus =
    test_consensus,
  
  error_upper_fence =
    pair_selection_parameters$error_upper_fence,
  
  stability_lower_fence =
    pair_selection_parameters$stability_lower_fence
)



# Prediction validation on accepted test pairs ---------------

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
  rowwise() %>%
  mutate(
    mean_temperature = mean(
      meteo_analysis %>%
        filter(
          Year_plus_Site == .env$Year_plus_Site,
          Date > .env$peak_1_date,
          Date <= .env$peak_2_date
        ) %>%
        pull(Taverage),
      na.rm = TRUE
    ),
    
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
      abs(
        error_days
      ),
    
    relative_error =
      absolute_error_days /
      observed_days
  ) %>%
  ungroup()



# Validation summary -----------------------------------------


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