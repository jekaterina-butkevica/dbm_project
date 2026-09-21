
# 08_select_generation_pairs.R
# Select candidate generation pairs using thermal consistency



# Packages ----------------------------------------------------

library(dplyr)


# Load data ---------------------------------------------------

dd_grid <- readRDS(
  "data/processed/candidate_pairs_degree_days.rds"
)

train_test_split <- readRDS(
  "data/processed/train_test_split.rds"
)


# Load functions ----------------------------------------------

source(
  "R/pair_selection_functions.R"
)



# Pair quality -----------------------------------------------


pair_candidates <- add_pair_quality(
  dd_grid
)



# Training data ----------------------------------------------


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


# Unique peak pairs without repeated Tbase scenarios

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


cv_series <- sort(
  unique(
    train_pairs$Year_plus_Site
  )
)



# Leave-one-series-out CV for each Tbase -----------------------


run_cv_for_tbase <- function(tb) {
  
  pairs_tb <- pair_candidates_train %>%
    filter(
      Tbase == tb
    )
  
  lapply(
    cv_series,
    function(validation_series) {
      
      fold_training <- pairs_tb %>%
        filter(
          Year_plus_Site != validation_series
        )
      
      fit <- estimate_K_iterative(
        fold_training
      )
      
      K_train <- fit$K
      
      pairs_tb %>%
        filter(
          Year_plus_Site == validation_series
        ) %>%
        mutate(
          K_train = K_train,
          
          thermal_relative_error =
            abs(
              degree_days - K_train
            ) /
            K_train
        ) %>%
        arrange(
          thermal_relative_error,
          desc(pair_quality_score),
          peak_1,
          peak_2
        ) %>%
        slice(1)
    }
  ) %>%
    bind_rows()
}


cv_all_tbase <- lapply(
  sort(
    unique(
      pair_candidates_train$Tbase
    )
  ),
  run_cv_for_tbase
) %>%
  bind_rows()



# Consensus pair across Tbase scenarios ----------------------


cv_consensus <- summarise_pair_consensus(
  cv_all_tbase
)



# Training-derived acceptance thresholds ------------------------


error_q1 <- quantile(
  cv_consensus$median_error,
  0.25
)

error_q3 <- quantile(
  cv_consensus$median_error,
  0.75
)

error_iqr <-
  error_q3 - error_q1

error_upper_fence <-
  error_q3 +
  1.5 * error_iqr


stability_q1 <- quantile(
  cv_consensus$modal_pair_fraction,
  0.25
)

stability_q3 <- quantile(
  cv_consensus$modal_pair_fraction,
  0.75
)

stability_iqr <-
  stability_q3 -
  stability_q1

stability_lower_fence <-
  stability_q1 -
  1.5 * stability_iqr


cv_consensus <- apply_pair_acceptance(
  consensus = cv_consensus,
  error_upper_fence = error_upper_fence,
  stability_lower_fence = stability_lower_fence
)



# Accepted generation pairs in training data -----------------------


accepted_generation_pairs_train <- train_pairs %>%
  mutate(
    pair_id = paste(
      peak_1,
      peak_2,
      sep = "-"
    )
  ) %>%
  inner_join(
    cv_consensus %>%
      filter(
        accepted_pair
      ) %>%
      select(
        Year_plus_Site,
        modal_pair,
        modal_pair_fraction,
        median_error,
        median_pair_quality
      ),
    by = "Year_plus_Site"
  ) %>%
  filter(
    pair_id == modal_pair
  )



# Full-training K profile -----------------------------------


training_K_profile <- lapply(
  sort(
    unique(
      pair_candidates_train$Tbase
    )
  ),
  function(tb) {
    
    fit <- pair_candidates_train %>%
      filter(
        Tbase == tb
      ) %>%
      estimate_K_iterative()
    
    tibble(
      Tbase = tb,
      K = fit$K
    )
  }
) %>%
  bind_rows()



# Save algorithm parameters ------------------------------------


pair_selection_parameters <- list(
  
  error_upper_fence =
    as.numeric(
      error_upper_fence
    ),
  
  stability_lower_fence =
    as.numeric(
      stability_lower_fence
    ),
  
  Tbase_values =
    sort(
      unique(
        pair_candidates_train$Tbase
      )
    ),
  
  K_profile =
    training_K_profile
)


saveRDS(
  pair_selection_parameters,
  "data/processed/pair_selection_parameters.rds"
)

saveRDS(
  accepted_generation_pairs_train,
  "data/processed/accepted_generation_pairs_train.rds"
)


if (file.exists(
  "data/processed/pair_selection_parameters.rds"
)) {
  cat(
    'Fails "data/processed/pair_selection_parameters.rds" ir izveidots.\n'
  )
}

if (file.exists(
  "data/processed/accepted_generation_pairs_train.rds"
)) {
  cat(
    'Fails "data/processed/accepted_generation_pairs_train.rds" ir izveidots.\n'
  )
}