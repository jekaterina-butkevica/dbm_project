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


# Pair quality----------------------------------------------------

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


# Training data-------------------------------------------------------

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
  unique(train_pairs$Year_plus_Site)
)


# Helper functions-----------------------------------------------------

weighted_median <- function(x, w) {
  
  keep <-
    !is.na(x) &
    !is.na(w) &
    w > 0
  
  x <- x[keep]
  w <- w[keep]
  
  ord <- order(x)
  
  x <- x[ord]
  w <- w[ord]
  
  cumulative_weight <-
    cumsum(w) / sum(w)
  
  x[
    which(cumulative_weight >= 0.5)[1]
  ]
}


estimate_K_iterative <- function(
    data,
    max_iter = 50,
    tolerance = 0.01
) {
  
  K_current <- weighted_median(
    x = data$degree_days,
    w = data$quality_pair_weight
  )
  
  for (i in seq_len(max_iter)) {
    
    selected <- data %>%
      mutate(
        thermal_relative_error =
          abs(degree_days - K_current) /
          K_current
      ) %>%
      arrange(
        Year_plus_Site,
        thermal_relative_error,
        desc(pair_quality_score)
      ) %>%
      group_by(
        Year_plus_Site
      ) %>%
      slice(1) %>%
      ungroup()
    
    K_new <- median(
      selected$degree_days
    )
    
    if (
      abs(K_new - K_current) <
      tolerance
    ) {
      break
    }
    
    K_current <- K_new
  }
  
  list(
    K = K_new,
    selected_pairs = selected,
    iterations = i
  )
}


# Leave-one-series-out CV for each Tbase---------------------------------------

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
            abs(degree_days - K_train) /
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
    unique(pair_candidates_train$Tbase)
  ),
  run_cv_for_tbase
) %>%
  bind_rows()


n_tbase_scenarios <- n_distinct(
  cv_all_tbase$Tbase
)


# Consensus pair across Tbase scenarios------------------------------------

cv_consensus <- cv_all_tbase %>%
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
    modal_pair = first(modal_pair),
    
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


# Training-derived reliability threshold

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
  error_q3 + 1.5 * error_iqr


cv_consensus <- cv_consensus %>%
  mutate(
    reliable_pair =
      median_error <=
      error_upper_fence
  )


# Training-derived stability threshold -----------------------------------------

stability_q1 <- quantile(
  cv_consensus$modal_pair_fraction,
  0.25
)

stability_q3 <- quantile(
  cv_consensus$modal_pair_fraction,
  0.75
)

stability_iqr <-
  stability_q3 - stability_q1

stability_lower_fence <-
  stability_q1 -
  1.5 * stability_iqr


cv_consensus <- cv_consensus %>%
  mutate(
    stable_pair =
      modal_pair_fraction >=
      stability_lower_fence,
    
    accepted_pair =
      reliable_pair &
      stable_pair
  )


# Accepted generation pairs in training data -----------------------------

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


# Full-training K profile---------------------------------------------------
# Needed later for independent/new-data pair selection

training_K_profile <- lapply(
  sort(
    unique(pair_candidates_train$Tbase)
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


# Save algorithm parameters,----------------------------------------------------

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