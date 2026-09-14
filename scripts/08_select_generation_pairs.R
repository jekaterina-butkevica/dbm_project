# 08_select_generation_pairs.R
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












# pair_candidates       = all candidate pairs × Tbase scenarios
# pair_candidates_train = training-only candidate pairs × Tbase scenarios
# train_pairs           = unique training candidate pairs

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






# Training data for generation-duration model --------------

train_pair_model_data <- pair_candidates_train %>%
  filter(
    Tbase == 0
  ) %>%
  mutate(
    mean_temperature =
      degree_days / n_temp_days,
    
    development_rate =
      1 / generation_days
  ) %>%
  select(
    Year_plus_Site,
    peak_1,
    peak_2,
    peak_1_date,
    peak_2_date,
    generation_days,
    mean_temperature,
    development_rate,
    pair_min_prominence,
    pair_min_relative_to_max,
    pair_edge_support,
    pair_quality_score,
    quality_pair_weight
  )


train_pair_model_data %>%
  summarise(
    n_pairs = n(),
    n_series = n_distinct(Year_plus_Site),
    min_days = min(generation_days),
    max_days = max(generation_days),
    min_mean_temperature = min(mean_temperature),
    max_mean_temperature = max(mean_temperature)
  )


summary(
  train_pair_model_data$mean_temperature
)




# ============================================================
# Inspect temperature-duration relationship in training data
# ============================================================

plot(
  train_pair_model_data$mean_temperature,
  train_pair_model_data$generation_days,
  xlab = "Mean temperature (°C)",
  ylab = "Generation interval (days)",
  main = "Candidate generation intervals: training data"
)


plot(
  train_pair_model_data$mean_temperature,
  train_pair_model_data$development_rate,
  xlab = "Mean temperature (°C)",
  ylab = "Development rate (1/day)",
  main = "Development rate vs temperature: training data"
)



development_lm_all <- lm(
  development_rate ~ mean_temperature,
  data = train_pair_model_data,
  weights = quality_pair_weight
)


summary(development_lm_all)

coef(development_lm_all)


# Diagnostic only:
# all candidate peak pairs together show essentially no
# temperature-development relationship.
# Candidate pairs therefore cannot all be treated as generations.





# Grouped cross-validation folds---------------
# Leave one Year_plus_Site series out at a time

cv_series <- sort(
  unique(train_pairs$Year_plus_Site)
)

cv_folds <- lapply(
  cv_series,
  function(validation_series) {
    
    tibble(
      validation_series = validation_series,
      training_series = list(
        setdiff(
          cv_series,
          validation_series
        )
      )
    )
  }
) %>%
  bind_rows()

nrow(cv_folds)


cv_folds %>%
  summarise(
    n_validation_series =
      n_distinct(validation_series),
    
    min_training_series =
      min(lengths(training_series)),
    
    max_training_series =
      max(lengths(training_series))
  )



cv_folds %>%
  rowwise() %>%
  mutate(
    validation_in_training =
      validation_series %in% training_series
  ) %>%
  ungroup() %>%
  count(validation_in_training)





# Baseline pair selection using peak information only ----


baseline_selected_pairs <- train_pairs %>%
  arrange(
    Year_plus_Site,
    desc(pair_quality_score),
    desc(pair_min_prominence),
    desc(pair_min_relative_to_max),
    desc(pair_edge_support),
    peak_1,
    peak_2
  ) %>%
  group_by(Year_plus_Site) %>%
  slice(1) %>%
  ungroup()


baseline_selected_pairs %>%
  summarise(
    n_pairs = n(),
    n_series = n_distinct(Year_plus_Site)
  )


baseline_model_data <- pair_candidates_train %>%
  filter(
    Tbase == 0
  ) %>%
  semi_join(
    baseline_selected_pairs,
    by = c(
      "Year_plus_Site",
      "peak_1",
      "peak_2"
    )
  ) %>%
  mutate(
    mean_temperature =
      degree_days / n_temp_days,
    
    development_rate =
      1 / generation_days
  )


baseline_model_data %>%
  select(
    Year_plus_Site,
    peak_1,
    peak_2,
    generation_days,
    mean_temperature,
    pair_quality_score
  ) %>%
  arrange(Year_plus_Site) %>%
  print(n = Inf)



baseline_development_lm <- lm(
  development_rate ~ mean_temperature,
  data = baseline_model_data
)

summary(baseline_development_lm)


# Secinājums: peak quality=generation probability

# ============================================================
# LOOCV thermal pair selection
# First working scenario: Tbase = 8
# ============================================================

pairs_train_8 <- pair_candidates_train %>%
  filter(
    Tbase == 8
  )

weighted_median <- function(x, w) {
  
  keep <- !is.na(x) &
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

cv_selected_pairs_8 <- lapply(
  cv_series,
  function(validation_series) {
    
    # --------------------------------
    # 27 series used to estimate K
    # --------------------------------
    
    fold_training <- pairs_train_8 %>%
      filter(
        Year_plus_Site != validation_series
      )
    
    K_train <- weighted_median(
      x = fold_training$degree_days,
      w = fold_training$quality_pair_weight
    )
    
    # --------------------------------
    # Candidate pairs in held-out series
    # --------------------------------
    
    fold_validation <- pairs_train_8 %>%
      filter(
        Year_plus_Site == validation_series
      ) %>%
      mutate(
        K_train = K_train,
        
        thermal_relative_error =
          abs(degree_days - K_train) /
          K_train
      )
    
    # --------------------------------
    # Select thermally closest pair
    # --------------------------------
    
    selected_pair <- fold_validation %>%
      arrange(
        thermal_relative_error,
        desc(pair_quality_score),
        peak_1,
        peak_2
      ) %>%
      slice(1)
    
    selected_pair
  }
) %>%
  bind_rows()


cv_selected_pairs_8 %>%
  summarise(
    n_selected = n(),
    n_series =
      n_distinct(Year_plus_Site)
  )


summary(
  cv_selected_pairs_8$K_train
)

cv_selected_pairs_8 %>%
  select(
    Year_plus_Site,
    peak_1,
    peak_2,
    generation_days,
    degree_days,
    K_train,
    thermal_relative_error,
    pair_quality_score
  ) %>%
  arrange(
    Year_plus_Site
  ) %>%
  print(n = Inf)



summary(
  cv_selected_pairs_8$thermal_relative_error
)




candidate_counts_8 <- pairs_train_8 %>%
  count(
    Year_plus_Site,
    name = "n_candidates"
  )

cv_selected_pairs_8 <- cv_selected_pairs_8 %>%
  left_join(
    candidate_counts_8,
    by = "Year_plus_Site"
  )


cv_selected_pairs_8 %>%
  select(
    Year_plus_Site,
    n_candidates,
    generation_days,
    degree_days,
    K_train,
    thermal_relative_error,
    pair_quality_score
  ) %>%
  arrange(
    desc(thermal_relative_error)
  ) %>%
  print(n = Inf)


cv_selected_pairs_8 %>%
  group_by(
    n_candidates == 1
  ) %>%
  summarise(
    n_series = n(),
    median_error = median(
      thermal_relative_error
    ),
    mean_error = mean(
      thermal_relative_error
    ),
    max_error = max(
      thermal_relative_error
    ),
    .groups = "drop"
  )





estimate_K_iterative <- function(
    data,
    max_iter = 50,
    tolerance = 0.01
) {
  
  # Initial robust estimate
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
      group_by(Year_plus_Site) %>%
      slice(1) %>%
      ungroup()
    
    # Each series now contributes one selected pair
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


iterative_K_8 <- estimate_K_iterative(
  pairs_train_8
)


iterative_K_8$K

iterative_K_8$iterations

iterative_K_8$selected_pairs %>%
  summarise(
    n_pairs = n(),
    n_series =
      n_distinct(Year_plus_Site),
    
    median_error =
      median(thermal_relative_error),
    
    mean_error =
      mean(thermal_relative_error),
    
    max_error =
      max(thermal_relative_error)
  )





cv_iterative_selected_8 <- lapply(
  cv_series,
  function(validation_series) {
    
    fold_training <- pairs_train_8 %>%
      filter(
        Year_plus_Site != validation_series
      )
    
    fit <- estimate_K_iterative(
      fold_training
    )
    
    K_train <- fit$K
    
    fold_validation <- pairs_train_8 %>%
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
    
    fold_validation %>%
      mutate(
        K_iterations = fit$iterations
      )
  }
) %>%
  bind_rows()


cv_iterative_selected_8 %>%
  summarise(
    n_selected = n(),
    n_series = n_distinct(Year_plus_Site),
    
    median_K = median(K_train),
    min_K = min(K_train),
    max_K = max(K_train),
    
    median_error =
      median(thermal_relative_error),
    
    mean_error =
      mean(thermal_relative_error),
    
    max_error =
      max(thermal_relative_error)
  )

cv_iterative_selected_8 %>%
  select(
    Year_plus_Site,
    generation_days,
    degree_days,
    K_train,
    thermal_relative_error,
    pair_quality_score
  ) %>%
  arrange(
    desc(thermal_relative_error)
  ) %>%
  print(n = Inf)




# ============================================================
# LOOCV across all Tbase scenarios
# ============================================================

run_cv_for_tbase <- function(tb) {
  
  pairs_tb <- pair_candidates_train %>%
    filter(
      Tbase == tb
    )
  
  lapply(
    cv_series,
    function(validation_series) {
      
      # Training part of this CV fold
      fold_training <- pairs_tb %>%
        filter(
          Year_plus_Site != validation_series
        )
      
      # Iterative K estimated only from other series
      fit <- estimate_K_iterative(
        fold_training
      )
      
      K_train <- fit$K
      
      # Candidate pairs in held-out series
      fold_validation <- pairs_tb %>%
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
        slice(1) %>%
        mutate(
          K_iterations = fit$iterations
        )
      
      fold_validation
    }
  ) %>%
    bind_rows()
}


cv_all_tbase <- lapply(
  sort(unique(pair_candidates_train$Tbase)),
  run_cv_for_tbase
) %>%
  bind_rows()

cv_all_tbase %>%
  summarise(
    n_rows = n(),
    n_Tbase = n_distinct(Tbase),
    n_series = n_distinct(Year_plus_Site)
  )


cv_tbase_summary <- cv_all_tbase %>%
  group_by(Tbase) %>%
  summarise(
    median_K = median(K_train),
    min_K = min(K_train),
    max_K = max(K_train),
    sd_K = sd(K_train),
    
    median_error =
      median(thermal_relative_error),
    
    mean_error =
      mean(thermal_relative_error),
    
    q75_error =
      quantile(
        thermal_relative_error,
        0.75
      ),
    
    max_error =
      max(thermal_relative_error),
    
    .groups = "drop"
  )
cv_tbase_summary
