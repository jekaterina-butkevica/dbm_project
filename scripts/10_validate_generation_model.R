# 10_validate_generation_model.R
# Final validation on held-out test series



# Pakotnes-----
library(dplyr)




# dati ----
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




train_series <- train_test_split %>%
  filter(split == "train") %>%
  pull(Year_plus_Site)

test_series <- train_test_split %>%
  filter(split == "test") %>%
  pull(Year_plus_Site)

pair_candidates_train <- pair_candidates %>%
  filter(
    Year_plus_Site %in% train_series
  )

pair_candidates_test <- pair_candidates %>%
  filter(
    Year_plus_Site %in% test_series
  )



pair_candidates_test %>%
  summarise(
    n_rows = n(),
    n_pairs = n_distinct(
      paste(
        Year_plus_Site,
        peak_1,
        peak_2
      )
    ),
    n_series = n_distinct(Year_plus_Site),
    n_Tbase = n_distinct(Tbase)
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
      group_by(Year_plus_Site) %>%
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




training_K_profile <- lapply(
  pair_selection_parameters$Tbase_values,
  function(tb) {
    
    training_tb <- pair_candidates_train %>%
      filter(
        Tbase == tb
      )
    
    fit <- estimate_K_iterative(
      training_tb
    )
    
    tibble(
      Tbase = tb,
      K_train = fit$K
    )
  }
) %>%
  bind_rows()



training_K_profile





# ============================================================
# Select best candidate pair in each test series
# using training-derived K only
# ============================================================

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



test_selected_all_tbase %>%
  summarise(
    n_rows = n(),
    n_series = n_distinct(Year_plus_Site),
    n_Tbase = n_distinct(Tbase)
  )



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
    modal_pair = first(modal_pair),
    
    modal_pair_count = n(),
    
    modal_pair_fraction =
      modal_pair_count / 13,
    
    median_error =
      median(
        thermal_relative_error
      ),
    
    max_error =
      max(
        thermal_relative_error
      ),
    
    median_generation_days =
      median(
        generation_days
      ),
    
    median_pair_quality =
      median(
        pair_quality_score
      ),
    
    .groups = "drop"
  )



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



test_consensus %>%
  select(
    Year_plus_Site,
    modal_pair,
    modal_pair_fraction,
    median_error,
    median_pair_quality,
    reliable_pair,
    stable_pair,
    accepted_pair
  ) %>%
  arrange(
    Year_plus_Site
  ) %>%
  print(n = Inf)



test_consensus %>%
  count(
    accepted_pair
  )





# ============================================================
# Final prediction validation on accepted test pairs
# ============================================================

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
      degree_days / n_temp_days,
    
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


accepted_test_pairs %>%
  select(
    Year_plus_Site,
    peak_1,
    peak_2,
    observed_days,
    mean_temperature,
    predicted_days,
    error_days,
    absolute_error_days,
    relative_error
  ) %>%
  arrange(
    Year_plus_Site
  ) %>%
  print(n = Inf)





accepted_test_pairs %>%
  summarise(
    n_test_pairs = n(),
    
    MAE_days =
      mean(
        absolute_error_days
      ),
    
    RMSE_days =
      sqrt(
        mean(
          error_days^2
        )
      ),
    
    median_relative_error =
      median(
        relative_error
      ),
    
    mean_relative_error =
      mean(
        relative_error
      )
  )



# Algoritms identificēja uzticamu paaudzes intervālu 3 no 6 neatkarīgajām test 
#sērijām (50%). Šajās sērijās paaudzes ilguma modeļa MAE bija 3.26 dienas, 
#RMSE 3.50 dienas un mediānā relatīvā kļūda 13.9%.



saveRDS(
  test_consensus,
  "data/processed/test_pair_validation.rds"
)

saveRDS(
  accepted_test_pairs,
  "data/processed/accepted_generation_pairs_test.rds"
)



final_validation_summary <- tibble(
  n_test_series =
    n_distinct(test_consensus$Year_plus_Site),
  
  n_accepted_series =
    sum(test_consensus$accepted_pair),
  
  acceptance_rate =
    mean(test_consensus$accepted_pair),
  
  MAE_days =
    mean(accepted_test_pairs$absolute_error_days),
  
  RMSE_days =
    sqrt(
      mean(accepted_test_pairs$error_days^2)
    ),
  
  median_relative_error =
    median(accepted_test_pairs$relative_error),
  
  mean_relative_error =
    mean(accepted_test_pairs$relative_error)
)

final_validation_summary


saveRDS(
  final_validation_summary,
  "data/processed/final_validation_summary.rds"
)
