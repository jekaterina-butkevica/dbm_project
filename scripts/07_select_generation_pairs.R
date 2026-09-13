# 07_select_generation_pairs.R
# Compare candidate generation pairs


library(dplyr)


# Load data ---------------------------------------------------

dd_grid <- readRDS(
  "data/processed/candidate_pairs_degree_days.rds"
)


# Start with one base temperature -----------------------------

pairs_tbase_8 <- dd_grid %>%
  filter(
    Tbase == 8
  )



pairs_per_series <- pairs_tbase_8 %>%
  count(
    Year_plus_Site,
    name = "n_candidate_pairs"
  ) %>%
  arrange(
    desc(n_candidate_pairs)
  )

pairs_per_series



pairs_tbase_8 %>%
  filter(
    Year_plus_Site == "2021Dignajas"
  ) %>%
  select(
    peak_1_date,
    peak_2_date,
    generation_days,
    degree_days,
    pair_peak_strength,
    pair_edge_support
  ) %>%
  arrange(
    generation_days
  )


pairs_tbase_8 %>%
  filter(
    Year_plus_Site == "2021Dignajas"
  ) %>%
  arrange(
    peak_1_time,
    peak_2_time
  ) %>%
  select(
    peak_1,
    peak_2,
    peak_1_date,
    peak_2_date,
    generation_days,
    degree_days,
    pair_peak_strength,
    pair_edge_support
  )




source("R/chain_functions.R")
two_generation_chains_8 <-
  generate_two_generation_chains(
    pairs_tbase_8
  )


nrow(two_generation_chains_8)

two_generation_chains_8 %>%
  filter(
    Year_plus_Site == "2021Dignajas"
  ) %>%
  select(
    Year_plus_Site,
    peak_1_a,
    peak_2_a,
    peak_2_b,
    generation_days_a,
    generation_days_b,
    degree_days_a,
    degree_days_b,
    dd_difference,
    relative_dd_difference,
    chain_peak_strength,
    chain_edge_support
  )






two_generation_chains_all <-
  generate_two_generation_chains(
    dd_grid
  )
nrow(two_generation_chains_all)
table(two_generation_chains_all$Tbase)


chain_summary_by_tbase <-
  two_generation_chains_all %>%
  group_by(Tbase) %>%
  summarise(
    n_chains = n(),
    mean_relative_dd_difference =
      mean(relative_dd_difference),
    
    median_relative_dd_difference =
      median(relative_dd_difference),
    
    .groups = "drop"
  )

chain_summary_by_tbase






# Inspect chain quality ------------


chain_quality <- two_generation_chains_all %>%
  filter(Tbase == 8) %>%
  select(
    Year_plus_Site,
    peak_1_a,
    peak_2_a,
    peak_2_b,
    chain_peak_strength,
    chain_edge_support
  ) %>%
  arrange(
    chain_peak_strength
  )

summary(chain_quality$chain_peak_strength)
table(chain_quality$chain_edge_support)
chain_quality %>%
  print(n = 40)

chain_quality %>%
  summarise(
    n_chains = n(),
    n_series = n_distinct(Year_plus_Site)
  )



# Quality-weighted thermal consistency within each series --------

series_tbase_summary <- two_generation_chains_all %>%
  group_by(
    Tbase,
    Year_plus_Site
  ) %>%
  summarise(
    n_chains = n(),
    
    # All compatible chains treated equally
    mean_relative_dd_difference =
      mean(relative_dd_difference),
    
    # Better-supported peak chains receive more weight
    weighted_relative_dd_difference =
      weighted.mean(
        relative_dd_difference,
        w = chain_peak_strength
      ),
    
    .groups = "drop"
  )


series_tbase_summary


tbase_series_summary <- series_tbase_summary %>%
  group_by(Tbase) %>%
  summarise(
    n_series = n(),
    
    mean_series_difference =
      mean(mean_relative_dd_difference),
    
    median_series_difference =
      median(mean_relative_dd_difference),
    
    mean_weighted_series_difference =
      mean(weighted_relative_dd_difference),
    
    median_weighted_series_difference =
      median(weighted_relative_dd_difference),
    
    .groups = "drop"
  )

tbase_series_summary



series_tbase_summary %>%
  filter(Tbase == 8) %>%
  arrange(
    weighted_relative_dd_difference
  ) %>%
  print(n = Inf)





# Leave-one-series-out sensitivity analysis ------------

series_ids <- unique(
  series_tbase_summary$Year_plus_Site
)

loo_results <- lapply(
  series_ids,
  function(series_to_remove) {
    
    temp <- series_tbase_summary %>%
      filter(
        Year_plus_Site != series_to_remove
      ) %>%
      group_by(Tbase) %>%
      summarise(
        mean_weighted_difference =
          mean(weighted_relative_dd_difference),
        .groups = "drop"
      )
    
    best <- temp %>%
      slice_min(
        mean_weighted_difference,
        n = 1,
        with_ties = FALSE
      )
    
    tibble(
      removed_series = series_to_remove,
      best_Tbase = best$Tbase,
      best_difference = best$mean_weighted_difference
    )
  }
) %>%
  bind_rows()


loo_results

table(loo_results$best_Tbase)

summary(loo_results$best_difference)





# Sensitivity to chain quality thresholds ----------


quality_thresholds <- c(
  0,
  0.001,
  0.005,
  0.01,
  0.02,
  0.05
)

quality_sensitivity <- lapply(
  quality_thresholds,
  function(threshold) {
    
    temp <- two_generation_chains_all %>%
      filter(
        chain_peak_strength >= threshold
      ) %>%
      group_by(
        Tbase,
        Year_plus_Site
      ) %>%
      summarise(
        weighted_relative_dd_difference =
          weighted.mean(
            relative_dd_difference,
            w = chain_peak_strength
          ),
        .groups = "drop"
      ) %>%
      group_by(Tbase) %>%
      summarise(
        n_series = n(),
        mean_weighted_difference =
          mean(weighted_relative_dd_difference),
        .groups = "drop"
      )
    
    best <- temp %>%
      slice_min(
        mean_weighted_difference,
        n = 1,
        with_ties = FALSE
      )
    
    tibble(
      threshold = threshold,
      best_Tbase = best$Tbase,
      best_difference = best$mean_weighted_difference,
      n_series = best$n_series
    )
  }
) %>%
  bind_rows()


quality_sensitivity
