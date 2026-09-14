# 07_select_generation_pairs.R
# Compare candidate generation pairs


library(dplyr)


# Load data ---------------------------------------------------

dd_grid <- readRDS(
  "data/processed/candidate_pairs_degree_days.rds"
)

meteo_analysis <- readRDS(
  "data/processed/meteo_analysis.rds"
)

candidate_generation_pairs <- readRDS(
  "data/processed/candidate_generation_pairs.rds"
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


pair_candidates %>%
  filter(
    Tbase == 8,
    Year_plus_Site == "2021Dignajas"
  ) %>%
  select(
    peak_1,
    peak_2,
    generation_days,
    degree_days,
    pair_min_prominence,
    pair_min_relative_to_max,
    pair_edge_support,
    edge_score,
    pair_quality_score
  ) %>%
  arrange(
    desc(pair_quality_score)
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

pair_candidates %>%
  filter(Tbase == 8) %>%
  group_by(Year_plus_Site) %>%
  summarise(
    n_pairs = n(),
    total_quality_weight =
      sum(quality_pair_weight),
    .groups = "drop"
  ) %>%
  arrange(desc(n_pairs)) %>%
  print(n = Inf)


pair_candidates %>%
  filter(
    Tbase == 8,
    Year_plus_Site == "2021Dignajas"
  ) %>%
  select(
    peak_1,
    peak_2,
    generation_days,
    degree_days,
    pair_quality_score,
    quality_pair_weight
  ) %>%
  arrange(desc(quality_pair_weight))






# ============================================================
# Inspect degree-day distributions
# ============================================================

dd_distribution_summary <- pair_candidates %>%
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


pair_candidates %>%
  filter(Tbase == 8) %>%
  select(
    Year_plus_Site,
    peak_1,
    peak_2,
    generation_days,
    degree_days,
    pair_quality_score,
    quality_pair_weight
  ) %>%
  arrange(degree_days) %>%
  print(n = Inf)



# ============================================================
# Can the data identify Tbase?
# ============================================================

candidate_intervals <- candidate_generation_pairs %>%
  select(
    Year_plus_Site,
    peak_1_date,
    peak_2_date
  ) %>%
  distinct()

temperature_identifiability <- candidate_intervals %>%
  rowwise() %>%
  mutate(
    min_temperature = min(
      meteo_analysis %>%
        filter(
          Year_plus_Site == .env$Year_plus_Site,
          Date > .env$peak_1_date,
          Date <= .env$peak_2_date
        ) %>%
        pull(Taverage),
      na.rm = TRUE
    )
  ) %>%
  ungroup()




summary(
  temperature_identifiability$min_temperature
)

temperature_identifiability %>%
  summarise(
    below_0  = sum(min_temperature <= 0),
    below_2  = sum(min_temperature <= 2),
    below_4  = sum(min_temperature <= 4),
    below_6  = sum(min_temperature <= 6),
    below_8  = sum(min_temperature <= 8),
    below_10 = sum(min_temperature <= 10),
    below_12 = sum(min_temperature <= 12)
  )




# Most informative intervals for Tbase ------------

informative_intervals <- temperature_identifiability %>%
  filter(
    min_temperature <= 12
  ) %>%
  left_join(
    candidate_generation_pairs %>%
      select(
        Year_plus_Site,
        peak_1,
        peak_2,
        peak_1_date,
        peak_2_date,
        generation_days,
        pair_min_prominence,
        pair_min_relative_to_max,
        pair_edge_support
      ),
    by = c(
      "Year_plus_Site",
      "peak_1_date",
      "peak_2_date"
    )
  ) %>%
  arrange(
    min_temperature
  )

informative_intervals %>%
  print(n = Inf)

informative_intervals %>%
  summarise(
    n_intervals = n(),
    n_series = n_distinct(Year_plus_Site)
  )


informative_intervals %>%
  count(
    Year_plus_Site,
    name = "n_informative_intervals"
  ) %>%
  arrange(
    desc(n_informative_intervals)
  ) %>%
  print(n = Inf)


# Cold-temperature exposure within informative intervals -----

temperature_exposure <- informative_intervals %>%
  rowwise() %>%
  mutate(
    
    n_days_total = {
      temp <- meteo_analysis %>%
        filter(
          Year_plus_Site == .env$Year_plus_Site,
          Date > .env$peak_1_date,
          Date <= .env$peak_2_date
        )
      
      nrow(temp)
    },
    
    n_days_below_8 = {
      temp <- meteo_analysis %>%
        filter(
          Year_plus_Site == .env$Year_plus_Site,
          Date > .env$peak_1_date,
          Date <= .env$peak_2_date
        )
      
      sum(temp$Taverage <= 8, na.rm = TRUE)
    },
    
    n_days_below_10 = {
      temp <- meteo_analysis %>%
        filter(
          Year_plus_Site == .env$Year_plus_Site,
          Date > .env$peak_1_date,
          Date <= .env$peak_2_date
        )
      
      sum(temp$Taverage <= 10, na.rm = TRUE)
    },
    
    n_days_below_12 = {
      temp <- meteo_analysis %>%
        filter(
          Year_plus_Site == .env$Year_plus_Site,
          Date > .env$peak_1_date,
          Date <= .env$peak_2_date
        )
      
      sum(temp$Taverage <= 12, na.rm = TRUE)
    }
  ) %>%
  ungroup()


temperature_exposure %>%
  select(
    Year_plus_Site,
    peak_1_date,
    peak_2_date,
    generation_days,
    min_temperature,
    n_days_total,
    n_days_below_8,
    n_days_below_10,
    n_days_below_12,
    pair_min_prominence,
    pair_min_relative_to_max,
    pair_edge_support
  ) %>%
  arrange(
    min_temperature
  ) %>%
  print(n = Inf)



temperature_exposure %>%
  summarise(
    intervals_with_days_below_8 =
      sum(n_days_below_8 > 0),
    
    intervals_with_days_below_10 =
      sum(n_days_below_10 > 0),
    
    intervals_with_days_below_12 =
      sum(n_days_below_12 > 0),
    
    max_days_below_8 =
      max(n_days_below_8),
    
    max_days_below_10 =
      max(n_days_below_10),
    
    max_days_below_12 =
      max(n_days_below_12)
  )


# Tbase = 0, 2, 4 un 6 °C mūsu datos būs ļoti grūti nošķirt pēc bioloģiskās reakcijas — lielākā daļa paaudzes notiek daudz siltākos apstākļos.


# Degree-day distribution at Tbase = 8   ----

pairs_8 <- pair_candidates %>%
  filter(Tbase == 8)

hist(
  pairs_8$degree_days,
  breaks = 15,
  main = "Candidate generation degree-days, Tbase = 8",
  xlab = "Degree-days"
)




dd_bins_8 <- pairs_8 %>%
  mutate(
    dd_group = cut(
      degree_days,
      breaks = seq(
        100,
        650,
        by = 50
      ),
      include.lowest = TRUE
    )
  ) %>%
  group_by(dd_group) %>%
  summarise(
    n_pairs = n(),
    
    total_quality_weight =
      sum(quality_pair_weight),
    
    n_series =
      n_distinct(Year_plus_Site),
    
    .groups = "drop"
  )

dd_bins_8




# Relationship between calendar duration and degree-days -----

pairs_8 %>%
  select(
    Year_plus_Site,
    peak_1,
    peak_2,
    generation_days,
    degree_days,
    pair_quality_score,
    quality_pair_weight
  ) %>%
  arrange(
    generation_days,
    degree_days
  ) %>%
  print(n = Inf)


duration_summary_8 <- pairs_8 %>%
  mutate(
    duration_group = cut(
      generation_days,
      breaks = c(
        18,
        25,
        32,
        39,
        51
      ),
      include.lowest = TRUE
    )
  ) %>%
  group_by(duration_group) %>%
  summarise(
    n_pairs = n(),
    n_series = n_distinct(Year_plus_Site),
    
    mean_days =
      mean(generation_days),
    
    mean_dd =
      mean(degree_days),
    
    median_dd =
      median(degree_days),
    
    min_dd =
      min(degree_days),
    
    max_dd =
      max(degree_days),
    
    .groups = "drop"
  )

duration_summary_8


cor(
  pairs_8$generation_days,
  pairs_8$degree_days
)













# Optimize thermal constant K for every Tbase ----------------

K_grid <- seq(
  100,
  600,
  by = 5
)


tbase_K_profile_relative <- lapply(
  sort(unique(pair_candidates$Tbase)),
  function(tb) {
    
    pairs_tb <- pair_candidates %>%
      filter(Tbase == tb)
    
    K_results <- lapply(
      K_grid,
      function(K) {
        
        temp <- pairs_tb %>%
          mutate(
            
            # One-generation interpretation
            relative_error_1 =
              abs(degree_days - K) / K,
            
            valid_1 =
              generation_days >= 18 &
              generation_days <= 50,
            
            # Two-generation interpretation
            relative_error_2 =
              abs(degree_days - 2 * K) / (2 * K),
            
            valid_2 =
              generation_days / 2 >= 18 &
              generation_days / 2 <= 50,
            
            constrained_error_1 = if_else(
              valid_1,
              relative_error_1,
              Inf
            ),
            
            constrained_error_2 = if_else(
              valid_2,
              relative_error_2,
              Inf
            ),
            
            best_relative_error = pmin(
              constrained_error_1,
              constrained_error_2
            )
          )
        
        tibble(
          Tbase = tb,
          K = K,
          
          weighted_relative_error = weighted.mean(
            temp$best_relative_error,
            w = temp$quality_pair_weight
          )
        )
      }
    ) %>%
      bind_rows()
    
    K_results %>%
      slice_min(
        weighted_relative_error,
        n = 1,
        with_ties = FALSE
      )
  }
) %>%
  bind_rows()


tbase_K_profile_relative


tbase_K_profile_relative %>%
  arrange(weighted_relative_error)

tbase_K_profile_relative %>%
  slice_min(
    weighted_relative_error,
    n = 1,
    with_ties = FALSE
  )


tbase_K_profile_relative

# Šie lauka dati nesatur pietiekami daudz tiešas informācijas, lai precīzi b
# identificētu zemu fizioloģisko Tbase. Zemajā Tbase diapazonā Tbase un termiskā 
# konstante K ir stipri savstarpēji kompensējami.



#Tbase precīzi neidentificējams no šiem datiem

