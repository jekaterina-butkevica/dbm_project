# 06_calculate_degree_days.R
# Calculate degree-days between candidate peaks


# Packages ----------------------------------------------------

library(dplyr)
library(tidyr)


# Load data ---------------------------------------------------

candidate_generation_pairs <- readRDS(
  "data/processed/candidate_generation_pairs.rds"
)

meteo_analysis <- readRDS(
  "data/processed/meteo_analysis.rds"
)


# Load functions ----------------------------------------------

source("R/temperature_functions.R")



# Izmēģinājusm
test_pair <- candidate_generation_pairs %>%
  filter(
    Year_plus_Site == "2021Dignajas",
    peak_1_date == as.Date("2021-07-02"),
    peak_2_date == as.Date("2021-07-30")
  )
test_pair

candidate_generation_pairs %>%
  filter(Year_plus_Site == "2021Dignajas") %>%
  select(
    peak_1_date,
    peak_2_date,
    generation_days
  )

candidate_generation_pairs %>%
  filter(Year_plus_Site == "2021Dignajas") %>%
  mutate(
    peak_1_numeric = as.numeric(peak_1_date),
    peak_2_numeric = as.numeric(peak_2_date)
  ) %>%
  select(
    peak_1_date,
    peak_1_numeric,
    peak_2_date,
    peak_2_numeric,
    generation_days
  )

calculate_pair_degree_days(
  pair_row = test_pair,
  meteo_data = meteo_analysis,
  Tbase = 0
)


calculate_pair_degree_days(
  pair_row = test_pair,
  meteo_data = meteo_analysis,
  Tbase = 8
)








test_temp_data <- meteo_analysis %>%
  filter(
    Year_plus_Site == "2021Dignajas",
    Date > as.Date("2021-07-02"),
    Date <= as.Date("2021-07-30")
  )

nrow(test_temp_data)


range(test_temp_data$Date)


#trukstoša temperartūra
sum(is.na(test_temp_data$Taverage))

min(test_temp_data$Taverage)




# ============================================================
# Calculate degree-days for all candidate pairs
# Tbase = 8
# ============================================================

dd_results_8 <- lapply(
  seq_len(nrow(candidate_generation_pairs)),
  function(i) {
    
    calculate_pair_degree_days(
      pair_row = candidate_generation_pairs[i, ],
      meteo_data = meteo_analysis,
      Tbase = 8
    )
  }
) %>%
  bind_rows()


candidate_pairs_dd_8 <- bind_cols(
  candidate_generation_pairs,
  dd_results_8
)

nrow(candidate_pairs_dd_8)


table(candidate_pairs_dd_8$n_missing_temp)


summary(candidate_pairs_dd_8$n_temp_days)
summary(candidate_pairs_dd_8$degree_days)



candidate_pairs_dd_8 %>%
  select(
    Year_plus_Site,
    peak_1_date,
    peak_2_date,
    generation_days,
    degree_days,
    n_temp_days,
    n_missing_temp,
    pair_peak_strength
  ) %>%
  arrange(degree_days) %>%
  print(n = 30)



# ============================================================
# Calculate degree-days across a range of base temperatures
# ============================================================

Tbase_values <- 0:12


dd_grid <- lapply(
  Tbase_values,
  function(tb) {
    
    dd_results <- lapply(
      seq_len(nrow(candidate_generation_pairs)),
      function(i) {
        
        calculate_pair_degree_days(
          pair_row = candidate_generation_pairs[i, ],
          meteo_data = meteo_analysis,
          Tbase = tb
        )
      }
    ) %>%
      bind_rows()
    
    bind_cols(
      candidate_generation_pairs,
      dd_results
    )
  }
) %>%
  bind_rows()

nrow(dd_grid)

table(dd_grid$Tbase)
table(dd_grid$n_missing_temp)


dd_summary_by_tbase <- dd_grid %>%
  group_by(Tbase) %>%
  summarise(
    n_pairs = n(),
    mean_dd = mean(degree_days),
    sd_dd = sd(degree_days),
    median_dd = median(degree_days),
    min_dd = min(degree_days),
    max_dd = max(degree_days),
    .groups = "drop"
  )

dd_summary_by_tbase


# Save degree-day results


saveRDS(
  dd_grid,
  "data/processed/candidate_pairs_degree_days.rds"
)

