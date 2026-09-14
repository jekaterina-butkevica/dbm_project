# ============================================================
# temperature_functions.R
# Functions for degree-day calculations
# ============================================================


calculate_pair_degree_days <- function(
    pair_row,
    meteo_data,
    Tbase
) {
  
  series_id <- pair_row$Year_plus_Site
  start_date <- pair_row$peak_1_date
  end_date <- pair_row$peak_2_date
  
  temp_data <- meteo_data %>%
    filter(
      Year_plus_Site == series_id,
      Date > start_date,
      Date <= end_date
    ) %>%
    mutate(
      daily_dd = pmax(
        Taverage - Tbase,
        0
      )
    )
  
  result <- tibble(
    Tbase = Tbase,
    
    degree_days = sum(
      temp_data$daily_dd,
      na.rm = TRUE
    ),
    
    n_temp_days = nrow(
      temp_data
    ),
    
    n_missing_temp = sum(
      is.na(temp_data$Taverage)
    )
  )
  
  return(result)
}




# ============================================================
# Most informative intervals for Tbase
# ============================================================

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
