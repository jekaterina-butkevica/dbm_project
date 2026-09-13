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