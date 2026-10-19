# temperature_functions.R
# Functions for degree-day calculations


calculate_pair_degree_days <- function(
    pair_row,
    meteo_data,
    Tbase
) {
  
  series_id <-
    pair_row$Year_plus_Site
  
  start_time <-
    pair_row$peak_1_time
  
  end_time <-
    pair_row$peak_2_time
  
  
  # Daily temperature records overlapping the peak interval ----
  
  temp_data <- meteo_data %>%
    filter(
      Year_plus_Site == series_id
    ) %>%
    mutate(
      day_start =
        as.numeric(Date),
      
      day_end =
        day_start + 1,
      
      # Fraction of each calendar day lying inside the interval
      day_weight =
        pmax(
          0,
          pmin(day_end, end_time) -
            pmax(day_start, start_time)
        )
    ) %>%
    filter(
      day_weight > 0
    ) %>%
    mutate(
      daily_dd =
        pmax(
          Taverage - Tbase,
          0
        ),
      
      weighted_dd =
        daily_dd * day_weight
    )
  
  
  result <- tibble(
    
    Tbase =
      Tbase,
    
    degree_days =
      sum(
        temp_data$weighted_dd,
        na.rm = TRUE
      ),
    
    n_temp_days =
      nrow(
        temp_data
      ),
    
    n_missing_temp =
      sum(
        is.na(
          temp_data$Taverage
        )
      ),
    
    temperature_coverage_days =
      sum(
        temp_data$day_weight
      ),
    
    expected_interval_days =
      end_time - start_time
  )
  
  
  return(result)
}


calculate_interval_mean_temperature <- function(
    series_id,
    start_time,
    end_time,
    meteo_data
) {
  
  temp_data <- meteo_data %>%
    filter(
      Year_plus_Site == series_id
    ) %>%
    mutate(
      day_start = as.numeric(Date),
      day_end = day_start + 1,
      
      day_weight = pmax(
        0,
        pmin(day_end, end_time) -
          pmax(day_start, start_time)
      )
    ) %>%
    filter(
      day_weight > 0
    )
  
  
  if (any(is.na(temp_data$Taverage))) {
    stop(
      "Mean-temperature calculation contains missing temperature data."
    )
  }
  
  
  if (
    abs(
      sum(temp_data$day_weight) -
      (end_time - start_time)
    ) > 1e-8
  ) {
    stop(
      "Temperature data do not fully cover a generation interval."
    )
  }
  
  
  weighted.mean(
    temp_data$Taverage,
    temp_data$day_weight
  )
}

