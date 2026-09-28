# input_validation_functions.R
# Functions for validating DBM moth and meteorological input data


validate_moth_data <- function(data) {
  
  required_columns <- c(
    "Year_plus_Site",
    "Year",
    "Site",
    "Date",
    "total_count",
    "n_traps",
    "interval_days",
    "mid_date",
    "mid_time",
    "trap_days"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Moth data are missing required columns:",
        paste(
          missing_columns,
          collapse = ", "
        )
      )
    )
  }
  
  
  if (!inherits(data$Date, "Date")) {
    stop(
      'Column "Date" in moth data must be of class Date.'
    )
  }
  
  
  if (any(is.na(data$Year_plus_Site))) {
    stop(
      'Column "Year_plus_Site" contains missing values.'
    )
  }
  
  
  if (any(is.na(data$mid_time))) {
    stop(
      'Column "mid_time" contains missing values.'
    )
  }
  
  
  if (any(
    data$interval_days <= 0,
    na.rm = TRUE
  )) {
    stop(
      'Column "interval_days" must contain only positive values.'
    )
  }
  
  
  if (any(
    data$n_traps <= 0,
    na.rm = TRUE
  )) {
    stop(
      'Column "n_traps" must contain only positive values.'
    )
  }
  
  
  if (any(
    data$trap_days <= 0,
    na.rm = TRUE
  )) {
    stop(
      'Column "trap_days" must contain only positive values.'
    )
  }
  
  
  if (any(
    data$total_count < 0,
    na.rm = TRUE
  )) {
    stop(
      'Column "total_count" cannot contain negative values.'
    )
  }
  
  
  invisible(TRUE)
}



validate_meteo_data <- function(data) {
  
  required_columns <- c(
    "Year_plus_Site",
    "Year",
    "Site",
    "Date",
    "Taverage"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Meteorological data are missing required columns:",
        paste(
          missing_columns,
          collapse = ", "
        )
      )
    )
  }
  
  
  if (!inherits(data$Date, "Date")) {
    stop(
      'Column "Date" in meteorological data must be of class Date.'
    )
  }
  
  
  if (any(is.na(data$Year_plus_Site))) {
    stop(
      'Column "Year_plus_Site" contains missing values.'
    )
  }
  
  
  invisible(TRUE)
}

