# ============================================================
# smoothing_functions.R
# Functions for reconstructing DBM flight activity curves
# ============================================================


fit_gam_flight_curve <- function(data, k = 6) {
  
  model <- mgcv::gam(
    total_count ~ s(mid_time, k = k),
    family = mgcv::nb(),
    offset = log(trap_days),
    data = data,
    method = "REML"
  )
  
  return(model)
}

# Funckija ņem vienu year_plus_Site sēriju un fito gam modeli (Generalized Additive Model),
# GAM ir piemērots, jo mēs negribam pieņemt, ka kožu aktivitāte sezonā mainās lineāri.
# Galvena modeļa ideja ir izskaidrot kopējo noķerto kožu skaitu ar gludu funkciju no laika.


# Midtime izmantots tapēc, ka noķerto tauriņu skaits reprezentē pperiodu. Tāpec mēs ņemam videjo datumu.








predict_gam_flight_curve <- function(model, data) {
  
  prediction_grid <- data.frame(
    mid_time = seq(
      min(data$mid_time, na.rm = TRUE),
      max(data$mid_time, na.rm = TRUE),
      by = 1
    )
  )
  
  # Standardise effort to 1 trap-day,
  # so predictions represent estimated capture intensity.
  prediction_grid$trap_days <- 1
  
  prediction_grid$estimated_activity <- predict(
    model,
    newdata = prediction_grid,
    type = "response"
  )
  
  prediction_grid$Date <- as.Date(
    prediction_grid$mid_time,
    origin = "1970-01-01"
  )
  
  return(prediction_grid)
}

# Sākotnējais total_count tika koriģēts pēc lamatu ekspozīcijas.
# Prognozējot pie trap_days = 1, mēs iegūstam standartizētu aktivitātes 
# intensitāti uz vienu trap-day, nevis prognozētu kopējo skaitu konkrētam 5-lamatu × 7-dienu intervālam.

#(trap_days = n_traps * interval_days)




# LOESS

fit_loess_flight_curve <- function(data, span = 0.5) {
  
  model <- loess(
    total_count / trap_days ~ mid_time,
    data = data,
    span = span,
    degree = 2,
    family = "symmetric"
  )
  
  return(model)
}


#prognozēšanai

predict_loess_flight_curve <- function(model, data) {
  
  prediction_grid <- data.frame(
    mid_time = seq(
      min(data$mid_time, na.rm = TRUE),
      max(data$mid_time, na.rm = TRUE),
      by = 1
    )
  )
  
  prediction_grid$estimated_activity <- predict(
    model,
    newdata = prediction_grid
  )
  
  prediction_grid$Date <- as.Date(
    prediction_grid$mid_time,
    origin = "1970-01-01"
  )
  
  return(prediction_grid)
}



# Initial comparison suggests that GAM smoothing may remove
# short-term flight-wave structure, whereas LOESS with smaller
# span values preserves multiple candidate local peaks.
#
# The smoothing parameter will not be selected manually from
# this single series. It will later be optimised/validated
# across multiple Year_plus_Site series.
