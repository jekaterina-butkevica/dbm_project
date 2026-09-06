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