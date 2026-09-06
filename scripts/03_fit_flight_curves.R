# ============================================================
# 03_fit_flight_curves.R
# Fit DBM flight activity curves
# ============================================================

# Pakotnes ----

library(dplyr)
library(ggplot2)
library(mgcv)


# Load data ---------------------------------------------------

moth_analysis <- readRDS(
  "data/processed/moth_analysis.rds"
)


# Load functions ----------------------------------------------

source("R/smoothing_functions.R")
