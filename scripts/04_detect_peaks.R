# ============================================================
# Detect candidate DBM flight peaks
# ============================================================


# Packages ----------------------------------------------------

library(dplyr)
library(ggplot2)


# Load data ---------------------------------------------------

moth_analysis <- readRDS(
  "data/processed/moth_analysis.rds"
)


# Load functions ----------------------------------------------

source("R/peak_functions.R")