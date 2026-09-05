# ============================================================
# 02_prepare_analysis_data.R
# Prepare assessment-level moth data for modelling
# ============================================================

# Šajā failā no koriģētajiem datiem tiek izveidotas analīzei gatava kožu (assessment-level) tabula



# Pakotnes ----

library(dplyr)
library(lubridate)


# 1. Load corrected data -------------------------------------

moth_corrected <- readRDS(
  "data/processed/moth_corrected.rds"
)

meteo_corrected <- readRDS(
  "data/processed/meteo_corrected.rds"
)


#Uz šo brīdi viena rinda ir viena lamata. Analīzei ir nepieciešams sassumēt
# visu lamatu rezultātu katra uzskaites piegajienā.

# 2. Aggregate trap catches by assessment date ---------------

moth_assessment <- moth_corrected %>%
  group_by(
    Year_plus_Site,
    Year,
    Site,
    Date
  ) %>%
  summarise(
    n_traps = n(),
    total_count = sum(Diamondback_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    Year_plus_Site,
    Date
  )

glimpse(moth_assessment)
head(moth_assessment, 20)



# 3. Calculate intervals between assessments -----------------

moth_assessment <- moth_assessment %>%
  group_by(Year_plus_Site) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    previous_date = lag(Date),
    interval_days = as.numeric(Date - previous_date)
  ) %>%
  ungroup()



summary(moth_assessment$interval_days)

table(
  moth_assessment$interval_days,
  useNA = "ifany"
)

moth_assessment %>%
  filter(
    !is.na(interval_days),
    interval_days > 14
  ) %>%
  select(
    Year_plus_Site,
    Date,
    previous_date,
    interval_days,
    n_traps,
    total_count
  ) %>%
  print(n = Inf)



# 4. Calculate interval midpoint ------------------------------

moth_assessment <- moth_assessment %>%
  mutate(
    mid_date = previous_date + interval_days / 2,
    mid_time = as.numeric(previous_date) + interval_days / 2
  )


# 5. Calculate sampling effort -------------------------------

moth_assessment <- moth_assessment %>%
  mutate(
    trap_days = n_traps * interval_days
  )



moth_assessment %>%
  select(
    Year_plus_Site,
    Date,
    previous_date,
    mid_date,
    mid_time,
    interval_days,
    n_traps,
    total_count,
    trap_days
  ) %>%
  head(25)



# 6. Create modelling dataset --------------------------------

moth_analysis <- moth_assessment %>%
  filter(
    !is.na(interval_days),
    interval_days > 0,
    !is.na(trap_days)
  )

nrow(moth_assessment)
nrow(moth_analysis)


saveRDS(
  moth_assessment,
  "data/processed/moth_assessment.rds"
)

saveRDS(
  moth_analysis,
  "data/processed/moth_analysis.rds"
)
