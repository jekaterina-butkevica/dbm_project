# Import and initial inspection of DBM and meteorological data

# Pakotnes ---------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)


# 1. Import raw data -----------------------------------------

moth_raw <- read_excel(
  "data/raw/diamondback_count_21_25.xlsx"
)

meteo_raw <- read_excel(
  "data/raw/Meteo 21-25.xlsx"
)


# 2. Initial inspection --------------------------------------

glimpse(moth_raw)
glimpse(meteo_raw)

names(moth_raw)
names(meteo_raw)

head(moth_raw)
head(meteo_raw)



# 3. Data quality checks -------------------------------------

# Convert datetime to plain Date.
# We keep the original raw objects unchanged and create
# temporary QC versions.

moth_qc <- moth_raw |>
  mutate(
    Date = as.Date(Date)
  )

meteo_qc <- meteo_raw |>
  mutate(
    Date = as.Date(Date)
  )



# 3.1. How many site-year series are present? -----------------

moth_qc |>
  distinct(Year, Site, Year_plus_Site) |>
  arrange(Year, Site)

moth_qc |>
  summarise(
    n_series = n_distinct(Year_plus_Site),
    n_sites = n_distinct(Site),
    min_year = min(Year),
    max_year = max(Year)
  )


# 3.2. Number of traps represented at each assessment date ----

trap_check <- moth_qc |>
  count(
    Year_plus_Site,
    Year,
    Site,
    Date,
    name = "n_traps"
  )

table(trap_check$n_traps)


# Show cases where number of trap observations is not 5
trap_check |>
  filter(n_traps != 5) |>
  arrange(Year, Site, Date)



# 3.3. Calculate intervals between successive assessments ----

date_check <- trap_check |>
  arrange(Year_plus_Site, Date) |>
  group_by(Year_plus_Site) |>
  mutate(
    previous_date = lag(Date),
    interval_days = as.numeric(Date - previous_date)
  ) |>
  ungroup()

summary(date_check$interval_days)

table(date_check$interval_days, useNA = "ifany")


# Show unusually short or long intervals
date_check |>
  filter(
    !is.na(interval_days),
    interval_days < 5 | interval_days > 9
  ) |>
  arrange(Year_plus_Site, Date)



# 3.4. Check whether Year agrees with the calendar date ------


moth_qc |>
  filter(year(Date) != Year) |>
  distinct(
    Year_plus_Site,
    Year,
    Site,
    Date,
    Absdate
  ) |>
  arrange(Year_plus_Site, Date)


# 3.5. Check missing values -----------------------------------


colSums(is.na(moth_qc))

colSums(is.na(meteo_qc))



# 3.6. Check meteorological data uniqueness ------------------
# One site-year-date should normally correspond to one row.

meteo_qc |>
  count(Year_plus_Site, Date, name = "n") |>
  filter(n != 1)



# 3.7. Inspect unusual numbers of traps ------------------------


trap_check |>
  filter(n_traps != 5) |>
  count(Site, n_traps) |>
  arrange(Site, n_traps)


unusual_trap_dates <- trap_check |>
  filter(!n_traps %in% c(3, 5)) |>
  arrange(Year_plus_Site, Date)

unusual_trap_dates



moth_qc |>
  semi_join(
    unusual_trap_dates,
    by = c(
      "Year_plus_Site",
      "Year",
      "Site",
      "Date"
    )
  ) |>
  arrange(Year_plus_Site, Date, Diamondback_count)



# 3.8. Date ranges by series ---------------------------------

series_ranges <- moth_qc |>
  group_by(Year_plus_Site, Year, Site) |>
  summarise(
    first_date = min(Date),
    last_date = max(Date),
    n_assessments = n_distinct(Date),
    .groups = "drop"
  ) |>
  arrange(Year, Site)

series_ranges



date_check |>
  filter(
    !is.na(interval_days),
    interval_days > 14
  ) |>
  arrange(Year_plus_Site, Date)
