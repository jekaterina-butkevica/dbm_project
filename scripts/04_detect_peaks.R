
# Detect candidate DBM flight peaks



# Packages ----------------------------------------------------

library(dplyr)
library(ggplot2)


# Load data ---------------------------------------------------

moth_analysis <- readRDS(
  "data/processed/moth_analysis.rds"
)


# Load functions ----------------------------------------------

source("R/peak_functions.R")


# Test series -------------------------------------------------

test_series <- moth_analysis %>%
  filter(
    Year_plus_Site == "2021Dignajas"
  )

test_peaks <- detect_candidate_peaks(
  test_series
)



test_peaks %>%
  filter(is_candidate_peak) %>%
  select(
    mid_date,
    activity,
    previous_activity,
    next_activity,
    rise,
    drop,
    local_prominence,
    relative_prominence
  )


test_peaks %>%
  filter(is_candidate_peak)

ggplot(
  test_peaks,
  aes(
    x = as.Date(mid_time, origin = "1970-01-01"),
    y = activity
  )
) +
  geom_line() +
  geom_point() +
  geom_point(
    data = test_peaks %>%
      filter(is_candidate_peak),
    size = 4
  ) +
  labs(
    title = "2021Dignajas: observed candidate peaks",
    x = "Date",
    y = "Moth catch per trap-day"
  ) +
  theme_minimal()




# Detect candidate peaks in all series


all_candidate_peaks <- moth_analysis %>%
  group_by(Year_plus_Site) %>%
  group_modify(
    ~ detect_candidate_peaks(.x)
  ) %>%
  ungroup()



candidate_peaks_only <- all_candidate_peaks %>%
  filter(is_candidate_peak)


nrow(candidate_peaks_only)
summary(candidate_peaks_only$relative_prominence)
candidate_peaks_only %>%
  count(Year_plus_Site, name = "n_candidate_peaks") %>%
  arrange(n_candidate_peaks) -> visi


ggplot(
  candidate_peaks_only,
  aes(x = relative_prominence)
) +
  geom_histogram(
    bins = 20
  ) +
  labs(
    title = "Distribution of candidate peak prominence",
    x = "Relative prominence",
    y = "Number of candidate peaks"
  ) +
  theme_minimal()


# Interesanti gadījumi ar relative prominence = 1, jo tas var būt arī ļoti mazas vērtības, kad apkart ir nulles

# ir serija bez neviena kandidātpīķa

all_series <- moth_analysis %>%
  distinct(Year_plus_Site)

series_with_peaks <- candidate_peaks_only %>%
  distinct(Year_plus_Site)

all_series %>%
  anti_join(
    series_with_peaks,
    by = "Year_plus_Site"
  )


# parbaudot manuāli, redzu, ka serija sākas līdošanas sezona vidū, un pēc tam sērija būtībā pārsvarā krīt kas visu čakarē.
# anāk, ka dati nesatur pietiekamu informāciju, lai lokalizētu šī agrīnā pīķa maksimumu.


# un šobrīd veidojas situācija, ka algoritms normāli atsķir tikai iekšejsus pīķus.





# relative prominance = 1 gadījumi

candidate_peaks_only %>%
  filter(relative_prominence == 1) %>%
  select(
    Year_plus_Site,
    mid_date,
    activity,
    previous_activity,
    next_activity,
    local_prominence,
    relative_prominence
  ) %>%
  arrange(activity) %>%
  print(n = Inf)

# visur ir ļoti mazi skaitļi




# Pilnais kandidātu saraksts pa serijam
candidate_peaks_only %>%
  count(
    Year_plus_Site,
    name = "n_candidate_peaks"
  ) %>%
  arrange(
    desc(n_candidate_peaks)
  ) %>%
  print(n = Inf)


# Candidate peak strength relative to each series


candidate_peaks_only %>%
  filter(relative_prominence == 1) %>%
  select(
    Year_plus_Site,
    mid_date,
    activity,
    series_median_activity,
    series_max_activity,
    relative_to_median,
    relative_to_max,
    relative_prominence
  ) %>%
  arrange(relative_to_max) %>%
  print(n = Inf)

summary(candidate_peaks_only$relative_to_max)


# Daudz nulles, tadēļ daudz kur mediāna sanāk nulle - nevar izmantot kā kvalitates kriteriju

# peak_strength apvieno cik pīķiis lokāli izteikts un cik liels tas ir visas sezonas kontekstā
summary(candidate_peaks_only$peak_strength)

candidate_peaks_only %>%
  arrange(peak_strength) %>%
  select(
    Year_plus_Site,
    mid_date,
    activity,
    relative_prominence,
    relative_to_max,
    peak_strength
  ) %>%
  print(n = 30)



# mālu problēma
summary(candidate_peaks_only$edge_distance)
candidate_peaks_only %>%
  arrange(edge_distance) %>%
  select(
    Year_plus_Site,
    mid_date,
    activity,
    peak_strength,
    days_from_start,
    days_to_end,
    edge_distance
  ) %>%
  print(n = 30)

table(candidate_peaks_only$observations_to_edge)


candidate_peaks_only %>%
  arrange(
    observations_to_edge,
    edge_distance
  ) %>%
  select(
    Year_plus_Site,
    mid_date,
    activity,
    peak_strength,
    observations_before,
    observations_after,
    observations_to_edge,
    edge_distance
  ) %>%
  print(n = 30)




# Candidate peak summary table ------------------


peak_summary <- candidate_peaks_only %>%
  select(
    Year_plus_Site,
    Year,
    Site,
    mid_date,
    activity,
    relative_prominence,
    relative_to_max,
    peak_strength,
    observations_before,
    observations_after,
    observations_to_edge,
    edge_distance
  ) %>%
  arrange(
    Year_plus_Site,
    mid_date
  )


glimpse(peak_summary)
head(peak_summary, 30)


saveRDS(
  peak_summary,
  "data/processed/candidate_peaks.rds"
)
