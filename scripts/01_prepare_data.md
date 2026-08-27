01_prepare_data
================
2026-08-28

## Pakotnes

``` r
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyverse)
```

## Direktorija

Ir jāizmanto projekta sāknes direktoriju, nevis šī faila direktoriju!

## 1. Import raw data

``` r
setwd("../")
moth_raw <- read_excel( # cekulkodes
  "data/raw/diamondback_count_21_25.xlsx"
)

meteo_raw <- read_excel( # laikapstakļi
  "data/raw/Meteo 21-25.xlsx"
)
```

Ieskāts datu struktūra:

``` r
glimpse(moth_raw)
```

    ## Rows: 2,962
    ## Columns: 6
    ## $ Year              <dbl> 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021…
    ## $ Site              <chr> "Daukstu", "Daukstu", "Daukstu", "Daukstu", "Daukstu…
    ## $ Year_plus_Site    <chr> "2021Daukstu", "2021Daukstu", "2021Daukstu", "2021Da…
    ## $ Date              <dttm> 2021-05-27, 2021-05-27, 2021-05-27, 2021-05-27, 202…
    ## $ Absdate           <dbl> 44343, 44343, 44343, 44343, 44343, 44350, 44350, 443…
    ## $ Diamondback_count <dbl> 13, 14, 10, 7, 7, 21, 18, 10, 9, 10, 7, 2, 0, 2, 3, …

``` r
glimpse(meteo_raw)
```

    ## Rows: 19,723
    ## Columns: 7
    ## $ Site           <chr> "Daukstu", "Daukstu", "Daukstu", "Daukstu", "Daukstu", …
    ## $ Year           <dbl> 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2…
    ## $ Year_plus_Site <chr> "2021Daukstu", "2021Daukstu", "2021Daukstu", "2021Dauks…
    ## $ Date           <dttm> 2021-01-01, 2021-01-02, 2021-01-03, 2021-01-04, 2021-0…
    ## $ Absdate        <dbl> 44197, 44198, 44199, 44200, 44201, 44202, 44203, 44204,…
    ## $ Taverage       <dbl> -0.2875000, -0.6916667, -1.6083333, -2.2791667, -1.4708…
    ## $ DDabove0       <dbl> 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, …

``` r
names(moth_raw)
```

    ## [1] "Year"              "Site"              "Year_plus_Site"   
    ## [4] "Date"              "Absdate"           "Diamondback_count"

``` r
names(meteo_raw)
```

    ## [1] "Site"           "Year"           "Year_plus_Site" "Date"          
    ## [5] "Absdate"        "Taverage"       "DDabove0"

## 2. Data quality checks

Convert datetime to plain Date. Oriģināli objekti netika aiztikti, tā
vieta izveidotas īslaicīgas versijas kvalitātes parbāudei.

``` r
moth_qc <- moth_raw %>% 
  mutate(
    Date = as.Date(Date)
  )

meteo_qc <- meteo_raw %>% 
  mutate(
    Date = as.Date(Date)
  )
```

### 2.1. How many site-year series are present?

``` r
moth_qc  %>% 
  distinct(Year, Site, Year_plus_Site)  %>% 
  arrange(Year, Site) 
```

    ## # A tibble: 37 × 3
    ##     Year Site     Year_plus_Site
    ##    <dbl> <chr>    <chr>         
    ##  1  2021 Daukstu  2021Daukstu   
    ##  2  2021 Dignajas 2021Dignajas  
    ##  3  2021 Dricanu  2021Dricanu   
    ##  4  2021 Elejas   2021Elejas    
    ##  5  2021 Kekavas  2021Kekavas   
    ##  6  2021 Ligatnes 2021Ligatnes  
    ##  7  2021 Varmes   2021Varmes    
    ##  8  2022 Dignajas 2022Dignajas  
    ##  9  2022 Dricanu  2022Dricanu   
    ## 10  2022 Elejas   2022Elejas    
    ## # ℹ 27 more rows

``` r
moth_qc %>% 
  summarise(
    n_series = n_distinct(Year_plus_Site),
    n_sites = n_distinct(Site),
    min_year = min(Year),
    max_year = max(Year)
  )
```

    ## # A tibble: 1 × 4
    ##   n_series n_sites min_year max_year
    ##      <int>   <int>    <dbl>    <dbl>
    ## 1       37      11     2021     2025

Datos ir pārstavēta 37 lamatu serija no 11 vietām.

### 2.2 Number of traps represented at each assessment date

``` r
trap_check <- moth_qc  %>% 
  count(
    Year_plus_Site,
    Year,
    Site,
    Date,
    name = "n_traps"
  )

table(trap_check$n_traps)
```

    ## 
    ##   1   2   3   4   5   6  10 
    ##   1   1  38   7 559   2   1

Metožu aprakstā noradīts, ka katrā vietā bija izvietotas 5 lamatas,
izņemot Jelgavu, kur tika izvietotas 3 lamatas. Lielāka daļa saskaitīto
lamatu katrā serīha atbilst noradītajam daudzumam, taču pastāv gadījumi,
kur skaits samazinājas vai ziteikti palielinājas (kopā 12 gadījumi).

Apskatām gadījumus, kur lamatu skaits nav vienāds ar 5.

``` r
trap_check %>% 
  filter(n_traps != 5) %>% 
  arrange(Year, Site, Date)
```

    ## # A tibble: 50 × 5
    ##    Year_plus_Site    Year Site         Date       n_traps
    ##    <chr>            <dbl> <chr>        <date>       <int>
    ##  1 2022Dricanu       2022 Dricanu      2022-08-25       4
    ##  2 2022Dricanu       2022 Dricanu      2022-09-01       6
    ##  3 2023Dignajas      2023 Dignajas     2023-05-31      10
    ##  4 2023Elejas        2023 Elejas       2023-08-21       6
    ##  5 2023Jelgava_city  2023 Jelgava_city 2023-05-15       3
    ##  6 2023Jelgava_city  2023 Jelgava_city 2023-05-22       3
    ##  7 2023Jelgava_city  2023 Jelgava_city 2023-05-29       3
    ##  8 2023Jelgava_city  2023 Jelgava_city 2023-06-05       3
    ##  9 2023Jelgava_city  2023 Jelgava_city 2023-06-12       3
    ## 10 2023Jelgava_city  2023 Jelgava_city 2023-06-19       3
    ## # ℹ 40 more rows

Aprakstā tika noradīts, ka Jelgava tika izvietotas tikai 3 lamatas.
Apskatam vai Jelgavā vienmēr ir 3 lamatas.

``` r
trap_check %>% 
  filter(Site == "Jelgava_city") %>% 
  count(n_traps)
```

    ## # A tibble: 1 × 2
    ##   n_traps     n
    ##     <int> <int>
    ## 1       3    38

Jā, Jelagvā vienmēr ir 3 lamatas. Pārbaudam, vai ir serijas ārpus
Jelgavas, kuros bija tikai tris lamatas:

``` r
trap_check %>% 
  filter(n_traps != 5) %>% 
  arrange(Year, Site, Date) %>% 
  filter(Site != "Jelgava_city") %>% 
  count(n_traps)
```

    ## # A tibble: 5 × 2
    ##   n_traps     n
    ##     <int> <int>
    ## 1       1     1
    ## 2       2     1
    ## 3       4     7
    ## 4       6     2
    ## 5      10     1

Nē, šādu seriju nav. Kopā 12 serijas apsēkoto lamatu skaits atšķiras no
noradītājā. Pārbaudam manuāli.

``` r
trap_check %>% 
  filter(n_traps != 5)  %>% 
  count(Site, n_traps)  %>% 
  arrange(Site, n_traps) %>% 
  filter(Site != "Jelgava_city")
```

    ## # A tibble: 8 × 3
    ##   Site     n_traps     n
    ##   <chr>      <int> <int>
    ## 1 Dignajas      10     1
    ## 2 Dricanu        4     1
    ## 3 Dricanu        6     1
    ## 4 Elejas         6     1
    ## 5 Pures          4     5
    ## 6 Tirzas         2     1
    ## 7 Varmes         1     1
    ## 8 Varmes         4     1

``` r
unusual_trap_dates <- trap_check |>
  filter(!n_traps %in% c(3, 5)) |>
  arrange(Year_plus_Site, Date)
unusual_trap_dates
```

    ## # A tibble: 12 × 5
    ##    Year_plus_Site  Year Site     Date       n_traps
    ##    <chr>          <dbl> <chr>    <date>       <int>
    ##  1 2022Dricanu     2022 Dricanu  2022-08-25       4
    ##  2 2022Dricanu     2022 Dricanu  2022-09-01       6
    ##  3 2023Dignajas    2023 Dignajas 2023-05-31      10
    ##  4 2023Elejas      2023 Elejas   2023-08-21       6
    ##  5 2023Pures       2023 Pures    2023-09-04       4
    ##  6 2023Pures       2023 Pures    2023-09-11       4
    ##  7 2023Pures       2023 Pures    2023-09-18       4
    ##  8 2023Pures       2023 Pures    2023-09-25       4
    ##  9 2023Pures       2023 Pures    2023-10-02       4
    ## 10 2023Tirzas      2023 Tirzas   2023-06-08       2
    ## 11 2025Varmes      2025 Varmes   2022-07-16       1
    ## 12 2025Varmes      2025 Varmes   2025-07-16       4

Apskāts:

- 2022Dricanu 25.08.2022 dati no 4 lamatam, bet nakamājā reizē -
  09.01.2022 dati no 6 lamatam, vai var būt ievādes kļūda ???

- 2023Dignajas 31.05.2023 ir dati no 10 lamatam, pēc tam tikai no 5. Ir
  vērtības 0 un 1, katra pārā skaita reižu - var būt datu dublēšana ???

- 2023Elejas - 21.08.2023 dati no 6 lamatam, pirms tam un pēc tam ir pa
  5 - ???

- 2023Pures - sezonā beigas 5 reizes pēc kārtas apsēkotas tikai 4
  lamatas, acimredzami vienkārši kāda iemesla dēļ samazinātais efforts.
  Nav kļūda.

- 2023Tirzas - dati no divām lamatām no 08.06.2023, pirms tam un pēc tam
  dati ir no visām 5 lamatam - ???

- 2025Varmes - vienreiz 4 lamatas, bet papildus viena lamata tājā pati
  datuma datēta ar 2022 gadu - datu ievādes kļūda.

### 2.3 Date ranges by series

``` r
series_ranges <- moth_qc %>% 
  group_by(Year_plus_Site, Year, Site)  %>% 
  summarise(
    first_date = min(Date),
    last_date = max(Date),
    n_assessments = n_distinct(Date),
    .groups = "drop"
  ) %>% 
  arrange(Year, Site)
summary(series_ranges)
```

    ##  Year_plus_Site          Year          Site             first_date        
    ##  Length:37          Min.   :2021   Length:37          Min.   :2021-05-27  
    ##  Class :character   1st Qu.:2022   Class :character   1st Qu.:2022-05-20  
    ##  Mode  :character   Median :2023   Mode  :character   Median :2023-05-30  
    ##                     Mean   :2023                      Mean   :2023-05-12  
    ##                     3rd Qu.:2024                      3rd Qu.:2024-05-29  
    ##                     Max.   :2025                      Max.   :2025-06-09  
    ##    last_date          n_assessments  
    ##  Min.   :2021-08-26   Min.   :10.00  
    ##  1st Qu.:2022-09-27   1st Qu.:14.00  
    ##  Median :2023-10-02   Median :16.00  
    ##  Mean   :2023-12-04   Mean   :16.46  
    ##  3rd Qu.:2024-09-03   3rd Qu.:18.00  
    ##  Max.   :2027-07-02   Max.   :37.00

Apskāts:

- 2022Kekavas ir divāini liels datu iegūšanas datumu skaits - 37.
  Apskatot, izskātas, ka datumu kļūdu nav, bet apsekošanas daļai sezonas
  notika rezi aptuvēni četras dienas.

Parbaudu intervāļu gārumus:

``` r
date_check <- trap_check %>% 
  arrange(Year_plus_Site, Date) %>% 
  group_by(Year_plus_Site) %>% 
  mutate(
    previous_date = lag(Date),
    interval_days = as.numeric(Date - previous_date)
  ) %>% 
  ungroup()

summary(date_check$interval_days)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    1.00    7.00    7.00   13.35    7.00 1082.00      37

``` r
table(date_check$interval_days, useNA = "ifany")
```

    ## 
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   21 
    ##    1    1   17   18    4   61  390   53    4    2    1    2    1    7    1    3 
    ##   64  210  619  667 1047 1082 <NA> 
    ##    1    1    1    1    1    1   37

``` r
date_check %>% 
  filter(
    !is.na(interval_days),
    interval_days > 14
  ) %>% 
  arrange(Year_plus_Site, Date)
```

    ## # A tibble: 10 × 7
    ##    Year_plus_Site  Year Site     Date       n_traps previous_date interval_days
    ##    <chr>          <dbl> <chr>    <date>       <int> <date>                <dbl>
    ##  1 2021Ligatnes    2021 Ligatnes 2024-09-02       5 2021-09-16             1082
    ##  2 2022Elejas      2022 Elejas   2024-05-11       5 2022-08-31              619
    ##  3 2024Dignajas    2024 Dignajas 2024-08-21       5 2024-08-06               15
    ##  4 2024Varmes      2024 Varmes   2024-07-18       5 2024-06-27               21
    ##  5 2025Elejas      2025 Elejas   2027-07-02       5 2025-09-03              667
    ##  6 2025Pures       2025 Pures    2025-08-14       5 2025-07-24               21
    ##  7 2025Varmes      2025 Varmes   2025-05-28       5 2022-07-16             1047
    ##  8 2025Varmes      2025 Varmes   2025-06-18       5 2025-05-28               21
    ##  9 2025Varmes      2025 Varmes   2025-11-06       5 2025-09-03               64
    ## 10 2025Varmes      2025 Varmes   2026-06-04       5 2025-11-06              210

Apskāts:

- 2021Ligatnes - ir viena sērija, kas datēta ar 2024 gadu. Datumu
  saskaņotību ar 2024. gadu neatradu. Iespējams tas ir 2021. gads,
  ievadīts ar kļūdu? Datumi to pieļauj.
- 2022Elejas - lidzīgi kā iepriekšēja punktā - viena serija no 2024.
  gada.
- 2024Dignajas - gārš intervāls. Vai nav izskrītusi viena serija?
- 2024Varmes - gārš intervāls. Vai nav izskrītusi viena serija?
- 2025Elejas - ir dati no 2027. gada, esmu pārliecināta, ka tas ir
  ievādes kļūda no 2025. gada.
- 2025Pures - gārš intervāls. Vai nav izskrītusi viena serija?
- 2025Varmes - 2022-07-16 - izskātas, ka ievades kļūda no 2025. gada;
  gārš intervāls starp 2025-06-18 un 2025-06-18. Vai nav izskrītusi
  viena serija?; Novērojumiem no 11.06.2025 izskatās, ka menesis un
  datums samainīti vietām - novēmbris nevar būt; dati no 2026. gada,
  visticamāk arī ir 2025.

## 3. Create assessment-level QC table
