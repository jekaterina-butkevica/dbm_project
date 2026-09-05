01_prepare_data
================
2026-09-05

Šajā failā veicuveicu datu izpēti un ieviesu labojumus, pēc Edītes
komentāru saņemšanas.

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

???

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

## 2. Datuma pārvērsana

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

## Izveidot korekcijas failus

``` r
moth_corrected <- moth_qc
```

# Kožu dati

### Vietas, lamatu skaiti

#### 2.1. How many site-year series are present?

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

#### 2.2 Number of traps represented at one asssessment

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

**Pēc Edītes atbildes saņemšānas ievesu šadus labojumus:**

# Manuālas palaišanas gadījumā, šo boku nedrikst palais vairākas reizes!

2022Dricanu: viens no 09.01.2022 ierakstiem pārcelts uz 25.08.2022
(“visticamāk ievades kļūda, kopējot datumus pārceļam pirmo vērtību no
9.01.2022 uz 25.08.2022”)

``` r
moth_corrected %>%
  filter(
    Year_plus_Site == "2022Dricanu",
    Date == as.Date("2022-09-01")
  )
```

    ## # A tibble: 6 × 6
    ##    Year Site    Year_plus_Site Date       Absdate Diamondback_count
    ##   <dbl> <chr>   <chr>          <date>       <dbl>             <dbl>
    ## 1  2022 Dricanu 2022Dricanu    2022-09-01   44805                 0
    ## 2  2022 Dricanu 2022Dricanu    2022-09-01   44805                 0
    ## 3  2022 Dricanu 2022Dricanu    2022-09-01   44805                 0
    ## 4  2022 Dricanu 2022Dricanu    2022-09-01   44805                 0
    ## 5  2022 Dricanu 2022Dricanu    2022-09-01   44805                 0
    ## 6  2022 Dricanu 2022Dricanu    2022-09-01   44805                 0

``` r
moth_corrected <- moth_corrected %>%
  group_by(Year_plus_Site, Date) %>%
  mutate(
    row_in_date = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    Date = case_when(
      Year_plus_Site == "2022Dricanu" &
        Date == as.Date("2022-09-01") &
        row_in_date == 1 ~ as.Date("2022-08-25"),

      TRUE ~ Date
    )
  ) %>%
  select(-row_in_date)
```

2023Dignajas 31.05.2023 ir ievadīts divas reizes pēc kārtas, atstāju
tikai pusi no vertībām. (“jā 31.05.2023 ir ievadīts divas reizes pēc
kārtas, reāli ir 1, 0, 1, 0, 0.”)

``` r
moth_corrected %>%
  filter(
    Year_plus_Site == "2023Dignajas",
    Date == as.Date("2023-05-31")
  )
```

    ## # A tibble: 10 × 6
    ##     Year Site     Year_plus_Site Date       Absdate Diamondback_count
    ##    <dbl> <chr>    <chr>          <date>       <dbl>             <dbl>
    ##  1  2023 Dignajas 2023Dignajas   2023-05-31   45077                 1
    ##  2  2023 Dignajas 2023Dignajas   2023-05-31   45077                 0
    ##  3  2023 Dignajas 2023Dignajas   2023-05-31   45077                 1
    ##  4  2023 Dignajas 2023Dignajas   2023-05-31   45077                 0
    ##  5  2023 Dignajas 2023Dignajas   2023-05-31   45077                 0
    ##  6  2023 Dignajas 2023Dignajas   2023-05-31   45077                 1
    ##  7  2023 Dignajas 2023Dignajas   2023-05-31   45077                 0
    ##  8  2023 Dignajas 2023Dignajas   2023-05-31   45077                 1
    ##  9  2023 Dignajas 2023Dignajas   2023-05-31   45077                 0
    ## 10  2023 Dignajas 2023Dignajas   2023-05-31   45077                 0

``` r
moth_corrected <- moth_corrected %>%
  group_by(Year_plus_Site, Date) %>%
  mutate(
    row_in_date = row_number()
  ) %>%
  ungroup() %>%
  filter(
    !(
      Year_plus_Site == "2023Dignajas" &
      Date == as.Date("2023-05-31") &
      row_in_date > 5
    )
  ) %>%
  select(-row_in_date)
```

2023Elejas - 21.08.2023 paradās papildus lamata - nolēmts izņemt
ierakstu ar 150 (“hmm, reāli nesaprotu, mazāk lamatu var būt ja kādu
vējš aizpūš vai agrotehnika salauž, bet vairāk nevar būt… Bet izskatās
ka tas ieraksts kur ir 150 tur neiederas, bet es reāli nevaru
iedomāties, kā tas tur ir nokļuvis. Jebkurā gadījumā metam 150 ārā”)

``` r
moth_corrected %>%
  filter(
    Year_plus_Site == "2023Elejas",
    Date == as.Date("2023-08-21")
  )
```

    ## # A tibble: 6 × 6
    ##    Year Site   Year_plus_Site Date       Absdate Diamondback_count
    ##   <dbl> <chr>  <chr>          <date>       <dbl>             <dbl>
    ## 1  2023 Elejas 2023Elejas     2023-08-21   45159                28
    ## 2  2023 Elejas 2023Elejas     2023-08-21   45159                85
    ## 3  2023 Elejas 2023Elejas     2023-08-21   45159                45
    ## 4  2023 Elejas 2023Elejas     2023-08-21   45159                62
    ## 5  2023 Elejas 2023Elejas     2023-08-21   45159                50
    ## 6  2023 Elejas 2023Elejas     2023-08-21   45159               150

``` r
moth_corrected <- moth_corrected %>%
  filter(
    !(
      Year_plus_Site == "2023Elejas" &
      Date == as.Date("2023-08-21") &
      Diamondback_count == 150
    )
  )
```

- 2023Pures - sezonā beigas 5 reizes pēc kārtas apsēkotas tikai 4
  lamatas, acimredzami vienkārši kāda iemesla dēļ samazinātais efforts.
  Nav kļūda.

- 2023Tirzas - dati no divām lamatām no 08.06.2023, pirms tam un pēc tam
  dati ir no visām 5 lamatam - (“visticamāk bojātas/ pazudušas
  lamatas”) - labojumus neveikšu

- 2025Varmes - vienreiz 4 lamatas 07.09.2025, bet papildus viena lamata
  tājā pati datuma datēta ar 2022 gadu - (“un 2022 tiešām ir datu
  ievades kļūda”) - man tomēr liekas, ka tas ir pareizais ieraksts ar
  nepareizo gadu:

``` r
moth_corrected %>%
  filter(
    Year_plus_Site == "2025Varmes",
    Date %in% as.Date(c("2022-07-16", "2025-07-16"))
  )
```

    ## # A tibble: 5 × 6
    ##    Year Site   Year_plus_Site Date       Absdate Diamondback_count
    ##   <dbl> <chr>  <chr>          <date>       <dbl>             <dbl>
    ## 1  2025 Varmes 2025Varmes     2022-07-16   44758                60
    ## 2  2025 Varmes 2025Varmes     2025-07-16   45854                19
    ## 3  2025 Varmes 2025Varmes     2025-07-16   45854                35
    ## 4  2025 Varmes 2025Varmes     2025-07-16   45854                 8
    ## 5  2025 Varmes 2025Varmes     2025-07-16   45854                22

``` r
moth_corrected <- moth_corrected %>%
  mutate(
    Date = case_when(
      Year_plus_Site == "2025Varmes" &
        Date == as.Date("2022-09-07") ~ as.Date("2025-09-07"),

      TRUE ~ Date
    )
  )
```

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
  notika rezi aptuvēni četras dienas. Edīte nokomentēja, ka tas ir
  pareizi - uzskaitēs vienkārši notika biežāk. Es neko nemetīšu ārā, jo
  pieejai tas netraucē. Tieši otrādi - biežakas uzskaites dod lielaku
  izškirtspēju.

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

**Pēc Edītes atbildes saņemšānas ievesu šadus labojumus:**

2021Ligatnes - (“tas ir tā ka tu saki, 2021. ievadīts ka 2024”) -
nomainu gadu

``` r
moth_corrected %>%
  filter(
    Year_plus_Site == "2021Ligatnes",
    Date %in% as.Date(c("2024-09-02", "2021-09-02"))
  )
```

    ## # A tibble: 5 × 6
    ##    Year Site     Year_plus_Site Date       Absdate Diamondback_count
    ##   <dbl> <chr>    <chr>          <date>       <dbl>             <dbl>
    ## 1  2021 Ligatnes 2021Ligatnes   2024-09-02   45537                 0
    ## 2  2021 Ligatnes 2021Ligatnes   2024-09-02   45537                 0
    ## 3  2021 Ligatnes 2021Ligatnes   2024-09-02   45537                 0
    ## 4  2021 Ligatnes 2021Ligatnes   2024-09-02   45537                 0
    ## 5  2021 Ligatnes 2021Ligatnes   2024-09-02   45537                 0

``` r
 moth_corrected <- moth_corrected %>%
  mutate(
    Date = case_when(
      Year_plus_Site == "2021Ligatnes" &
        Date == as.Date("2024-09-02") ~ as.Date("2021-09-02"),

      TRUE ~ Date
    )
  )
```

2022Elejas - tas pats, noaminu gada vērtību

``` r
moth_corrected %>%
  filter(
    Year_plus_Site == "2022Elejas",
    Date %in% as.Date(c("2024-05-11", "2022-05-11"))
  )
```

    ## # A tibble: 5 × 6
    ##    Year Site   Year_plus_Site Date       Absdate Diamondback_count
    ##   <dbl> <chr>  <chr>          <date>       <dbl>             <dbl>
    ## 1  2022 Elejas 2022Elejas     2024-05-11   45423                 0
    ## 2  2022 Elejas 2022Elejas     2024-05-11   45423                 0
    ## 3  2022 Elejas 2022Elejas     2024-05-11   45423                 0
    ## 4  2022 Elejas 2022Elejas     2024-05-11   45423                 0
    ## 5  2022 Elejas 2022Elejas     2024-05-11   45423                 0

``` r
 moth_corrected <- moth_corrected %>%
  mutate(
    Date = case_when(
      Year_plus_Site == "2022Elejas" &
        Date == as.Date("2024-05-11") ~ as.Date("2022-05-11"),

      TRUE ~ Date
    )
  )
```

- 2024Dignajas - gārš intervāls. Vai nav izskrītusi viena serija? -
  uzskaite nenotika, labojumu nav

- 2024Varmes - gārš intervāls. Vai nav izskrītusi viena serija? -
  uzskaite nenotika, labojumu nav

- 2025Elejas - ir dati no 2027. gada, esmu pārliecināta, ka tas ir
  ievādes kļūda no 2025. gada - (“jā, tas būs 2025. gads”) - laboju gadu

``` r
moth_corrected %>%
  filter(
    Year_plus_Site == "2025Elejas",
    Date %in% as.Date(c("2027-07-02", "2025-07-02"))
    )
```

    ## # A tibble: 5 × 6
    ##    Year Site   Year_plus_Site Date       Absdate Diamondback_count
    ##   <dbl> <chr>  <chr>          <date>       <dbl>             <dbl>
    ## 1  2025 Elejas 2025Elejas     2027-07-02   46570               100
    ## 2  2025 Elejas 2025Elejas     2027-07-02   46570               105
    ## 3  2025 Elejas 2025Elejas     2027-07-02   46570               113
    ## 4  2025 Elejas 2025Elejas     2027-07-02   46570               121
    ## 5  2025 Elejas 2025Elejas     2027-07-02   46570               130

``` r
 moth_corrected <- moth_corrected %>%
  mutate(
    Date = case_when(
      Year_plus_Site == "2025Elejas" &
        Date == as.Date("2027-07-02") ~ as.Date("2025-07-02"),

      TRUE ~ Date
    )
  )
```

- 2025Pures - gārš intervāls. Vai nav izskrītusi viena serija? -
  labojummi nav nepieciešami

- 2025Varmes - 2022-07-16 - izskātas, ka ievades kļūda no 2025. gada;

``` r
moth_corrected %>%
  filter(
    Year_plus_Site == "2025Varmes",
    Date %in% as.Date(c("2022-07-16", "2025-07-16"))
    )
```

    ## # A tibble: 5 × 6
    ##    Year Site   Year_plus_Site Date       Absdate Diamondback_count
    ##   <dbl> <chr>  <chr>          <date>       <dbl>             <dbl>
    ## 1  2025 Varmes 2025Varmes     2022-07-16   44758                60
    ## 2  2025 Varmes 2025Varmes     2025-07-16   45854                19
    ## 3  2025 Varmes 2025Varmes     2025-07-16   45854                35
    ## 4  2025 Varmes 2025Varmes     2025-07-16   45854                 8
    ## 5  2025 Varmes 2025Varmes     2025-07-16   45854                22

``` r
 moth_corrected <- moth_corrected %>%
  mutate(
    Date = case_when(
      Year_plus_Site == "2025Varmes" &
        Date == as.Date("2022-07-16") ~ as.Date("2025-07-16"),

      TRUE ~ Date
    )
  )
```

2025Varmes laboju 07.16.2022 uz 07.16.2025

``` r
moth_corrected %>%
  filter(
    Year_plus_Site == "2025Varmes",
    Date %in% as.Date(c("2027-07-16", "2025-07-16"))
    )
```

    ## # A tibble: 5 × 6
    ##    Year Site   Year_plus_Site Date       Absdate Diamondback_count
    ##   <dbl> <chr>  <chr>          <date>       <dbl>             <dbl>
    ## 1  2025 Varmes 2025Varmes     2025-07-16   44758                60
    ## 2  2025 Varmes 2025Varmes     2025-07-16   45854                19
    ## 3  2025 Varmes 2025Varmes     2025-07-16   45854                35
    ## 4  2025 Varmes 2025Varmes     2025-07-16   45854                 8
    ## 5  2025 Varmes 2025Varmes     2025-07-16   45854                22

``` r
 moth_corrected <- moth_corrected %>%
  mutate(
    Date = case_when(
      Year_plus_Site == "2025Varmes" &
        Date == as.Date("2027-07-02") ~ as.Date("2025-07-02"),

      TRUE ~ Date
    )
  )
```

Gārš intervāls starp 2025-05-28 un 2025-06-18. Vai nav izskrītusi viena
serija?; Novērojumiem no 11.06.2025 izskatās, ka menesis un datums
samainīti vietām - novēmbris nevar būt;

``` r
moth_corrected %>%
  filter(
    Year_plus_Site == "2025Varmes",
    Date %in% as.Date(c("2025-11-06", "2025-06-11"))
    )
```

    ## # A tibble: 5 × 6
    ##    Year Site   Year_plus_Site Date       Absdate Diamondback_count
    ##   <dbl> <chr>  <chr>          <date>       <dbl>             <dbl>
    ## 1  2025 Varmes 2025Varmes     2025-11-06   45967                72
    ## 2  2025 Varmes 2025Varmes     2025-11-06   45967                76
    ## 3  2025 Varmes 2025Varmes     2025-11-06   45967                92
    ## 4  2025 Varmes 2025Varmes     2025-11-06   45967               171
    ## 5  2025 Varmes 2025Varmes     2025-11-06   45967               184

``` r
 moth_corrected <- moth_corrected %>%
  mutate(
    Date = case_when(
      Year_plus_Site == "2025Varmes" &
        Date == as.Date("2025-11-06") ~ as.Date("2025-06-11"),

      TRUE ~ Date
    )
  )
```

dati no 2026. gada, visticamāk arī ir 2025. - Jāizlaboi gadu un datumu

``` r
moth_corrected %>%
  filter(
    Year_plus_Site == "2025Varmes",
    Date %in% as.Date(c("2026-06-04", "2025-06-04"))
    )
```

    ## # A tibble: 5 × 6
    ##    Year Site   Year_plus_Site Date       Absdate Diamondback_count
    ##   <dbl> <chr>  <chr>          <date>       <dbl>             <dbl>
    ## 1  2025 Varmes 2025Varmes     2026-06-04   46177                70
    ## 2  2025 Varmes 2025Varmes     2026-06-04   46177                85
    ## 3  2025 Varmes 2025Varmes     2026-06-04   46177               112
    ## 4  2025 Varmes 2025Varmes     2026-06-04   46177               123
    ## 5  2025 Varmes 2025Varmes     2026-06-04   46177               131

``` r
 moth_corrected <- moth_corrected %>%
  mutate(
    Date = case_when(
      Year_plus_Site == "2025Varmes" &
        Date == as.Date("2026-06-04") ~ as.Date("2025-06-04"),

      TRUE ~ Date
    )
  )
```

Tagad pārbaudu vai visur vieta un gads sakrīt ar ierakstu ViteGads:

``` r
moth_corrected %>%
  mutate(
    year_from_id = as.numeric(substr(Year_plus_Site, 1, 4))
  ) %>%
  filter(
    Year != year_from_id
  )
```

    ## # A tibble: 0 × 7
    ## # ℹ 7 variables: Year <dbl>, Site <chr>, Year_plus_Site <chr>, Date <date>,
    ## #   Absdate <dbl>, Diamondback_count <dbl>, year_from_id <dbl>

``` r
moth_corrected %>%
  mutate(
    site_from_id = substr(Year_plus_Site, 5, nchar(Year_plus_Site))
  ) %>%
  filter(
    Site != site_from_id
  )
```

    ## # A tibble: 0 × 7
    ## # ℹ 7 variables: Year <dbl>, Site <chr>, Year_plus_Site <chr>, Date <date>,
    ## #   Absdate <dbl>, Diamondback_count <dbl>, site_from_id <chr>

Vai vērtība Year sakrīt ar gadu noradītu datumā:

``` r
moth_corrected %>%
  filter(
    lubridate::year(Date) != Year
  )
```

    ## # A tibble: 0 × 6
    ## # ℹ 6 variables: Year <dbl>, Site <chr>, Year_plus_Site <chr>, Date <date>,
    ## #   Absdate <dbl>, Diamondback_count <dbl>

# temperatūras dati QC

``` r
colSums(is.na(meteo_qc))
```

    ##           Site           Year Year_plus_Site           Date        Absdate 
    ##              1              1              1              1              1 
    ##       Taverage       DDabove0 
    ##             84             84

Pievienoju previous date un aprēķinu intervālu

``` r
# izveido objektu
meteo_date_check <- meteo_qc %>%
  arrange(Year_plus_Site, Date) %>%
  group_by(Year_plus_Site) %>%
  mutate(
    previous_date = lag(Date),
    interval_days = as.numeric(Date - previous_date)
  ) %>%
  ungroup()
```

``` r
table(
  meteo_date_check$interval_days,
  useNA = "ifany"
)
```

    ## 
    ##     1  <NA> 
    ## 19667    56

``` r
meteo_date_check %>%
  filter(
    !is.na(interval_days),
    interval_days != 1
  ) %>%
  select(
    Year_plus_Site,
    Site,
    Year,
    previous_date,
    Date,
    interval_days
  ) %>%
  print(n = Inf)
```

    ## # A tibble: 0 × 6
    ## # ℹ 6 variables: Year_plus_Site <chr>, Site <chr>, Year <dbl>,
    ## #   previous_date <date>, Date <date>, interval_days <dbl>

``` r
summary(meteo_qc$Taverage)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ## -24.542   1.275   7.517   7.853  15.463  28.246      84

``` r
meteo_qc %>%
  filter(is.na(Taverage)) %>%
  select(
    Year_plus_Site,
    Year,
    Site,
    Date,
    Taverage,
    DDabove0
  ) %>%
  print(n = Inf)
```

    ## # A tibble: 84 × 6
    ##    Year_plus_Site  Year Site     Date       Taverage DDabove0
    ##    <chr>          <dbl> <chr>    <date>        <dbl>    <dbl>
    ##  1 2021Daukstu     2021 Daukstu  2021-09-25       NA       NA
    ##  2 2021Daukstu     2021 Daukstu  2021-09-26       NA       NA
    ##  3 2021Daukstu     2021 Daukstu  2021-09-27       NA       NA
    ##  4 2021Daukstu     2021 Daukstu  2021-09-28       NA       NA
    ##  5 2021Tirzas      2021 Tirzas   2021-09-25       NA       NA
    ##  6 2021Tirzas      2021 Tirzas   2021-09-26       NA       NA
    ##  7 2021Tirzas      2021 Tirzas   2021-09-27       NA       NA
    ##  8 2021Tirzas      2021 Tirzas   2021-09-28       NA       NA
    ##  9 2021Dignajas    2021 Dignajas 2021-02-06       NA       NA
    ## 10 2021Dignajas    2021 Dignajas 2021-09-27       NA       NA
    ## 11 2022Dignajas    2022 Dignajas 2022-03-27       NA       NA
    ## 12 2024Allazu      2024 Allazu   2024-02-11       NA       NA
    ## 13 2024Allazu      2024 Allazu   2024-02-12       NA       NA
    ## 14 2024Allazu      2024 Allazu   2024-02-13       NA       NA
    ## 15 2024Allazu      2024 Allazu   2024-02-14       NA       NA
    ## 16 2024Allazu      2024 Allazu   2024-02-15       NA       NA
    ## 17 2024Allazu      2024 Allazu   2024-02-16       NA       NA
    ## 18 2024Allazu      2024 Allazu   2024-02-17       NA       NA
    ## 19 2024Allazu      2024 Allazu   2024-02-18       NA       NA
    ## 20 2024Allazu      2024 Allazu   2024-02-19       NA       NA
    ## 21 2024Allazu      2024 Allazu   2024-02-20       NA       NA
    ## 22 2024Allazu      2024 Allazu   2024-02-21       NA       NA
    ## 23 2024Allazu      2024 Allazu   2024-02-22       NA       NA
    ## 24 2024Allazu      2024 Allazu   2024-02-23       NA       NA
    ## 25 2024Allazu      2024 Allazu   2024-02-24       NA       NA
    ## 26 2024Allazu      2024 Allazu   2024-02-25       NA       NA
    ## 27 2024Allazu      2024 Allazu   2024-02-26       NA       NA
    ## 28 2024Allazu      2024 Allazu   2024-02-27       NA       NA
    ## 29 2024Allazu      2024 Allazu   2024-02-28       NA       NA
    ## 30 2024Allazu      2024 Allazu   2024-02-29       NA       NA
    ## 31 2024Allazu      2024 Allazu   2024-03-01       NA       NA
    ## 32 2024Allazu      2024 Allazu   2024-03-02       NA       NA
    ## 33 2024Allazu      2024 Allazu   2024-03-03       NA       NA
    ## 34 2024Allazu      2024 Allazu   2024-03-04       NA       NA
    ## 35 2024Allazu      2024 Allazu   2024-03-05       NA       NA
    ## 36 2024Allazu      2024 Allazu   2024-03-06       NA       NA
    ## 37 2024Allazu      2024 Allazu   2024-03-07       NA       NA
    ## 38 2024Allazu      2024 Allazu   2024-03-08       NA       NA
    ## 39 2024Allazu      2024 Allazu   2024-03-09       NA       NA
    ## 40 2024Allazu      2024 Allazu   2024-03-10       NA       NA
    ## 41 2024Allazu      2024 Allazu   2024-03-11       NA       NA
    ## 42 2024Allazu      2024 Allazu   2024-03-12       NA       NA
    ## 43 2024Allazu      2024 Allazu   2024-03-13       NA       NA
    ## 44 2024Allazu      2024 Allazu   2024-03-14       NA       NA
    ## 45 2024Allazu      2024 Allazu   2024-03-15       NA       NA
    ## 46 2024Allazu      2024 Allazu   2024-03-16       NA       NA
    ## 47 2024Allazu      2024 Allazu   2024-03-17       NA       NA
    ## 48 2024Ligatnes    2024 Ligatnes 2024-02-11       NA       NA
    ## 49 2024Ligatnes    2024 Ligatnes 2024-02-12       NA       NA
    ## 50 2024Ligatnes    2024 Ligatnes 2024-02-13       NA       NA
    ## 51 2024Ligatnes    2024 Ligatnes 2024-02-14       NA       NA
    ## 52 2024Ligatnes    2024 Ligatnes 2024-02-15       NA       NA
    ## 53 2024Ligatnes    2024 Ligatnes 2024-02-16       NA       NA
    ## 54 2024Ligatnes    2024 Ligatnes 2024-02-17       NA       NA
    ## 55 2024Ligatnes    2024 Ligatnes 2024-02-18       NA       NA
    ## 56 2024Ligatnes    2024 Ligatnes 2024-02-19       NA       NA
    ## 57 2024Ligatnes    2024 Ligatnes 2024-02-20       NA       NA
    ## 58 2024Ligatnes    2024 Ligatnes 2024-02-21       NA       NA
    ## 59 2024Ligatnes    2024 Ligatnes 2024-02-22       NA       NA
    ## 60 2024Ligatnes    2024 Ligatnes 2024-02-23       NA       NA
    ## 61 2024Ligatnes    2024 Ligatnes 2024-02-24       NA       NA
    ## 62 2024Ligatnes    2024 Ligatnes 2024-02-25       NA       NA
    ## 63 2024Ligatnes    2024 Ligatnes 2024-02-26       NA       NA
    ## 64 2024Ligatnes    2024 Ligatnes 2024-02-27       NA       NA
    ## 65 2024Ligatnes    2024 Ligatnes 2024-02-28       NA       NA
    ## 66 2024Ligatnes    2024 Ligatnes 2024-02-29       NA       NA
    ## 67 2024Ligatnes    2024 Ligatnes 2024-03-01       NA       NA
    ## 68 2024Ligatnes    2024 Ligatnes 2024-03-02       NA       NA
    ## 69 2024Ligatnes    2024 Ligatnes 2024-03-03       NA       NA
    ## 70 2024Ligatnes    2024 Ligatnes 2024-03-04       NA       NA
    ## 71 2024Ligatnes    2024 Ligatnes 2024-03-05       NA       NA
    ## 72 2024Ligatnes    2024 Ligatnes 2024-03-06       NA       NA
    ## 73 2024Ligatnes    2024 Ligatnes 2024-03-07       NA       NA
    ## 74 2024Ligatnes    2024 Ligatnes 2024-03-08       NA       NA
    ## 75 2024Ligatnes    2024 Ligatnes 2024-03-09       NA       NA
    ## 76 2024Ligatnes    2024 Ligatnes 2024-03-10       NA       NA
    ## 77 2024Ligatnes    2024 Ligatnes 2024-03-11       NA       NA
    ## 78 2024Ligatnes    2024 Ligatnes 2024-03-12       NA       NA
    ## 79 2024Ligatnes    2024 Ligatnes 2024-03-13       NA       NA
    ## 80 2024Ligatnes    2024 Ligatnes 2024-03-14       NA       NA
    ## 81 2024Ligatnes    2024 Ligatnes 2024-03-15       NA       NA
    ## 82 2024Ligatnes    2024 Ligatnes 2024-03-16       NA       NA
    ## 83 2024Ligatnes    2024 Ligatnes 2024-03-17       NA       NA
    ## 84 <NA>              NA <NA>     NA               NA       NA

Ir peridoi ar trūkstošiem datiem.

``` r
moth_ranges <- moth_corrected %>%
  group_by(
    Year_plus_Site,
    Year,
    Site
  ) %>%
  summarise(
    moth_first_date = min(Date, na.rm = TRUE),
    moth_last_date = max(Date, na.rm = TRUE),
    .groups = "drop"
  )
```

Tikai trūsktošas temperatūras:

``` r
meteo_missing <- meteo_qc %>%
  filter(
    is.na(Taverage),
    !is.na(Date)
  )
```

Pievienojju kožu novērojumu periodus:

``` r
meteo_missing_check <- meteo_missing %>%
  left_join(
    moth_ranges,
    by = c(
      "Year_plus_Site",
      "Year",
      "Site"
    )
  ) %>%
  mutate(
    during_moth_period =
      Date >= moth_first_date &
      Date <= moth_last_date
  )
```

``` r
meteo_missing_check %>%
  select(
    Year_plus_Site,
    Date,
    moth_first_date,
    moth_last_date,
    during_moth_period
  ) %>%
  print(n = Inf)
```

    ## # A tibble: 83 × 5
    ##    Year_plus_Site Date       moth_first_date moth_last_date during_moth_period
    ##    <chr>          <date>     <date>          <date>         <lgl>             
    ##  1 2021Daukstu    2021-09-25 2021-05-27      2021-08-26     FALSE             
    ##  2 2021Daukstu    2021-09-26 2021-05-27      2021-08-26     FALSE             
    ##  3 2021Daukstu    2021-09-27 2021-05-27      2021-08-26     FALSE             
    ##  4 2021Daukstu    2021-09-28 2021-05-27      2021-08-26     FALSE             
    ##  5 2021Tirzas     2021-09-25 NA              NA             NA                
    ##  6 2021Tirzas     2021-09-26 NA              NA             NA                
    ##  7 2021Tirzas     2021-09-27 NA              NA             NA                
    ##  8 2021Tirzas     2021-09-28 NA              NA             NA                
    ##  9 2021Dignajas   2021-02-06 2021-06-09      2021-10-12     FALSE             
    ## 10 2021Dignajas   2021-09-27 2021-06-09      2021-10-12     TRUE              
    ## 11 2022Dignajas   2022-03-27 2022-05-20      2022-09-27     FALSE             
    ## 12 2024Allazu     2024-02-11 NA              NA             NA                
    ## 13 2024Allazu     2024-02-12 NA              NA             NA                
    ## 14 2024Allazu     2024-02-13 NA              NA             NA                
    ## 15 2024Allazu     2024-02-14 NA              NA             NA                
    ## 16 2024Allazu     2024-02-15 NA              NA             NA                
    ## 17 2024Allazu     2024-02-16 NA              NA             NA                
    ## 18 2024Allazu     2024-02-17 NA              NA             NA                
    ## 19 2024Allazu     2024-02-18 NA              NA             NA                
    ## 20 2024Allazu     2024-02-19 NA              NA             NA                
    ## 21 2024Allazu     2024-02-20 NA              NA             NA                
    ## 22 2024Allazu     2024-02-21 NA              NA             NA                
    ## 23 2024Allazu     2024-02-22 NA              NA             NA                
    ## 24 2024Allazu     2024-02-23 NA              NA             NA                
    ## 25 2024Allazu     2024-02-24 NA              NA             NA                
    ## 26 2024Allazu     2024-02-25 NA              NA             NA                
    ## 27 2024Allazu     2024-02-26 NA              NA             NA                
    ## 28 2024Allazu     2024-02-27 NA              NA             NA                
    ## 29 2024Allazu     2024-02-28 NA              NA             NA                
    ## 30 2024Allazu     2024-02-29 NA              NA             NA                
    ## 31 2024Allazu     2024-03-01 NA              NA             NA                
    ## 32 2024Allazu     2024-03-02 NA              NA             NA                
    ## 33 2024Allazu     2024-03-03 NA              NA             NA                
    ## 34 2024Allazu     2024-03-04 NA              NA             NA                
    ## 35 2024Allazu     2024-03-05 NA              NA             NA                
    ## 36 2024Allazu     2024-03-06 NA              NA             NA                
    ## 37 2024Allazu     2024-03-07 NA              NA             NA                
    ## 38 2024Allazu     2024-03-08 NA              NA             NA                
    ## 39 2024Allazu     2024-03-09 NA              NA             NA                
    ## 40 2024Allazu     2024-03-10 NA              NA             NA                
    ## 41 2024Allazu     2024-03-11 NA              NA             NA                
    ## 42 2024Allazu     2024-03-12 NA              NA             NA                
    ## 43 2024Allazu     2024-03-13 NA              NA             NA                
    ## 44 2024Allazu     2024-03-14 NA              NA             NA                
    ## 45 2024Allazu     2024-03-15 NA              NA             NA                
    ## 46 2024Allazu     2024-03-16 NA              NA             NA                
    ## 47 2024Allazu     2024-03-17 NA              NA             NA                
    ## 48 2024Ligatnes   2024-02-11 2024-05-29      2024-09-03     FALSE             
    ## 49 2024Ligatnes   2024-02-12 2024-05-29      2024-09-03     FALSE             
    ## 50 2024Ligatnes   2024-02-13 2024-05-29      2024-09-03     FALSE             
    ## 51 2024Ligatnes   2024-02-14 2024-05-29      2024-09-03     FALSE             
    ## 52 2024Ligatnes   2024-02-15 2024-05-29      2024-09-03     FALSE             
    ## 53 2024Ligatnes   2024-02-16 2024-05-29      2024-09-03     FALSE             
    ## 54 2024Ligatnes   2024-02-17 2024-05-29      2024-09-03     FALSE             
    ## 55 2024Ligatnes   2024-02-18 2024-05-29      2024-09-03     FALSE             
    ## 56 2024Ligatnes   2024-02-19 2024-05-29      2024-09-03     FALSE             
    ## 57 2024Ligatnes   2024-02-20 2024-05-29      2024-09-03     FALSE             
    ## 58 2024Ligatnes   2024-02-21 2024-05-29      2024-09-03     FALSE             
    ## 59 2024Ligatnes   2024-02-22 2024-05-29      2024-09-03     FALSE             
    ## 60 2024Ligatnes   2024-02-23 2024-05-29      2024-09-03     FALSE             
    ## 61 2024Ligatnes   2024-02-24 2024-05-29      2024-09-03     FALSE             
    ## 62 2024Ligatnes   2024-02-25 2024-05-29      2024-09-03     FALSE             
    ## 63 2024Ligatnes   2024-02-26 2024-05-29      2024-09-03     FALSE             
    ## 64 2024Ligatnes   2024-02-27 2024-05-29      2024-09-03     FALSE             
    ## 65 2024Ligatnes   2024-02-28 2024-05-29      2024-09-03     FALSE             
    ## 66 2024Ligatnes   2024-02-29 2024-05-29      2024-09-03     FALSE             
    ## 67 2024Ligatnes   2024-03-01 2024-05-29      2024-09-03     FALSE             
    ## 68 2024Ligatnes   2024-03-02 2024-05-29      2024-09-03     FALSE             
    ## 69 2024Ligatnes   2024-03-03 2024-05-29      2024-09-03     FALSE             
    ## 70 2024Ligatnes   2024-03-04 2024-05-29      2024-09-03     FALSE             
    ## 71 2024Ligatnes   2024-03-05 2024-05-29      2024-09-03     FALSE             
    ## 72 2024Ligatnes   2024-03-06 2024-05-29      2024-09-03     FALSE             
    ## 73 2024Ligatnes   2024-03-07 2024-05-29      2024-09-03     FALSE             
    ## 74 2024Ligatnes   2024-03-08 2024-05-29      2024-09-03     FALSE             
    ## 75 2024Ligatnes   2024-03-09 2024-05-29      2024-09-03     FALSE             
    ## 76 2024Ligatnes   2024-03-10 2024-05-29      2024-09-03     FALSE             
    ## 77 2024Ligatnes   2024-03-11 2024-05-29      2024-09-03     FALSE             
    ## 78 2024Ligatnes   2024-03-12 2024-05-29      2024-09-03     FALSE             
    ## 79 2024Ligatnes   2024-03-13 2024-05-29      2024-09-03     FALSE             
    ## 80 2024Ligatnes   2024-03-14 2024-05-29      2024-09-03     FALSE             
    ## 81 2024Ligatnes   2024-03-15 2024-05-29      2024-09-03     FALSE             
    ## 82 2024Ligatnes   2024-03-16 2024-05-29      2024-09-03     FALSE             
    ## 83 2024Ligatnes   2024-03-17 2024-05-29      2024-09-03     FALSE

``` r
meteo_missing_check %>%
  filter(during_moth_period == TRUE)
```

    ## # A tibble: 1 × 10
    ##   Site      Year Year_plus_Site Date       Absdate Taverage DDabove0
    ##   <chr>    <dbl> <chr>          <date>       <dbl>    <dbl>    <dbl>
    ## 1 Dignajas  2021 2021Dignajas   2021-09-27   44466       NA       NA
    ## # ℹ 3 more variables: moth_first_date <date>, moth_last_date <date>,
    ## #   during_moth_period <lgl>

\*\* Beigas ir tikai viena trukstošā diena, aks sakrīt ar kožu
novērošans periodiem\*\*.

Apskatos kādi dati ir tajā periodā:

``` r
meteo_qc %>%
  filter(
    Year_plus_Site == "2021Dignajas",
    Date >= as.Date("2021-09-24"),
    Date <= as.Date("2021-09-30")
  ) %>%
  select(
    Date,
    Taverage,
    DDabove0
  )
```

    ## # A tibble: 7 × 3
    ##   Date       Taverage DDabove0
    ##   <date>        <dbl>    <dbl>
    ## 1 2021-09-24    10.1     10.1 
    ## 2 2021-09-25    11.3     11.3 
    ## 3 2021-09-26     9.21     9.21
    ## 4 2021-09-27    NA       NA   
    ## 5 2021-09-28     8.86     8.86
    ## 6 2021-09-29     9.06     9.06
    ## 7 2021-09-30     7.80     7.80

# Confirmed / justified meteorological corrections

Blakus dienās bija ļoti lidzīga temperatūra. Labs gadījums lineārai
interpolacijai.

Pārreiķinu vidsdus DDabove0 Un izņemu tukšo rindu:

``` r
meteo_corrected <- meteo_qc %>%
  
  # Remove completely empty row
  filter(!is.na(Date)) %>%
  
  # Fill missing temperature for 2021 Dignajas
  mutate(
    Taverage = case_when(
      Year_plus_Site == "2021Dignajas" &
        Date == as.Date("2021-09-27") ~ 9.035,
      
      TRUE ~ Taverage
    ),
    
    # Recalculate DDabove0 from corrected Taverage
    DDabove0 = pmax(Taverage, 0)
  )
```

Pēdeja parbaude:

``` r
colSums(is.na(meteo_corrected))
```

    ##           Site           Year Year_plus_Site           Date        Absdate 
    ##              0              0              0              0              0 
    ##       Taverage       DDabove0 
    ##             82             82

``` r
meteo_corrected %>%
  filter(
    is.na(Taverage)
  ) %>%
  left_join(
    moth_ranges,
    by = c(
      "Year_plus_Site",
      "Year",
      "Site"
    )
  ) %>%
  mutate(
    during_moth_period =
      Date >= moth_first_date &
      Date <= moth_last_date
  ) %>%
  filter(during_moth_period == TRUE)
```

    ## # A tibble: 0 × 10
    ## # ℹ 10 variables: Site <chr>, Year <dbl>, Year_plus_Site <chr>, Date <date>,
    ## #   Absdate <dbl>, Taverage <dbl>, DDabove0 <dbl>, moth_first_date <date>,
    ## #   moth_last_date <date>, during_moth_period <lgl>

# 7. Final QC checks

``` r
# Moth data: no missing critical identifiers
colSums(is.na(
  moth_corrected %>%
    select(Year, Site, Year_plus_Site, Date, Diamondback_count)
))
```

    ##              Year              Site    Year_plus_Site              Date 
    ##                 0                 0                 0                 0 
    ## Diamondback_count 
    ##                 0

``` r
# Meteo data: remaining missing temperatures
colSums(is.na(meteo_corrected))
```

    ##           Site           Year Year_plus_Site           Date        Absdate 
    ##              0              0              0              0              0 
    ##       Taverage       DDabove0 
    ##             82             82

``` r
moth_corrected %>%
  filter(year(Date) != Year)
```

    ## # A tibble: 0 × 6
    ## # ℹ 6 variables: Year <dbl>, Site <chr>, Year_plus_Site <chr>, Date <date>,
    ## #   Absdate <dbl>, Diamondback_count <dbl>

``` r
meteo_corrected %>%
  filter(is.na(Date))
```

    ## # A tibble: 0 × 7
    ## # ℹ 7 variables: Site <chr>, Year <dbl>, Year_plus_Site <chr>, Date <date>,
    ## #   Absdate <dbl>, Taverage <dbl>, DDabove0 <dbl>

``` r
moth_ranges <- moth_corrected %>%
  group_by(
    Year_plus_Site,
    Year,
    Site
  ) %>%
  summarise(
    moth_first_date = min(Date, na.rm = TRUE),
    moth_last_date = max(Date, na.rm = TRUE),
    .groups = "drop"
  )

meteo_corrected %>%
  filter(is.na(Taverage)) %>%
  left_join(
    moth_ranges,
    by = c(
      "Year_plus_Site",
      "Year",
      "Site"
    )
  ) %>%
  mutate(
    during_moth_period =
      Date >= moth_first_date &
      Date <= moth_last_date
  ) %>%
  filter(during_moth_period == TRUE)
```

    ## # A tibble: 0 × 10
    ## # ℹ 10 variables: Site <chr>, Year <dbl>, Year_plus_Site <chr>, Date <date>,
    ## #   Absdate <dbl>, Taverage <dbl>, DDabove0 <dbl>, moth_first_date <date>,
    ## #   moth_last_date <date>, during_moth_period <lgl>

# Rezultātu faili 8. Save corrected datasets

``` r
saveRDS(
  moth_corrected,
  "../data/processed/moth_corrected.rds"
)

saveRDS(
  meteo_corrected,
  "../data/processed/meteo_corrected.rds"
)
```
