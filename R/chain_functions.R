# chain_functions.R
# Functions for generating compatible generation chains


generate_two_generation_chains <- function(data) {
  
  first_generation <- data %>%
    select(
      Year_plus_Site,
      Tbase,
      peak_1_a = peak_1,
      peak_2_a = peak_2,
      start_date_a = peak_1_date,
      end_date_a = peak_2_date,
      generation_days_a = generation_days,
      degree_days_a = degree_days,
      pair_strength_a = pair_peak_strength,
      pair_edge_a = pair_edge_support
    )
  
  second_generation <- data %>%
    select(
      Year_plus_Site,
      Tbase,
      peak_1_b = peak_1,
      peak_2_b = peak_2,
      start_date_b = peak_1_date,
      end_date_b = peak_2_date,
      generation_days_b = generation_days,
      degree_days_b = degree_days,
      pair_strength_b = pair_peak_strength,
      pair_edge_b = pair_edge_support
    )
  
  chains <- first_generation %>%
    inner_join(
      second_generation,
      by = c(
        "Year_plus_Site",
        "Tbase",
        "peak_2_a" = "peak_1_b"
      ),
      relationship = "many-to-many"
    ) %>%
    mutate(
      dd_difference = abs(
        degree_days_b - degree_days_a
      ),
      
      mean_degree_days = (
        degree_days_a + degree_days_b
      ) / 2,
      
      relative_dd_difference =
        dd_difference / mean_degree_days,
      
      chain_peak_strength = pmin(
        pair_strength_a,
        pair_strength_b
      ),
      
      chain_edge_support = pmin(
        pair_edge_a,
        pair_edge_b
      )
    )
  
  return(chains)
}
