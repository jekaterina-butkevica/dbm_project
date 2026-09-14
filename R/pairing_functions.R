# ============================================================
# pairing_functions.R
# Functions for generating candidate DBM peak pairs
# ============================================================


# Šeit cenšos vienas Year_plus_Site sērijas ietvaros izveidot visus iespējamos pīķu pārus
generate_compatible_pair_sets <- function(data) {

  data <- data %>%
    arrange(
      peak_1,
      peak_2
    ) %>%
    mutate(
      pair_id = row_number()
    )

  n_pairs <- nrow(data)

  if (n_pairs == 0) {
    return(
      tibble(
        set_id = integer(),
        n_pairs = integer(),
        selected_pair_ids = list()
      )
    )
  }
  
  all_sets <- lapply(
    seq_len(n_pairs),
    function(set_size) {
      
      combinations <- combn(
        data$pair_id,
        set_size,
        simplify = FALSE
      )
      
      valid_combinations <- lapply(
        combinations,
        function(ids) {
          
          selected <- data %>%
            filter(
              pair_id %in% ids
            )
          
          valid_start <-
            !anyDuplicated(selected$peak_1)
          
          valid_end <-
            !anyDuplicated(selected$peak_2)
          
          if (
            valid_start &&
            valid_end
          ) {
            return(ids)
          }
          
          NULL
        }
      )
      
      valid_combinations[
        !vapply(
          valid_combinations,
          is.null,
          logical(1)
        )
      ]
    }
  ) %>%
    unlist(
      recursive = FALSE
    )
  
  tibble(
    set_id = seq_along(all_sets),
    
    n_pairs = vapply(
      all_sets,
      length,
      integer(1)
    ),
    
    selected_pair_ids = all_sets
  )
}