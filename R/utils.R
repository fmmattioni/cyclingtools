#' Get combinations
#'
#' Derives all the possible combinations of the given time-to-exhaustion trials.
#' This function is mainly for internal use.
#'
#' @param .data A data frame containing a power output column and their respective time-to-exhaustion.
#' @param power_output_column The name of the power output column. This value has to be in watts. Default to `"PO"`.
#' @param time_to_exhaustion_column The name of the time-to-exhaustion column. This value has to be in seconds. Default to `"TTE"`.
#'
#' @importFrom utils combn tail
#' @importFrom rlang :=
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
get_combinations <- function(
  .data,
  power_output_column,
  time_to_exhaustion_column
) {

  # Get PO and TTE columns as vectors
  vector_po <- .data[[power_output_column]]
  vector_tte <- .data[[time_to_exhaustion_column]]

  # Power Output ------------------------------------------------------------

  combinations_vec_po <- sapply(sort(seq_along(unlist(vector_po)), decreasing = TRUE),
                                function(y) combn(unlist(vector_po), y, simplify = FALSE))

  # Drop the combinations using only one trial
  drop_this <- tail(combinations_vec_po, n = 1) %>%
    match(combinations_vec_po)

  combinations_vec_po[[drop_this]] <- NULL

  combinations_vec_po <- unlist(combinations_vec_po, recursive = FALSE) %>%
    tibble::enframe(name = NULL, value = power_output_column) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      !!power_output_column :=
        tibble::enframe(!!rlang::sym(power_output_column), name = NULL, value = power_output_column) %>% list()
    ) %>%
    dplyr::ungroup()

  # Time-to-exhaustion -----------------------------------------------------------

  combinations_vec_tte <- sapply(sort(seq_along(unlist(vector_tte)), decreasing = TRUE),
                                 function(y) combn(unlist(vector_tte), y, simplify=FALSE))

  # Drop the combinations using only one trial
  drop_this <- tail(combinations_vec_tte, n = 1) %>%
    match(combinations_vec_tte)

  combinations_vec_tte[[drop_this]] <- NULL

  combinations_vec_tte <- unlist(combinations_vec_tte, recursive = FALSE) %>%
    tibble::enframe(name = NULL, value = time_to_exhaustion_column) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      !!time_to_exhaustion_column :=
        tibble::enframe(!!rlang::sym(time_to_exhaustion_column), name = NULL, value = time_to_exhaustion_column) %>% list()
    ) %>%
    dplyr::ungroup()

  out <- dplyr::bind_cols(combinations_vec_po, combinations_vec_tte)

  out
}

#' Get index of combinations
#'
#' This function is used in combination with `get_combinations()` and it derives the indexes
#' of the time-to-exhaustion trials in the different combinations.
#'
#' @param .data A data frame containing a power output column and their respective time-to-exhaustion.
#' @param power_output_column The name of the power output column. This value has to be in watts. Default to `"PO"`.
#' @param time_to_exhaustion_column The name of the time-to-exhaustion column. This value has to be in seconds. Default to `"TTE"`.
#' @param combinations The data derived from `get_combinations()`.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' combinations <- get_combinations(demo_critical_power, "PO", "TTE")
#'
#' results_index <- get_indexes(demo_critical_power, "PO", "TTE", combinations)
#'
#' results_index
get_indexes <- function(
  .data,
  power_output_column,
  time_to_exhaustion_column,
  combinations
) {

  out <- combinations %>%
    dplyr::rowwise() %>%
    dplyr::mutate(data = dplyr::bind_cols(!!rlang::sym(power_output_column), !!rlang::sym(time_to_exhaustion_column)) %>% list()) %>%
    dplyr::select(data) %>%
    dplyr::mutate(
      raw_data = list({{ .data }}),
      index = match(data[[power_output_column]], raw_data[[power_output_column]]) %>% as.character() %>% list(),
      index = paste(index, collapse = ","),
      index = glue::glue("[{index}]")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(index, data)

  out
}
