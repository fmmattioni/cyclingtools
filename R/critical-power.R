#' Critical Power
#'
#' @description
#' Performs critical power estimations based on the methods chosen.
#'
#' @param .data A data frame containing a power output column and their respective time-to-exhaustion.
#' @param power_output_column The name of the power output column. This value has to be in watts. Default to `"PO"`.
#' @param time_to_exhaustion_column The name of the time-to-exhaustion column. This value has to be in seconds. Default to `"TTE"`.
#' @param method The method for estimating critical power. It can be one or multiple methods. Default to `c("3-hyp", "2-hyp", "linear", "1/time")`.
#' @param plot A boolean indicating whether to produce a plot from the critical power estimation. Default to `TRUE`.
#' @param all_combinations A boolean indicating whether to perform the critical power estimation from all the possible combinations of time-to-exhaustion trials provided. Please, see 'Details' for more information. Default to `FALSE`.
#' @param power_in_y_axis A boolean to indicate whether to plot the Power Output in the y-axis in the hyperbolic methods. Default to `FALSE`.
#'
#' @details
#' Please, note that estimations of critical power are highly influenced by the range
#' of time-to-exhaustion trials performed, and the chosen critical power model. Please, see Mattioni Maturana et al. (2018).
#'
#' **A note on the R2 value**: Please, note that for the hyperbolic fits (CP 3-hyp and CP 2-hyp) this value is actually a pseudo r-squared, as they are not linear models.
#'
#' ## CP 3-hyp
#'
#' The 3-parameter hyperbolic model is calculated as the following (Morton, 1996):
#'
#' \loadmathjax
#' \mjdeqn{t=\frac{W'}{PO-CP}+\frac{W'}{CP-P_{max}}}{t=W'/(PO-CP) + W'/(CP-Pmax)}
#'
#' ## CP 2-hyp
#'
#' The 2-parameter hyperbolic model is calculated as the following (Hill, 1993):
#'
#' \mjdeqn{t=\frac{W'}{PO-CP}}{t=W'/(PO-CP)}
#'
#' ## CP linear
#'
#' The linear model is calculated as the following (Moritani et al. 1981):
#'
#' \mjdeqn{W_{lim}=W'+CP\cdot t}{Wlim=W'+CP*t}
#'
#' ## CP 1/time
#'
#' The 1/time linear model is calculated as the following (Whipp et al. 1982):
#'
#' \mjdeqn{PO=W'\cdot \frac{1}{t}+CP}{PO=W'*(1/t)+CP}
#'
#' ## All combinations
#'
#' When the argument `all_combinations = TRUE`, the function takes the given data and finds all the
#' possible combinations of the trials given. For example, if 5 trials are provided (i.e., your data contains 5 rows),
#' critical power will be fitted with all the 5 trials, then all the possible combinations using only 4 trials, and so on.
#' This feature was inspired on our paper Mattioni Maturana et al. (2018).
#'
#' When all the combinations are fitted, an extra column called "index" appears in the final results.
#' This is the index of the trial number used to fit the model.
#' This corresponds to the row number of your data (e.g., in `[1,3,5]` trials #1, #3, and #5 are used).
#'
#' @references
#' Mattioni Maturana, F., Fontana, F. Y., Pogliaghi, S., Passfield, L., & Murias, J. M. (2018). Critical power: How different protocols and models affect its determination. Journal of Science and Medicine in Sport, 21(7), 742-747.
#'
#' Hugh Morton, R. (1996). A 3-parameter critical power model. Ergonomics, 39(4), 611-619.
#'
#' Hill, D. W. (1993). The critical power concept. Sports medicine, 16(4), 237-254.
#'
#' Moritani, T., NAGATA, A., DEVRIES, H. A., & MURO, M. (1981). Critical power as a measure of physical work capacity and anaerobic threshold. Ergonomics, 24(5), 339-350.
#'
#' Whipp, B. J., Huntsman, D. J., Storer, T. W., Lamarra, N., & Wasserman, K. (1982, January). A constant which determines the duration of tolerance to high-intensity work. In Federation proceedings (Vol. 41, No. 5, pp. 1591-1591). 9650 ROCKVILLE PIKE, BETHESDA, MD 20814-3998: FEDERATION AMER SOC EXP BIOL.
#'
#'
#' @return a [tibble][tibble::tibble-package] containing the following columns:
#' \item{index}{In case `all_combinations = TRUE` this column indicates the row number of the trials chosen for the critical power estimation. They correspond to the row numbers in your data.}
#' \item{method}{The critical power method for that estimation.}
#' \item{data}{The data you provided.}
#' \item{model}{A `nls` or `lm` object. The model used in the critical power fitting.}
#' \item{CP}{Critical power, in watts.}
#' \item{CP SEE}{Standard error of the estimation of critical power, in watts.}
#' \item{W'}{Anaerobic work capacity, in joules.}
#' \item{W' SEE}{Standard error of the estimation of anaerobic work capacity, in joules.}
#' \item{Pmax}{Maximal instantaneous power, in watts (only available in CP 3-hyp).}
#' \item{Pmax SEE}{Standard error of the estimation of maximal instantaneous power, in watts (only available in CP 3-hyp).}
#' \item{R2}{R-squared.}
#' \item{RMSE}{Root mean square error, the units vary depending on the method, as they represent the units of the response (i.e., y-axis): CP3-hyp in seconds, CP2-hyp in seconds, CPlinear in joules, and CP1/time in watts.}
#' \item{plot}{The critical power plot.}
#'
#' @export
#'
#' @examples
#' results <- critical_power(
#'  .data = demo_data,
#'  power_output_column = "PO",
#'  time_to_exhaustion_column = "TTE",
#'  method = c("3-hyp", "2-hyp", "linear", "1/time"),
#'  plot = TRUE,
#'  all_combinations = FALSE
#' )
#'
#' results
critical_power <- function(
  .data,
  power_output_column = "PO",
  time_to_exhaustion_column = "TTE",
  method = c("3-hyp", "2-hyp", "linear", "1/time"),
  plot = TRUE,
  all_combinations = FALSE,
  power_in_y_axis = FALSE
) {

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  ## make sure column names work unquoted too
  power_output_column <- rlang::ensym(power_output_column)
  time_to_exhaustion_column <- rlang::ensym(time_to_exhaustion_column)

  ## check method argument
  method <- match.arg(arg = method, several.ok = TRUE)

  if(all_combinations) {
    combinations <- get_combinations(
      .data = {{ .data }},
      power_output_column = {{ power_output_column }},
      time_to_exhaustion_column = {{ time_to_exhaustion_column }}
    )

    data_pre_processed <- get_indexes(
      .data = {{ .data }},
      power_output_column = {{ power_output_column }},
      time_to_exhaustion_column = {{ time_to_exhaustion_column }},
      combinations = combinations
    )

  } else {
    data_pre_processed <- .data %>%
      tidyr::nest(data = dplyr::everything())
  }

  data_calculated <- data_pre_processed %>%
    tidyr::crossing(method = method) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(results = dplyr::case_when(
      method == "3-hyp" ~ method_3_hyp(
        .data = data,
        power_output_column = {{ power_output_column }},
        time_to_exhaustion_column = {{ time_to_exhaustion_column }},
        plot = {{ plot }},
        power_in_y_axis = {{ power_in_y_axis }}
      ) %>% list(),
      method == "2-hyp" ~ method_2_hyp(
        .data = data,
        power_output_column = {{ power_output_column }},
        time_to_exhaustion_column = {{ time_to_exhaustion_column }},
        plot = {{ plot }},
        power_in_y_axis = {{ power_in_y_axis }}
      ) %>% list(),
      method == "linear" ~ method_linear(
        .data = data,
        power_output_column = {{ power_output_column }},
        time_to_exhaustion_column = {{ time_to_exhaustion_column }},
        plot = {{ plot }}
      ) %>% list(),
      method == "1/time" ~ method_1_time(
        .data = data,
        power_output_column = {{ power_output_column }},
        time_to_exhaustion_column = {{ time_to_exhaustion_column }},
        plot = {{ plot }}
      ) %>% list()
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(purrr::map_lgl(results, ~!rlang::is_lgl_na(.x))) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(cols = results)

  if(all_combinations) {
    out <- data_calculated %>%
      dplyr::mutate(method = factor(x = method, levels = c("3-hyp", "2-hyp", "linear", "1/time")),
                    n_char = nchar(index),
                    index = forcats::as_factor(index)) %>%
      dplyr::arrange(-n_char, index, method) %>%
      dplyr::mutate(index = as.character(index),
                    method = as.character(method)) %>%
      dplyr::select(-n_char)
  } else {
    out <- data_calculated %>%
      dplyr::mutate(method = factor(x = method, levels = c("3-hyp", "2-hyp", "linear", "1/time"))) %>%
      dplyr::arrange(method) %>%
      dplyr::mutate(method = as.character(method))
  }

  if(plot & all_combinations) {
    out <- out %>%
      dplyr::rowwise() %>%
      dplyr::mutate(plot = add_index_plot(.plot = plot, method = method, index = index) %>% list()) %>%
      dplyr::ungroup()
  }

  ## this excludes the Pmax and Pmax SEE columns in case 3-hyp was not chosen
  out <- out %>%
    janitor::remove_empty(which = "cols") %>%
    ## make sure empty cells are displayed as NA
    dplyr::mutate(dplyr::across(.fns = ~ ifelse(.x == "", NA, .x)))

  out
}
