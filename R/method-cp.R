#' CP 3-parameter hyperbolic model
#'
#' Fits critical power with the 3-parameter hyperbolic model (Morton, 1996).
#' For more details see `?critical_power`.
#'
#' @param .data A data frame containing a power output column and their respective time-to-exhaustion.
#' @param power_output_column The name of the power output column. This value has to be in watts. Default to `"PO"`.
#' @param time_to_exhaustion_column The name of the time-to-exhaustion column. This value has to be in seconds. Default to `"TTE"`.
#' @param plot A boolean indicating whether to produce a plot from the critical power estimation. Default to `TRUE`.
#' @param power_in_y_axis A boolean to indicate whether to plot the Power Output in the y-axis. Default to `FALSE`.
#'
#' @return a [tibble][tibble::tibble-package] containing the following columns:
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
#' results_3_hyp <- method_3_hyp(.data = demo_data, "PO", "TTE")
#'
#' results_3_hyp
method_3_hyp <- function(
  .data,
  power_output_column,
  time_to_exhaustion_column,
  plot = TRUE,
  power_in_y_axis = FALSE
) {

  if(nrow(.data) < 4)
    return(NA)

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  ## make sure column names work unquoted too
  power_output_column <- rlang::ensym(power_output_column)
  time_to_exhaustion_column <- rlang::ensym(time_to_exhaustion_column)

  # Setting starting values
  a_start <- max(.data[[{{ power_output_column }}]]) * min(.data[[{{ time_to_exhaustion_column }}]])
  b_start <- min(.data[[{{ power_output_column }}]]) - 30
  c_start <- max(.data[[{{ power_output_column }}]]) * 1.5

  # Formula
  cp_formula <- glue::glue("{time_to_exhaustion_column} ~ (AWC / ({power_output_column} - CP)) + (AWC / (CP - Pmax))")

  # Model
  model <- minpack.lm::nlsLM(
    formula = cp_formula,
    data = .data,
    start = list(AWC = a_start, CP = b_start, Pmax = c_start),
    control = list(maxiter = 1000),
    lower = c(AWC = 0, CP = 0, Pmax = 0)
  )

  results_summary <- summary_cp(.data = .data, time_to_exhaustion_column = {{ time_to_exhaustion_column }}, model = model, method = "3-hyp")

  if(plot) {

    cp_plot <- plot_cp(
      .data = .data,
      power_output_column = {{ power_output_column }},
      time_to_exhaustion_column = {{ time_to_exhaustion_column }},
      method = "3-hyp",
      model = model
    )

    if(power_in_y_axis)
      cp_plot <- cp_plot +
        ggplot2::coord_flip()

    results_summary <- results_summary %>%
      dplyr::mutate(plot = list(cp_plot))
  }

  out <- .data %>%
    tidyr::nest(data = dplyr::everything()) %>%
    dplyr::bind_cols(results_summary)

  out

}

#' CP 2-parameter hyperbolic model
#'
#' Fits critical power with the 2-parameter hyperbolic model (Hill, 1993).
#' For more details see `?critical_power`.
#'
#' @param .data A data frame containing a power output column and their respective time-to-exhaustion.
#' @param power_output_column The name of the power output column. This value has to be in watts. Default to `"PO"`.
#' @param time_to_exhaustion_column The name of the time-to-exhaustion column. This value has to be in seconds. Default to `"TTE"`.
#' @param plot A boolean indicating whether to produce a plot from the critical power estimation. Default to `TRUE`.
#' @param power_in_y_axis A boolean to indicate whether to plot the Power Output in the y-axis. Default to `FALSE`.
#'
#' @return a [tibble][tibble::tibble-package] containing the following columns:
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
#' results_2_hyp <- method_2_hyp(.data = demo_data, "PO", "TTE")
#'
#' results_2_hyp
method_2_hyp <- function(
  .data,
  power_output_column,
  time_to_exhaustion_column,
  plot = TRUE,
  power_in_y_axis = FALSE
) {

  if(nrow(.data) < 3)
    return(NA)

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  ## make sure column names work unquoted too
  power_output_column <- rlang::ensym(power_output_column)
  time_to_exhaustion_column <- rlang::ensym(time_to_exhaustion_column)

  # Setting starting values
  a_start <- max(.data[[{{ power_output_column }}]]) * min(.data[[{{ time_to_exhaustion_column }}]])
  b_start <- min(.data[[{{ power_output_column }}]]) - 30

  # Formula
  cp_formula <- glue::glue("{time_to_exhaustion_column} ~ AWC / ({power_output_column} - CP)")

  # Model
  model <- minpack.lm::nlsLM(
    formula = cp_formula,
    data = .data,
    start = list(AWC = a_start, CP = b_start),
    control = list(maxiter = 1000),
    lower = c(AWC = 0, CP = 0)
  )

  results_summary <- summary_cp(.data = .data, time_to_exhaustion_column = {{ time_to_exhaustion_column }}, model = model, method = "2-hyp")

  if(plot) {

    cp_plot <- plot_cp(
      .data = .data,
      power_output_column = {{ power_output_column }},
      time_to_exhaustion_column = {{ time_to_exhaustion_column }},
      method = "2-hyp",
      model = model
    )

    if(power_in_y_axis)
      cp_plot <- cp_plot +
        ggplot2::coord_flip()

    results_summary <- results_summary %>%
      dplyr::mutate(plot = list(cp_plot))
  }

  out <- .data %>%
    tidyr::nest(data = dplyr::everything()) %>%
    dplyr::bind_cols(results_summary)

  out
}

#' CP linear model
#'
#' Fits critical power with linear model (Moritani, 1981).
#' For more details see `?critical_power`.
#'
#' @param .data A data frame containing a power output column and their respective time-to-exhaustion.
#' @param power_output_column The name of the power output column. This value has to be in watts. Default to `"PO"`.
#' @param time_to_exhaustion_column The name of the time-to-exhaustion column. This value has to be in seconds. Default to `"TTE"`.
#' @param plot A boolean indicating whether to produce a plot from the critical power estimation. Default to `TRUE`.
#'
#' @return a [tibble][tibble::tibble-package] containing the following columns:
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
#' results_linear <- method_linear(.data = demo_data, "PO", "TTE")
#'
#' results_linear
method_linear <- function(
  .data,
  power_output_column,
  time_to_exhaustion_column,
  plot = TRUE
) {

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  ## make sure column names work unquoted too
  power_output_column <- rlang::ensym(power_output_column)
  time_to_exhaustion_column <- rlang::ensym(time_to_exhaustion_column)

  # Formula
  cp_formula <- glue::glue("{power_output_column} * {time_to_exhaustion_column} ~ {time_to_exhaustion_column}")

  # Model
  model <- lm(
    formula = cp_formula,
    data = .data
  )

  results_summary <- summary_cp(.data = .data, time_to_exhaustion_column = {{ time_to_exhaustion_column }}, model = model, method = "linear")

  if(plot) {

    cp_plot <- plot_cp(
      .data = .data,
      power_output_column = {{ power_output_column }},
      time_to_exhaustion_column = {{ time_to_exhaustion_column }},
      method = "linear",
      model = model
    )

    results_summary <- results_summary %>%
      dplyr::mutate(plot = list(cp_plot))
  }

  out <- .data %>%
    tidyr::nest(data = dplyr::everything()) %>%
    dplyr::bind_cols(results_summary)

  out
}

#' CP 1/time linear model
#'
#' Fits critical power with the 1/time linear model (Whipp et al. 1982).
#' For more details see `?critical_power`.
#'
#' @param .data A data frame containing a power output column and their respective time-to-exhaustion.
#' @param power_output_column The name of the power output column. This value has to be in watts. Default to `"PO"`.
#' @param time_to_exhaustion_column The name of the time-to-exhaustion column. This value has to be in seconds. Default to `"TTE"`.
#' @param plot A boolean indicating whether to produce a plot from the critical power estimation. Default to `TRUE`.
#'
#' @return a [tibble][tibble::tibble-package] containing the following columns:
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
#' results_1_time <- method_1_time(.data = demo_data, "PO", "TTE")
#'
#' results_1_time
method_1_time <- function(
  .data,
  power_output_column,
  time_to_exhaustion_column,
  plot = TRUE
) {

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  ## make sure column names work unquoted too
  power_output_column <- rlang::ensym(power_output_column)
  time_to_exhaustion_column <- rlang::ensym(time_to_exhaustion_column)

  # Formula
  cp_formula <- glue::glue("{power_output_column} ~ I(1 / {time_to_exhaustion_column})")

  # Model
  model <- lm(
    formula = cp_formula,
    data = .data
  )

  results_summary <- summary_cp(.data = .data, time_to_exhaustion_column = {{ time_to_exhaustion_column }}, model = model, method = "1/time")

  if(plot) {

    cp_plot <- plot_cp(
      .data = .data,
      power_output_column = {{ power_output_column }},
      time_to_exhaustion_column = {{ time_to_exhaustion_column }},
      method = "1/time",
      model = model
    )

    results_summary <- results_summary %>%
      dplyr::mutate(plot = list(cp_plot))
  }

  out <- .data %>%
    tidyr::nest(data = dplyr::everything()) %>%
    dplyr::bind_cols(results_summary)

  out
}
