#' CP models
#'
#' These functions fit the following critical power/speed models: \cr
#' * 3-parameter hyperbolic model (Morton, 1996). \cr
#' * 2-parameter hyperbolic model (Hill, 1993). \cr
#' * linear model (Moritani, 1981). \cr
#' * 1/time linear model (Whipp et al. 1982). \cr
#' For more details see `?critical_power` or `?critical_speed`.
#'
#' @param .data A data frame containing a power output/distance column and their respective time-to-exhaustion.
#' @param power_output_column The name of the power output column. This value has to be in watts. Used in the 'power' mode only.
#' @param distance_column The name of the distance column. This value has to be in meters. Used in the 'speed' mode only.
#' @param time_to_exhaustion_column The name of the time-to-exhaustion column. This value has to be in seconds. Default to `"TTE"`.
#' @param method The method for estimating critical speed. It can be one or multiple methods. Default to `c("3-hyp", "2-hyp", "linear", "1/time")`.
#' @param mode Specify 'power' for critical power, and 'speed' for critical speed.
#'
#' @return a [tibble][tibble::tibble-package] containing the following columns:
#' \item{method}{The critical power method for that estimation.}
#' \item{data}{The data you provided.}
#' \item{model}{A `nls` or `lm` object. The model used in the critical power fitting.}
#' \item{CP or CS}{Critical power, in watts, or critical speed, in meters per second.}
#' \item{CP SEE or CS SEE}{Standard error of the estimation of critical power, in watts, or critical speed, in meters per second.}
#' \item{W' or D'}{Anaerobic work capacity, in joules, or anaerobic distance capacity, in meters.}
#' \item{W' or D' SEE}{Standard error of the estimation of anaerobic work capacity, in joules, or anaerobic distance capacity, in meters.}
#' \item{Pmax or Smax}{Maximal instantaneous power/speed, in watts/meters per second (only available in CP/CS 3-hyp).}
#' \item{Pmax SEE or Smax SEE}{Standard error of the estimation of maximal instantaneous power/speed, in watts/meters per second (only available in CP/CS 3-hyp).}
#' \item{R2}{R-squared.}
#' \item{RMSE}{Root mean square error, the units vary depending on the method, as they represent the units of the response (i.e., y-axis): CP3-hyp in seconds, CP2-hyp in seconds, CPlinear in joules, and CP1/time in watts.}
#' \item{plot}{The critical power plot.}
#'
#' @importFrom rlang :=
#' @keywords internal
method_cp <- function(
  .data,
  power_output_column,
  distance_column,
  time_to_exhaustion_column = "TTE",
  method = c("3-hyp", "2-hyp", "linear", "1/time"),
  mode = c("power", "speed")
) {

  if(missing(.data))
    stop("No data, no fun. Please, include your data to the function.", call. = FALSE)

  mode <- match.arg(arg = mode, several.ok = FALSE)

  class(.data) <- mode

  UseMethod("method_cp", .data)
}

#' @export
method_cp.power <- function(
  .data,
  power_output_column,
  distance_column,
  time_to_exhaustion_column = "TTE",
  method = c("3-hyp", "2-hyp", "linear", "1/time"),
  mode = c("power", "speed")
) {

  if(method == "3-hyp") {

    if(nrow(.data) < 4)
      return(NA)

    # Setting starting values
    a_start <- max(.data[[{{ power_output_column }}]]) * min(.data[[{{ time_to_exhaustion_column }}]])
    b_start <- min(.data[[{{ power_output_column }}]]) * 0.9
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

    min_power_output <- min(.data[[{{ power_output_column }}]])
    max_power_output <- max(.data[[{{ power_output_column }}]])

    new_data <- dplyr::tibble(
      !!{{ power_output_column }} := seq(min_power_output, max_power_output, length.out = 200)
    )

    data_plot <- broom::augment(model, newdata = new_data)
    names(data_plot) <- c("power", "time")

  } else if(method == "2-hyp") {

    if(nrow(.data) < 3)
      return(NA)

    # Setting starting values
    a_start <- max(.data[[{{ power_output_column }}]]) * min(.data[[{{ time_to_exhaustion_column }}]])
    b_start <- min(.data[[{{ power_output_column }}]]) * 0.9

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

    min_power_output <- min(.data[[{{ power_output_column }}]])
    max_power_output <- max(.data[[{{ power_output_column }}]])

    new_data <- dplyr::tibble(
      !!{{ power_output_column }} := seq(min_power_output, max_power_output, length.out = 200)
    )

    data_plot <- broom::augment(model, newdata = new_data)
    names(data_plot) <- c("power", "time")

  } else if (method == "linear") {

    # Formula
    cp_formula <- glue::glue("{power_output_column} * {time_to_exhaustion_column} ~ {time_to_exhaustion_column}")

    # Model
    model <- lm(
      formula = cp_formula,
      data = .data
    )

    min_time_to_exhaustion <- min(.data[[{{ time_to_exhaustion_column }}]])
    max_time_to_exhaustion <- max(.data[[{{ time_to_exhaustion_column }}]])

    new_data <- dplyr::tibble(
      !!{{ time_to_exhaustion_column }} := seq(min_time_to_exhaustion, max_time_to_exhaustion, length.out = 200)
    )

    data_plot <- broom::augment(model, newdata = new_data)
    names(data_plot) <- c("time", "awc")

  } else if (method == "1/time") {

    # Formula
    cp_formula <- glue::glue("{power_output_column} ~ I(1 / {time_to_exhaustion_column})")

    # Model
    model <- lm(
      formula = cp_formula,
      data = .data
    )

    min_time_to_exhaustion <- min(.data[[{{ time_to_exhaustion_column }}]])
    max_time_to_exhaustion <- max(.data[[{{ time_to_exhaustion_column }}]])

    new_data <- dplyr::tibble(
      !!{{ time_to_exhaustion_column }} := seq(min_time_to_exhaustion, max_time_to_exhaustion, length.out = 200)
    )

    data_plot <- broom::augment(model, newdata = new_data)
    names(data_plot) <- c("1/time", "power")
    data_plot <- data_plot %>%
      dplyr::mutate(`1/time` = 1 / `1/time`)
  }

  results_summary <- summary_cp(
    .data = .data,
    time_to_exhaustion_column = {{ time_to_exhaustion_column }},
    model = model,
    method = {{ method }}
  )

  out <- .data %>%
    tidyr::nest(data = dplyr::everything()) %>%
    dplyr::bind_cols(results_summary, tidyr::nest(data_plot, data_plot = dplyr::everything()))

  out
}

#' @export
method_cp.speed <- function(
  .data,
  power_output_column,
  distance_column,
  time_to_exhaustion_column = "TTE",
  method = c("3-hyp", "2-hyp", "linear", "1/time"),
  mode = c("power", "speed")
) {

  # mutate the speed column
  .data <- .data %>%
    dplyr::mutate(speed = !!rlang::sym(distance_column) / !!rlang::sym(time_to_exhaustion_column))

  if(method == "3-hyp") {

    if(nrow(.data) < 4)
      return(NA)

    # Setting starting values
    a_start <- max(.data[["speed"]]) * min(.data[[{{ time_to_exhaustion_column }}]])
    b_start <- min(.data[["speed"]]) * 0.7
    c_start <- max(.data[["speed"]]) * 1.5

    # Formula
    cs_formula <- glue::glue("{time_to_exhaustion_column} ~ (AWC / (speed - CS)) + (AWC / (CS - Smax))")

    # Model
    model <- minpack.lm::nlsLM(
      formula = cs_formula,
      data = .data,
      start = list(AWC = a_start, CS = b_start, Smax = c_start),
      control = list(maxiter = 1000),
      lower = c(AWC = 0, CS = 0, Smax = 0)
    )

    min_speed <- min(.data[["speed"]])
    max_speed <- max(.data[["speed"]])

    new_data <- dplyr::tibble(
      "speed" = seq(min_speed, max_speed, length.out = 200)
    )

    data_plot <- broom::augment(model, newdata = new_data)
    names(data_plot) <- c("speed", "time")
  } else if(method == "2-hyp") {

    if(nrow(.data) < 3)
      return(NA)

    # Setting starting values
    a_start <- max(.data[["speed"]]) * min(.data[[{{ time_to_exhaustion_column }}]])
    b_start <- min(.data[["speed"]]) * 0.7

    # Formula
    cs_formula <- glue::glue("{time_to_exhaustion_column} ~ AWC / (speed - CS)")

    # Model
    model <- minpack.lm::nlsLM(
      formula = cs_formula,
      data = .data,
      start = list(AWC = a_start, CS = b_start),
      control = list(maxiter = 1000),
      lower = c(AWC = 0, CS = 0)
    )
    min_speed <- min(.data[["speed"]])
    max_speed <- max(.data[["speed"]])

    new_data <- dplyr::tibble(
      "speed" = seq(min_speed, max_speed, length.out = 200)
    )

    data_plot <- broom::augment(model, newdata = new_data)
    names(data_plot) <- c("speed", "time")
  } else if (method == "linear") {

    # Formula
    cs_formula <- glue::glue("speed * {time_to_exhaustion_column} ~ {time_to_exhaustion_column}")

    # Model
    model <- lm(
      formula = cs_formula,
      data = .data
    )
    min_time_to_exhaustion <- min(.data[[{{ time_to_exhaustion_column }}]])
    max_time_to_exhaustion <- max(.data[[{{ time_to_exhaustion_column }}]])

    new_data <- dplyr::tibble(
      !!{{ time_to_exhaustion_column }} := seq(min_time_to_exhaustion, max_time_to_exhaustion, length.out = 200)
    )

    data_plot <- broom::augment(model, newdata = new_data)
    names(data_plot) <- c("time", "awc")
  } else if (method == "1/time") {

    # Formula
    cs_formula <- glue::glue("speed ~ I(1 / {time_to_exhaustion_column})")

    # Model
    model <- lm(
      formula = cs_formula,
      data = .data
    )
    min_time_to_exhaustion <- min(.data[[{{ time_to_exhaustion_column }}]])
    max_time_to_exhaustion <- max(.data[[{{ time_to_exhaustion_column }}]])

    new_data <- dplyr::tibble(
      !!{{ time_to_exhaustion_column }} := seq(min_time_to_exhaustion, max_time_to_exhaustion, length.out = 200)
    )

    data_plot <- broom::augment(model, newdata = new_data)
    names(data_plot) <- c("1/time", "speed")
    data_plot <- data_plot %>%
      dplyr::mutate(`1/time` = 1 / `1/time`)
  }

  results_summary <- summary_cp(
    .data = .data,
    time_to_exhaustion_column = {{ time_to_exhaustion_column }},
    model = model,
    method = {{ method }},
    critical_speed = TRUE
  )

  out <- .data %>%
    tidyr::nest(data = dplyr::everything()) %>%
    dplyr::bind_cols(results_summary, tidyr::nest(data_plot, data_plot = dplyr::everything()))

  out
}
