#' Plot CP model
#'
#' @param .data A data frame containing a power output column and their respective time-to-exhaustion.
#' @param power_output_column The name of the power output column. This value has to be in watts. Default to `"PO"`.
#' @param time_to_exhaustion_column The name of the time-to-exhaustion column. This value has to be in seconds. Default to `"TTE"`.
#' @param method The method for estimating critical power. It can be one of `c("3-hyp", "2-hyp", "linear", "1/time")`.
#' @param model The `nls` or `lm` object derived in one of the `method_` functions.
#'
#' @return A `ggplot2` object
#' @export
plot_cp <- function(
  .data,
  power_output_column,
  time_to_exhaustion_column,
  method = c("3-hyp", "2-hyp", "linear", "1/time"),
  model
) {

  method <- match.arg(method)

  if(method == "3-hyp") {
    min_power_output <- min(.data[[{{ power_output_column }}]])
    max_power_output <- max(.data[[{{ power_output_column }}]])

    new_data <- dplyr::tibble(
      !!{{ power_output_column }} := seq(min_power_output, max_power_output, 1)
    )

    data_plot <- broom::augment(model, newdata = new_data)
    names(data_plot) <- c({{ power_output_column }}, {{ time_to_exhaustion_column }})

    cp_plot <- data_plot %>%
      ggplot2::ggplot(ggplot2::aes(!!rlang::sym(power_output_column), !!rlang::sym(time_to_exhaustion_column))) +
      ggplot2::geom_line(colour = "red", size = 1) +
      ggplot2::geom_point(data = .data, shape = 21, size = 4) +
      ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
      ggplot2::labs(x = "Power Output (W)",
                    y = "Time-to-exhaustion (s)",
                    title = bquote(CP[3-hyp])) +
      ggplot2::theme_light()
  } else if(method == "2-hyp") {

    min_power_output <- min(.data[[{{ power_output_column }}]])
    max_power_output <- max(.data[[{{ power_output_column }}]])

    new_data <- dplyr::tibble(
      !!{{ power_output_column }} := seq(min_power_output, max_power_output, 1)
    )

    data_plot <- broom::augment(model, newdata = new_data)
    names(data_plot) <- c({{ power_output_column }}, {{ time_to_exhaustion_column }})

    cp_plot <- data_plot %>%
      ggplot2::ggplot(ggplot2::aes(!!rlang::sym(power_output_column), !!rlang::sym(time_to_exhaustion_column))) +
      ggplot2::geom_line(colour = "red", size = 1) +
      ggplot2::geom_point(data = .data, shape = 21, size = 4) +
      ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
      ggplot2::labs(x = "Power Output (W)",
                    y = "Time-to-exhaustion (s)",
                    title = bquote(CP[2-hyp])) +
      ggplot2::theme_light()
  } else if(method == "linear") {

    cp_plot <- .data %>%
      ggplot2::ggplot(ggplot2::aes(!!rlang::sym(time_to_exhaustion_column), !!rlang::sym(power_output_column) * !!rlang::sym(time_to_exhaustion_column))) +
      ggplot2::geom_smooth(method = "lm", formula = "y ~ x", se = FALSE, colour = "red", size = 1) +
      ggplot2::geom_point(data = .data, shape = 21, size = 4) +
      ggplot2::scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
      ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
      ggplot2::labs(x = "Time-to-exhaustion (s)",
                    y = "Work (J)",
                    title = bquote(CP[linear])) +
      ggplot2::theme_light()

  } else if(method == "1/time") {

    cp_plot <- .data %>%
      ggplot2::ggplot(ggplot2::aes(1 / !!rlang::sym(time_to_exhaustion_column), !!rlang::sym(power_output_column))) +
      ggplot2::geom_smooth(method = "lm", formula = "y ~ x", se = FALSE, colour = "red", size = 1) +
      ggplot2::geom_point(data = .data, shape = 21, size = 4) +
      ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      ggplot2::labs(x = "Time-to-exhaustion (1/s)",
                    y = "Power Output (W)",
                    title = bquote(CP[1/time])) +
      ggplot2::theme_light()
  }

  cp_plot
}
