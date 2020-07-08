#' CP modeling summary
#'
#' Retrieves a summary of the critical power model in a tidy way. Mainly for internal use.
#'
#' @param .data A data frame containing a power output column and their respective time-to-exhaustion.
#' @param time_to_exhaustion_column The name of the time-to-exhaustion column. This value has to be in seconds. Default to `"TTE"`.
#' @param model The `nls` or `lm` object derived in one of the `method_` functions.
#' @param method The method for estimating critical power. It can be one of `c("3-hyp", "2-hyp", "linear", "1/time")`.
#'
#' @importFrom stats cor lm predict
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
summary_cp <- function(.data, time_to_exhaustion_column, model, method) {

  if(method == "3-hyp") {
    # CP
    coeff_cp <- summary(model)$coeff[2,1]

    # CP Standard Error of the Estimate
    coeff_cp_se <- summary(model)$coeff[2,2]

    # W'
    coeff_awc <- summary(model)$coeff[1,1]

    # W' Standard Error of the Estimate
    coeff_awc_se <- summary(model)$coeff[1,2]

    # P max
    coeff_pmax <- summary(model)$coeff[3,1]

    # P max Standard Error of the Estimate
    coeff_pmax_se <- summary(model)$coeff[3,2]

    # R^2
    r2 <- cor(.data[[{{ time_to_exhaustion_column }}]], predict(model)) ^ 2

    # RMSE
    rmse <- summary(model)$sigma

  } else if(method == "2-hyp") {
    # CP
    coeff_cp <- summary(model)$coeff[2,1]

    # CP Standard Error of the Estimate
    coeff_cp_se <- summary(model)$coeff[2,2]

    # W'
    coeff_awc <- summary(model)$coeff[1,1]

    # W' Standard Error of the Estimate
    coeff_awc_se <- summary(model)$coeff[1,2]

    # P max
    coeff_pmax <- NA

    # P max Standard Error of the Estimate
    coeff_pmax_se <- NA

    # R^2
    r2 <- cor(.data[[{{ time_to_exhaustion_column }}]], predict(model)) ^ 2

    # RMSE
    rmse <- summary(model)$sigma

  } else if(method == "linear") {
    # CP
    coeff_cp <- summary(model)$coeff[2,1]

    # CP Standard Error of the Estimate
    coeff_cp_se <- summary(model)$coeff[2,2]

    # W'
    coeff_awc <- summary(model)$coeff[1,1]

    # W' Standard Error of the Estimate
    coeff_awc_se <- summary(model)$coeff[1,2]

    # P max
    coeff_pmax <- NA

    # P max Standard Error of the Estimate
    coeff_pmax_se <- NA

    # R^2
    r2 <- summary(model)$r.squared

    # RMSE
    rmse <- summary(model)$sigma

  } else if(method == "1/time") {
    # CP
    coeff_cp <- summary(model)$coeff[1,1]

    # CP Standard Error of the Estimate
    coeff_cp_se <- summary(model)$coeff[1,2]

    # W'
    coeff_awc <- summary(model)$coeff[2,1]

    # W' Standard Error of the Estimate
    coeff_awc_se <- summary(model)$coeff[2,2]

    # P max
    coeff_pmax <- NA

    # P max Standard Error of the Estimate
    coeff_pmax_se <- NA

    # R^2
    r2 <- summary(model)$r.squared

    # RMSE
    rmse <- summary(model)$sigma
  }

  # Final summary
  out <- dplyr::tibble(
    model = list(model),
    CP = round(coeff_cp, digits = 1),
    "CP SEE" = round(coeff_cp_se, digits = 1),
    "W'" = round(coeff_awc, digits = 1),
    "W' SEE" = round(coeff_awc_se, digits = 1),
    "Pmax" = round(coeff_pmax, digits = 1),
    "Pmax SEE" = round(coeff_pmax_se, digits = 1),
    "R2" = round(r2, digits = 5),
    "RMSE" = round(rmse, digits = 2)
  )

  out
}
