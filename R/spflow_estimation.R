#' @keywords internal
spflow_estimation <- function(
  spflow_moments,
  spflow_nbfunctions,
  estimation_control) {

  estimation_results <- switch(estimation_control[["estimation_method"]],
    "ols" = {
      spflow_ols(
        ZZ  = spflow_moments[["ZZ"]],
        ZY  = spflow_moments[["ZY"]],
        TSS = spflow_moments[["TSS"]],
        N   = spflow_moments[["N"]],
        TCORR = spflow_moments[["TCORR"]],
        estimation_control = estimation_control
      )},
    "s2sls" = {
      spflow_s2sls(
        UU  = spflow_moments[["UU"]],
        UY  = spflow_moments[["UY"]],
        ZZ  = spflow_moments[["ZZ"]],
        ZY  = spflow_moments[["ZY"]],
        TSS = spflow_moments[["TSS"]],
        N   = spflow_moments[["N"]],
        TCORR = spflow_moments[["TCORR"]],
        pspace_validator = spflow_nbfunctions[["pspace_validator"]],
        estimation_control = estimation_control
        )},
    "mle" = {
      spflow_mle(
        ZZ    = spflow_moments[["ZZ"]],
        ZY    = spflow_moments[["ZY"]],
        TSS   = spflow_moments[["TSS"]],
        N     = spflow_moments[["N"]],
        n_d   = spflow_moments[["n_d"]],
        n_o   = spflow_moments[["n_o"]],
        TCORR = spflow_moments[["TCORR"]],
        estimation_control = estimation_control,
        pspace_validator = spflow_nbfunctions[["pspace_validator"]],
        logdet_calculator = spflow_nbfunctions[["logdet_calculator"]]
      )},
    "mcmc" = {
      spflow_mcmc(
        ZZ  = spflow_moments[["ZZ"]],
        ZY  = spflow_moments[["ZY"]],
        TSS = spflow_moments[["TSS"]],
        N   = spflow_moments[["N"]],
        n_d = spflow_moments[["n_d"]],
        n_o = spflow_moments[["n_o"]],
        TCORR = spflow_moments[["TCORR"]],
        estimation_control = estimation_control,
        pspace_validator = spflow_nbfunctions[["pspace_validator"]],
        logdet_calculator = spflow_nbfunctions[["logdet_calculator"]]
      )})

  estimation_results@spflow_nbfunctions <- spflow_nbfunctions
  return(estimation_results)
}

#' @title Internal method to add details to a [spflow_model-class()]
#'
#' @details
#' The method adds the design matrix and the coefficient names to an
#' [spflow_model-class()] object.
#' It also calculates the fitted values and the residuals as well as a
#' goodness-of-fit measure.
#'
#' @return An object of class spflow_model
#' @noRd
#' @keywords  internal
spflow_post_estimation <- function(
    object,
    spflow_networks,
    spflow_indicators,
    spflow_moments,
    spflow_matrices,
    spflow_formula) {


  object@spflow_moments <- spflow_moments
  object@spflow_formula <- spflow_formula
  object@spflow_matrices <- drop_instruments(spflow_matrices)
  object@spflow_networks <- spflow_networks

  object@spflow_indicators <- cbind(spflow_indicators, SIGNAL = NA, FITTED = NA)
  pred <- predict(
    object,
    method = object@estimation_control[["fitted_value_method"]],
    add_new_signal = TRUE)
  object@spflow_indicators[c("SIGNAL","FITTED")] <- pred[c("NEW_SIGNAL", "PREDICTION")]
  object@estimation_diagnostics[["R2_corr"]] <-
    spflow_indicators2Rcorr(object@spflow_indicators)

  object@estimation_diagnostics <- c(
    object@estimation_diagnostics,
    spflow_indicators2obs(spflow_indicators))

  if (isTRUE(object@estimation_control[["reduce_model_size"]])) {
    object@spflow_matrices <- NULL
    object@spflow_networks <- NULL
    object@spflow_indicators <- NULL
  }

  return(object)
}


#' @importFrom utils askYesNo
#' @keywords internal
solve_savely <- function(ZZ, ZY, TCORR, error_msg) {

  result <- try(solve(ZZ, ZY), silent = TRUE)
  if (!is(result,"try-error"))
    return(result)

  if (interactive()) {
    question <- sprintfwrap("
    The covariates of your models lead to a singular fit! <br>
    Type \"yes\" to show a correlpation plot.")
    answer <- askYesNo(question, default = FALSE)
    if (answer)
      cor_image(TCORR)
  }

  if (missing(error_msg))
    stop(result[1])

  stop(sprintfwrap(error_msg))
}
