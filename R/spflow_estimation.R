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
#' @keywords  internal
spflow_post_estimation <- function(
    object,
    spflow_networks,
    spflow_indicators,
    spflow_moments,
    spflow_matrices,
    spflow_formula) {


  object@spflow_moments <- spflow_moments
  object@spflow_matrices <- drop_instruments(spflow_matrices)
  object@spflow_networks <- spflow_networks
  object@spflow_formula <- spflow_formula

  object@spflow_indicators <- cbind(spflow_indicators, SIGNAL = NA, FITTED = NA)
  pred <- predict(
    object,
    method = object@estimation_control[["fitted_value_method"]],
    add_new_signal = TRUE)
  object@spflow_indicators[c("SIGNAL","FITTED")] <- pred[c("NEW_SIGNAL", "PREDICTION")]


  R2_corr <- as.numeric(cor(
    object@spflow_indicators[["FITTED"]],
    object@spflow_indicators[["ACTUAL"]],
    use = "complete.obs"))^2

  object@estimation_diagnostics <- c(
    object@estimation_diagnostics,
    spflow_indicators2obs(spflow_indicators),
    "R2_corr" = R2_corr)

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
         The covariates of your models lead to a singular fit!
         <br>Type \"yes\" to show a correlpation plot.")
    answer <- askYesNo(question, default = FALSE)

    if (answer)
      corr_map(TCORR)
  }

  if (missing(error_msg))
    stop(result[1])

  stop(sprintfwrap(error_msg))
}
