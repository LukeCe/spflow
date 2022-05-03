#' @keywords internal
spflow_model_estimation <- function(
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
