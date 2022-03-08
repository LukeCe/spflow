#' @keywords internal
spflow_model_estimation <- function(
  model_moments,
  flow_control) {

  estimation_results <- switch(flow_control$estimation_method,
    "ols" = {
      spflow_ols(
        ZZ  = model_moments[["ZZ"]],
        ZY  = model_moments[["ZY"]],
        TSS = model_moments[["TSS"]],
        N   = model_moments[["N"]],
        TCOVAR = model_moments[["TCOVAR"]],
        flow_control = flow_control
      )},
    "s2sls" = {
      spflow_s2sls(
        UU  = model_moments[["UU"]],
        UY  = model_moments[["UY"]],
        ZZ  = model_moments[["ZZ"]],
        ZY  = model_moments[["ZY"]],
        TSS = model_moments[["TSS"]],
        N   = model_moments[["N"]],
        TCOVAR = model_moments[["TCOVAR"]],
        flow_control = flow_control
      )},
    "mle" = {
      spflow_mle(
        ZZ    = model_moments[["ZZ"]],
        ZY    = model_moments[["ZY"]],
        TSS   = model_moments[["TSS"]],
        N     = model_moments[["N"]],
        n_d   = model_moments[["n_d"]],
        n_o   = model_moments[["n_o"]],
        TCOVAR = model_moments[["TCOVAR"]],
        flow_control = flow_control,
        logdet_calculator = model_moments[["logdet_calculator"]]
      )},
    "mcmc" = {spflow_mcmc(
      ZZ  = model_moments[["ZZ"]],
      ZY  = model_moments[["ZY"]],
      TSS = model_moments[["TSS"]],
      N   = model_moments[["N"]],
      n_d = model_moments[["n_d"]],
      n_o = model_moments[["n_o"]],
      TCOVAR = model_moments[["TCOVAR"]],
      flow_control = flow_control,
      logdet_calculator = model_moments[["logdet_calculator"]]
    )}
  )
  return(estimation_results)
}

#' @importFrom utils askYesNo
#' @keywords internal
solve_savely <- function(ZZ, ZY, TCOVAR, error_msg) {

  result <- try(solve(ZZ, ZY), silent = TRUE)
  if (!is(result,"try-error"))
    return(result)

  if (interactive()) {
    question <- sprintfwrap("
         The covariates of your models lead to a singular fit!
         <br>Type \"yes\" to show a correlpation plot.")
    answer <- askYesNo(question, default = FALSE)

    if (answer)
      cor_map(TCOVAR)
  }

  if (missing(error_msg))
    stop(result[1])

  stop(sprintfwrap(error_msg))
}
