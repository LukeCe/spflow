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
    spflow_data,
    spflow_indicators,
    spflow_moments,
    spflow_neighborhood,
    spflow_matrices) {

  # add fitted values , residuals, and goodness-of-fit
  model <- object@estimation_control[["model"]]
  nb_rho <- spatial_model_order(model)
  mu <- coef(object)
  rho <- mu[seq_len(nb_rho)]
  delta <- mu[!names(mu) %in% names(rho)]

  spflow_matrices <- drop_instruments(spflow_matrices)
  signal <- compute_signal(delta, spflow_matrices, spflow_indicators, keep_matrix_form = FALSE)
  spflow_indicators <- cbind(spflow_indicators, SIGNAL = NA, FITTED = NA)

  filter_x <- spflow_indicators[["HAS_SIG"]] %||% TRUE
  filter_y <- spflow_indicators[["HAS_Y"]] %||% filter_x
  spflow_indicators[filter_x, "SIGNAL"] <- signal

  fit_method <- object@estimation_control[["fitted_value_method"]]
  fit_method <- ifelse(model == "model_1", yes = "LIN", fit_method)
  if (fit_method == "LIN") {
    filter_y <- spflow_indicators[["HAS_Y"]] %||% TRUE
    spflow_indicators[filter_y, "FITTED"] <- spflow_indicators[filter_y, "SIGNAL"]
  }
  if (fit_method == "TS") {
    y_ind <- spflow_indicators2pairindex(spflow_indicators, "HAS_Y")
    trend <- Reduce("+", Map("*", rho, spflow_matrices[["Y_"]][-1]))[y_ind]
    spflow_indicators[filter_y, "FITTED"] <- trend + spflow_indicators[filter_y, "SIGNAL"]
  }
  if (fit_method == "TC") {
    EY <- compute_expectation(
      signal_matrix = signal,
      DW = object@spflow_neighborhood$DW,
      OW = object@spflow_neighborhood$OW,
      rho = rho,
      model = model,
      M_indicator = spflow_indicators2pairindex(spflow_indicators, "HAS_Y"),
      approximate = object@estimation_control[["approx_expectation"]],
      max_it = object@estimation_control[["expectation_approx_order"]],
      keep_matrix_form = FALSE)
    spflow_indicators[filter_y, "FITTED"] <- EY
  }

  R2_corr <- cor(spflow_indicators[["FITTED"]], spflow_indicators[["ACTUAL"]], use = "complete.obs")
  object@estimation_diagnostics <- c(
    object@estimation_diagnostics,
    spflow_indicators2obs(spflow_indicators),
    R2_corr = as.numeric(R2_corr^2))

  object@spflow_neighborhood <- spflow_neighborhood
  object@spflow_indicators <- spflow_indicators
  object@spflow_moments <- spflow_moments
  if (!object@estimation_control[["reduce_size"]]) {
    object@spflow_matrices <- spflow_matrices
    object@spflow_data <- spflow_data
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
