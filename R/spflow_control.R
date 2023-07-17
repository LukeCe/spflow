#' @title Control parameters passed to [spflow()]
#'
#' @description
#' This function creates a list of parameters, that are used to fine tune the
#' estimation with [spflow()].
#' Here we can change the estimation method, adjust how the exogenous
#' variables enter the model, and also define the spatial autocorrelation structure.
#'
#' @section Details:
#'
#' ## Adjusting the autocorrelation structure
#'
#' The option `model` allows to declare one of nine different forms of
#' autocorrelation that follow the naming convention of
#' \insertCite{LeSage2008;textual}{spflow}.
#' The most general specification is "model_9", leading to the model
#'
#' \deqn{y = \rho_dW_dy + \rho_o W_oy + \rho_wW_wy + Z\delta + \epsilon.}
#'
#' All other models special cases of this one.
#' The constraints that lead to the different sub models are summarized in
#' this table.
#'
#'  | **Model Number** | **Autocorrelation Parameters** | **Constraints** |
#'  | :--------------: | :----------------------------: | :-------------: |
#'  | Model 9          | \eqn{\rho_d, \rho_o, \rho_w}   | unconstrained                      |
#'  | Model 8          | \eqn{\rho_d, \rho_o, \rho_w}   | \eqn{\rho_w = - \rho_d \rho_o}     |
#'  | Model 7          | \eqn{\rho_d, \rho_o}           | \eqn{\rho_w = 0}                   |
#'  | Model 6          | \eqn{\rho_{dow}}               | \eqn{\rho_d = \rho_o = \rho_w}     |
#'  | Model 5          | \eqn{\rho_{do}}                | \eqn{\rho_d = \rho_o, \rho_w = 0}  |
#'  | Model 4          | \eqn{\rho_w}                   | \eqn{\rho_d = \rho_o = 0}          |
#'  | Model 3          | \eqn{\rho_o}                   | \eqn{\rho_d = \rho_w = 0}          |
#'  | Model 2          | \eqn{\rho_d}                   | \eqn{\rho_o = \rho_w = 0}          |
#'  | Model 1          | none                           | \eqn{\rho_d = \rho_o = \rho_w = 0} |
#'
#' @param estimation_method
#'   A character which indicates the estimation method, should be one of
#'   `c("mle","s2sls","mcmc")`
#' @param model
#'   A character indicating the model number, that controls different spatial
#'   dependence structures should be one of `paste0("model_", 1:9)`.
#'   Details are given in the documentation of [spflow_control()].
#' @param use_intra
#'   A logical which activates the option to use a separate set of parameters
#'   for intra-regional flows (origin == destination)
#' @param sdm_variables
#'   Either a formula or a character; the formula can be used to explicitly
#'   declare the variables in SDM specification, the character should be one of
#'   `c("same", "all")` which are short cuts for using all available variables
#'   or the same as used in the main formula provided to [spflow()]
#' @param weight_functions
#'   A named list of functions that derive weights from the input data.
#'   The function "pair" is evaluated in the data-set of OD-pairs, "orig" in the
#'   data for origins and "dest" relates to the destination.
#'   Each function should return a single vector weight and in case there
#'   are multiple ones their product is used as weights.
#' @param approx_parameter_space A logical indicating whether the test for
#'   the feasible parameter space should be based on an approximation. Note
#'   that FALSE requires solving a system with a potentially large number
#'   of equations.
#' @param fitted_value_method
#'   A character indicating the method used for computing the fitted values.
#'   Should be one of c("TS", "TC", "BP").
#' @param approx_expectation
#'   A logical indicating whether the expected value of the flows should be
#'   computed based on a truncated power series.
#'   Note that FALSE requires solving a system with a potentially large number
#'   of equations.
#' @param expectation_approx_order
#'   A numeric indicating the order of the power series expression used to
#'   approximate the expected value of the flows.
#' @param logdet_approx_order
#'   A numeric indicating the order of the Taylor expansion used to approximate
#'   the value of the log-determinant term.
#' @param mle_hessian_method
#'   A character which indicates the method for Hessian calculation
#' @param mle_optim_limit
#'   A numeric indicating the number of trials given to the optimizer of the
#'   likelihood function. A trial refers to a new initiation of the
#'   optimization procedure using different (random) starting values for the
#'   parameters. If the optimizer does not converge after the
#'   indicated number of trails an error will be thrown after this limit.
#' @param twosls_instrumental_variables
#'   Either a formula or a character; the formula can be used to explicitly
#'   declare the variables that should be used as instruments during S2SLS
#'   estimation, the character should be one of `c("same", "all")` which
#'   are short cuts for using all available variables or the same as used in
#'   the main formula provided to [spflow()]
#' @param mcmc_iterations
#'   A numeric indicating the number of iterations
#' @param mcmc_burn_in
#'   A numeric indicating the length of the burn in period
#' @param mcmc_resampling_limit
#'   A numeric indicating the maximal number of trials during rejection
#'   sampling of the autoregressive parameters
#' @param twosls_decorrelate_instruments
#'   A logical whether to perform a PCA to remove (linear) correlation from the
#'   instruments generated for the S2SLS estimator
#' @param twosls_reduce_pair_instruments
#'   A logical that indicates whether the number of instruments that are
#'   derived from pair attributes should be reduced or not. The default is
#'   `TRUE`, because constructing these instruments is often the most demanding
#'   part of the estimation \insertCite{Dargel2021}{spflow}.
#' @param track_condition_numbers
#'   A logical that controls whether or not the reciprocal condition numbers
#'   of the systems that are solved during the estimation are tracked.
#' @param na_rm
#'   A logical, if `FALSE` the estimation throws an error when the data
#'   contains `NA` values. Otherwise the estimation proceeds with a sub-sample.
#' @param reduce_model_size
#'   A logical, if `TRUE` all data will be removed from the model.
#'   This is helpful when comparing many models but doing will inhibit some
#'   methods of the model.
#'
#' @seealso [spflow()]
#' @references \insertAllCited{}
#' @author Lukas Dargel
#' @return
#'   A list of parameters used to control the model estimation with [spflow()]
#' @export
#' @examples
#'
#' # default is MLE estimation of the most comprehensive model
#' default_control <- spflow_control()
#'
#' # change the estimation method
#' custom_control <- spflow_control(estimation_method = "mcmc")
#'
#' # change the form of autocorrelation to be considered
#' custom_control <- spflow_control(model = "model_7")
#'
#' # declare precisely which variables are to be used in the SDM form
#' custom_control <-
#'   spflow_control(sdm_variables = ~ O_(v1 + v2) + D_(v2 + v3) + I_(v1 + v4))
#'
#' # deactivate the intra-regional coefficients and SDM variables
#' custom_control <- spflow_control(use_intra = FALSE, sdm_variables = "none")
spflow_control <- function(
    estimation_method = "mle",
    model = "model_9",
    use_intra = TRUE,
    sdm_variables = "same",
    weight_functions = NULL,
    approx_parameter_space = TRUE,
    fitted_value_method = "TS",
    approx_expectation = TRUE,
    expectation_approx_order = 10,
    logdet_approx_order = 2,
    mle_hessian_method = "mixed",
    mle_optim_limit = 100,
    mcmc_iterations = 5500,
    mcmc_burn_in = 2500,
    mcmc_resampling_limit = 100,
    twosls_instrumental_variables = "same",
    twosls_decorrelate_instruments = FALSE,
    twosls_reduce_pair_instruments = TRUE,
    track_condition_numbers = FALSE,
    na_rm = FALSE,
    allow_singular = TRUE,
    reduce_model_size = FALSE) {

  assert_valid_option(estimation_method, c("s2sls", "mle","mcmc","ols"))
  assert_valid_option(model, paste0("model_", 1:9))
  if (estimation_method == "ols" | model == "model_1") {
    estimation_method <- "ols"
    model <- "model_1"
  }
  assert(model != "model_8" || estimation_method == "mle",
         "Model 8 is only implemented for the MLE estimator!")

  assert_is_single_x(use_intra, "logical")

  formula_shortcuts <- c("none","same","all")
  assert(is(sdm_variables,"formula") || sdm_variables %in% formula_shortcuts,
         'The sdm_variables should be a formula or one of %s!',
         deparse(formula_shortcuts))

  weight_fun_opts <- c("pair", "orig", "dest")
  assert(is.null(weight_functions) || (
         is.list(weight_functions) &&
         all(sapply(weight_functions, is.function)) &&
         all(names(weight_functions) %in% weight_fun_opts)),
         "The weight_functions must be a list of functions with names in %s!",
         deparse(weight_fun_opts))

  assert_is_single_x(track_condition_numbers, "logical")
  assert_is_single_x(na_rm, "logical")
  assert_is_single_x(reduce_model_size, "logical")
  assert_is_single_x(allow_singular, "logical")

  # control parameter used in all cases
  general_control <- list(
    "estimation_method" = estimation_method,
    "model" = model,
    "use_intra" = use_intra,
    "sdm_variables" = sdm_variables,
    "weight_functions" = weight_functions,
    "track_condition_numbers" = track_condition_numbers,
    "na_rm" = na_rm,
    "fitted_value_method" = fitted_value_method,
    "reduce_model_size" = reduce_model_size,
    "allow_singular" = allow_singular)

  if (estimation_method == "ols")
    return(general_control)

  # ---- spatial models -------------------------------------------------------
  assert_is_single_x(approx_parameter_space, "logical")
  assert_is_single_x(approx_expectation, "logical")
  assert_is_single_x(expectation_approx_order, "numeric")
  assert_is_single_x(fitted_value_method, "character")
  assert_valid_option(fitted_value_method, c("TS","TCI", "BPI"))

  general_control <- c(general_control, list(
    "approx_parameter_space" = approx_parameter_space,
    "approx_expectation" = approx_expectation,
    "expectation_approx_order" = expectation_approx_order))

  # ---- ... s2sls -----------------------------------------------------------
  if (estimation_method == "s2sls") {
    assert_is_single_x(twosls_decorrelate_instruments, "logical")
    assert_is_single_x(twosls_reduce_pair_instruments, "logical")
    assert(is(twosls_instrumental_variables,"formula")
           || twosls_instrumental_variables %in% formula_shortcuts,
           'The twosls_instrumental_variables should be a formula or one of %s!',
           deparse(formula_shortcuts))

    twosls_control <- list(
      "twosls_instrumental_variables"  = twosls_instrumental_variables,
      "twosls_decorrelate_instruments" = twosls_decorrelate_instruments,
      "twosls_reduce_pair_instruments" = twosls_reduce_pair_instruments)
    return(c(general_control,twosls_control))
  }

  # ---- ... likelihood -------------------------------------------------------
  assert_is_single_x(logdet_approx_order, "numeric")
  assert(logdet_approx_order >= 2, "The logdet_approx_order must be two or larger!")

  general_control <- c(general_control, list(
    "logdet_approx_order" = as.integer(logdet_approx_order)))

  # ---- ... ... mle ----------------------------------------------------------
  if (estimation_method == "mle") {

    assert_is_single_x(mle_optim_limit, "numeric")
    assert(mle_optim_limit > 0, "The mle_optim_limit must be positive!")

    available_hessians <- c("mixed","f2")
    assert(mle_hessian_method %in% available_hessians,
           'The approx_parameter_space must be a one string among c("%s")!',
           paste0(available_hessians, collapse = ", "))

    mle_control <- list(
      "mle_hessian_method" = mle_hessian_method,
      "mle_optim_limit" = mle_optim_limit)
    return(c(general_control,mle_control))
  }


  # ---- ... ... mcmc ---------------------------------------------------------
  if (estimation_method == "mcmc") {

    assert_is_single_x(mcmc_iterations, "numeric")
    assert_is_single_x(mcmc_burn_in, "numeric")
    assert_is_single_x(mcmc_resampling_limit, "numeric")

    assert(mcmc_burn_in < mcmc_iterations,
           "The mcmc_burn_in must be smaller than mcmc_iterations!")

    assert(all(c(mcmc_iterations, mcmc_burn_in, mcmc_resampling_limit) > 0),
           "All mcmc control parameters must be postive.")

    mcmc_control <- list(
      "mcmc_iterations"  = mcmc_iterations,
      "mcmc_burn_in" = mcmc_burn_in,
      "mcmc_resampling_limit" = mcmc_resampling_limit)
    return(c(general_control,mcmc_control))
  }
}



#' @title Derive additional information from the [spflow_control()] arguments
#' @description
#' This is an internal function to validates the content of the
#' `estimation_control` argument of [spflow()].
#' It also derives some verbal descriptions of the estimated model.
#' @keywords internal
#' @author Lukas Dargel
#' @return A list of control parameters
enhance_spflow_control <- function(estimation_control, is_within) {

  # validate by calling again
  estimation_control <- do.call("spflow_control", estimation_control)
  estimation_control <- c(
    estimation_control,
    "spatial_type" = sp_model_type(estimation_control),
    "is_within" = as.logical(is_within))

  if (!is_within) estimation_control[["use_intra"]] <- FALSE
  return(estimation_control)
}


