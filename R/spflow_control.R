#' @title
#' Define details of the estimation procedure with the [spflow()] function.
#'
#' @description
#' This function creates a list to fine tune the estimation of a spatial
#' interaction model with [spflow()].
#' The options allow to adjust the estimation method and give the user full
#' control over the use of the explanatory variables.
#' The user can also adjust the form of autocorrelation to be considered.
#'
#' @section Details:
#'
#' ## Adjusting the form of autocorrelation
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
#'   A character indicating the model number,  indicating different spatial
#'   dependence structures (see documentation for details), should be one of
#'   `paste0("model_", 1:9)`
#' @param use_intra
#'   A logical which activates the option to use a separate set of parameters
#'   for intra-regional flows (origin == destination)
#' @param sdm_variables
#'   Either a formula or a character; the formula can be used to explicitly
#'   declare the variables in SDM specification, the character should be one of
#'   `c("same", "all")` which are short cuts for using all available variables
#'   or the same as used in the main formula provided to [spflow()]
#' @param weight_variable
#'   A character indicating the name of one column in the node pair data.
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
#' @param log_det_approx_order
#'   A numeric indicating the order of the Taylor expansion used to approximate
#'   the value of the log-determinant term.
#' @param log_det_simplify2cartesian
#'   A logical indicating whether the log-determinant approximation should be
#'   based on the spatial filter matrix of the cartesian model even when the
#'   actual flows are non-cartesian.
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
#'
#' @seealso [spflow()]
#' @references \insertAllCited{}
#' @return
#'   A list of parameters used to control the model estimation with
#'   [spflow()]
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
  weight_variable = NULL,
  approx_parameter_space = TRUE,
  fitted_value_method = "TS",
  approx_expectation = TRUE,
  expectation_approx_order = 10,
  log_det_approx_order = 10,
  log_det_simplify2cartesian = TRUE,
  mle_hessian_method = "mixed",
  mle_optim_limit = 100,
  mcmc_iterations = 5500,
  mcmc_burn_in = 2500,
  mcmc_resampling_limit = 100,
  twosls_instrumental_variables = "same",
  twosls_decorrelate_instruments = FALSE,
  twosls_reduce_pair_instruments = TRUE) {


  available_estimators <- c("s2sls", "mle","mcmc","ols")
  assert(estimation_method %in% available_estimators,
         'The estimation_method must one string among c("%s")!',
         paste0(available_estimators, collapse = ", "))

  possible_models <- ("model_" %p% 1:9)
  assert(model %in% possible_models,
         'The model must one string among c("%s")!',
         paste0(possible_models, collapse = ", "))

  if (estimation_method == "ols" | model == "model_1") {
    estimation_method <- "ols"
    model <- "model_1"
  }

  assert_is_single_x(use_intra, "logical")

  check_formula_msg <-
    "The %s must either be declared as a formula " %p%
    'or one string among c("none", "all", "same")!'
  assert(is(sdm_variables,"formula")
         || sdm_variables %in% c("none","same","all"),
         check_formula_msg, sdm_variables)

  assert(is_single_character(weight_variable) || is.null(weight_variable),
         "The weight_variable must be a character of length one!")

  # control parameter used in all cases
  general_control <- list(
    "estimation_method" = estimation_method,
    "model" = model,
    "use_intra" = use_intra,
    "sdm_variables" = sdm_variables,
    "weight_variable" = weight_variable
  )

  if (estimation_method == "ols")
    return(general_control)

  # ---- spatial models -------------------------------------------------------
  assert_is_single_x(approx_parameter_space, "logical")
  assert_is_single_x(approx_expectation, "logical")
  assert_is_single_x(expectation_approx_order, "numeric")
  assert_is_single_x(fitted_value_method, "character")
  assert_valid_case(fitted_value_method, c("TS","TC", "BP"))


  general_control <- c(general_control, list(
    "approx_parameter_space" = approx_parameter_space,
    "approx_expectation" = approx_expectation,
    "expectation_approx_order" = expectation_approx_order,
    "fitted_value_method" = fitted_value_method))

  # ---- ... s2sls -----------------------------------------------------------
  if (estimation_method == "s2sls") {
    assert_is_single_x(twosls_decorrelate_instruments, "logical")
    assert_is_single_x(twosls_reduce_pair_instruments, "logical")
    assert(is(twosls_instrumental_variables,"formula")
           || twosls_instrumental_variables %in% c("none","same","all"),
           check_formula_msg)

    twosls_control <- list(
      "twosls_instrumental_variables"  = twosls_instrumental_variables,
      "twosls_decorrelate_instruments" = twosls_decorrelate_instruments,
      "twosls_reduce_pair_instruments" = twosls_reduce_pair_instruments)
    return(c(general_control,twosls_control))
  }

  # ---- ... likelihood -------------------------------------------------------
  assert_is_single_x(log_det_approx_order, "numeric")
  assert_is_single_x(log_det_simplify2cartesian, "logical")
  assert(log_det_approx_order >= 2,
         "The log_det_approx_order must be two or larger!")

  general_control <- c(general_control, list(
    "log_det_approx_order" = as.integer(log_det_approx_order),
    "log_det_simplify2cartesian" = log_det_simplify2cartesian))

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

#' @title Enhance the flow_control object for estimation
#' @description
#' Validate the input to flow control and add additional information related
#' to the matrix form expression of the model.
#' @keywords internal
enhance_flow_control <- function(
  flow_control,
  net_pair) {

  # validate by calling again
  flow_control <- do.call("spflow_control", flow_control)
  flow_control <- c(
    flow_control,
    matrix_form_control(net_pair),
    list("spatial_type" = sp_model_type(flow_control)))

  if (flow_control$mat_nrows != flow_control$mat_ncols)
    flow_control$use_intra <- FALSE

  requires_logdet <- !is.null(flow_control$log_det_simplify2cartesian)
  if (requires_logdet & (flow_control$mat_complet == 1))
    flow_control$log_det_simplify2cartesian <- TRUE

  return(flow_control)
}
