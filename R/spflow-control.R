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
#' @section Adjusting the form of autocorrelation:
#'
#' The option `model` allows to declare one of nine different forms of
#' autocorrelation that follow the naming convention of
#' \insertCite{LeSage2008;textual}{spflow}.
#' The most general specification is "model_9" and all other correspond to
#' special cases of this one.
#' The different sub models are summarized in this table.
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
#' @param parameter_space
#'   A character indicating how to define the limits of the parameter space.
#'   The only available option is `c("approx")`.
#' @param mle_hessian_method
#'   A character which indicates the method for Hessian calculation
#' @param twosls_instrumental_variables
#'   Either a formula or a character; the formula can be used to explicitly
#'   declare the variables that should be used as instruments during S2SLS
#'   estimation, the character should be one of `c("same", "all")` which
#'   are short cuts for using all available variables or the same as used in
#'   the main formula provided to [spflow()]
#' @param twosls_decorrelate_instruments
#'   A logical whether to perform a PCA to remove (linear) correlation from the
#'   instruments generated for the S2SLS estimator
#' @param twosls_reduce_pair_instruments
#'   A logical that indicates whether the number of instruments that are
#'   derived from pair attributes should be reduced or not (default is TRUE
#'   because constructing these instruments is often the most demanding part of
#' @param mcmc_iterations
#'   A numeric indicating
#' @param mcmc_burn_in
#' @param mcmc_resampling_limit
#'   the estimation.)
#' @seealso [spflow()]
#' @references \insertAllCited{}
#' @return A list of control parameters for estimation via [spflow()]
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
#' custom_control <- spflow_control(use_intra = FALSE, use_sdm = FALSE)
#' custom_control <- spflow_control(use_intra = FALSE, sdm_variables = "none")
spflow_control <- function(
  estimation_method = "mle",
  model = "model_9",
  use_intra = TRUE,
  sdm_variables = "same",
  weight_variable = NULL,
  parameter_space = "approx",
  mle_hessian_method = "mixed",
  twosls_instrumental_variables = "same",
  twosls_decorrelate_instruments = FALSE,
  twosls_reduce_pair_instruments = TRUE,
  mcmc_iterations = 5500,
  mcmc_burn_in = 2500,
  mcmc_resampling_limit = 100) {


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

  general_control <- list(
    "estimation_method" = estimation_method,
    "model" = model,
    "use_intra" = use_intra,
    "sdm_variables" = sdm_variables,
    "weight_variable" = weight_variable
  )

  if (estimation_method == "ols")
    return(general_control)

  ps_methods <- c("approx")
  assert(parameter_space %in% ps_methods,
         'The parameter_space must be a one string among c("%s")!',
         paste0(ps_methods, collapse = ", "))
  general_control <-
    c(general_control, list("parameter_space" = parameter_space))

  if (estimation_method == "mle") {
    available_hessians <- c("mixed","f2")
    assert(mle_hessian_method %in% available_hessians,
           'The parameter_space must be a one string among c("%s")!',
           paste0(available_hessians, collapse = ", "))

    mle_control <- list("mle_hessian_method" = mle_hessian_method)
    return(c(general_control,mle_control))
  }

  if (estimation_method == "s2sls") {
    assert_is_single_x(twosls_decorrelate_instruments, "logical")
    assert_is_single_x(twosls_reduce_pair_instruments, "logical")
    assert(is(twosls_instrumental_variables,"formula")
           || twosls_instrumental_variables %in% c("none","same","all"),
           check_vars_msg)

    twosls_control <- list(
      "twosls_instrumental_variables"  = twosls_instrumental_variables,
      "twosls_decorrelate_instruments" = twosls_decorrelate_instruments,
      "twosls_reduce_pair_instruments" = twosls_reduce_pair_instruments
    )
    return(c(general_control,twosls_control))
  }

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
      "mcmc_resampling_limit" = mcmc_resampling_limit
    )
    return(c(general_control,mcmc_control))
  }
}
