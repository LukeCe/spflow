#' Define details of the estimation procedure with the [spflow()] function.
#'
#' @description
#' This function creates a list to fine tune the estimation of a spatial
#' interaction model with [spflow()].
#'
#' @param estimation_method
#'   A character which indicates the estimation method, should be one of
#'   `c("mle","s2sls","mcmc")`
#' @param model
#'   A character indicating the model number,  indicating different spatial
#'   dependence structures (see documentation for details), should be one of
#'   `paste0("model_", 1:9)`
#' @param hessian_method
#'   A character which indicates the method for hessian calculation
#' @param sdm_variables
#'   Either a formula or a character; the formula can be used to explicitly
#'   declare the variables in SDM specification, the character should be one of
#'   `c("same", "all")` which are short cuts for using all available variables
#'   or the same as used in the main formula provided to [spflow()]
#' @param instrumental_variables
#'   Either a formula or a character; the formula can be used to explicitly
#'   declare the variables that should be used as instruments during S2SLS
#'   estimation, the character should be one of `c("same", "all")` which
#'   are short cuts for using all available variables or the same as used in
#'   the main formula provided to [spflow()]
#' @param decorrelate_instruments
#'   A logical whether to perform a PCA to remove (linear) correlation from the
#'   instruments generated for the S2SLS estimator
#' @param use_sdm
#'   A logical which adds spatial lags of origin and destination attributes as
#'   explanatory variables to the model.
#' @param use_intra
#'   A logical which activates the option to use a separate set of parameters
#'   for intra-regional flows (origin == destination)
#'
#' @examples
#' # default is MLE estimation of the most comprehensive model
#' spflow_control()
#'
#'
#' @seealso spflow
#' @return A list of control parameters for estimation via [spflow()]
#' @export
spflow_control <- function(
  estimation_method = "mle",
  model = "model_9",
  formulation = "matrix",
  use_intra = TRUE,
  use_sdm = TRUE,
  sdm_variables = "same",
  instrumental_variables = "same",
  weight_variable = NULL,
  decorrelate_instruments = FALSE,
  reduce_pair_instruments = TRUE,
  hessian_method = "mixed",
  flow_type = NULL) {

  available_estimators <- c("s2sls", "mle","mcmc","ols")
  assert(estimation_method %in% available_estimators,
         "The estimation method must be one of [%s]!" %>%
           sprintf(., paste(available_estimators, collapse = " or ")))

  possible_models <- ("model_" %p% 1:9)
  assert(model %in% possible_models,
         "The model can only be one of:\\n" %p%
           paste0(possible_models,collapse =  "\\n"))

  # model 1 is always ols
  if (estimation_method == "ols" | model == "model_1") {
    estimation_method <- "ols"
    model <- "model_1"
  }

  possible_formulations <- c("matrix", "vector")
  assert(formulation %in% possible_formulations,
         "The estimation method must be one of [%s]!" %>%
           sprintf(., paste(possible_formulations, collapse = " or ")))

  assert_is_single_x(use_intra, "logical")
  assert_is_single_x(use_sdm, "logical")
  assert_is_single_x(decorrelate_instruments, "logical")
  assert_is_single_x(reduce_pair_instruments, "logical")


  # check sdm variables
  if (!use_sdm)
    sdm_variables <- "none"

  assert(is(sdm_variables,"formula")
         || sdm_variables %in% c("none","same","all"),
         "The sdm_variables must either be declared as a formula " %p%
         "or as a string with one of the keywords [none, or all, or same]!")

  # check instrumental variables
  if (estimation_method != "s2sls")
    instrumental_variables <- "none"

  assert(is(instrumental_variables,"formula")
         || instrumental_variables %in% c("none","same","all"),
         "The instrumental_variables must either be declared as a formula " %p%
           "or as a string with one of the keywords [none, or all, or same]!")

  assert(is_single_character(weight_variable) || is.null(weight_variable),
         "The weight_variable must be a character of length one!")

  # check hessian (only relevant for mle estimator)
  if (estimation_method != "mle")
    hessian_method <- "mixed"

  available_hessians <- c("mixed","f2") # ,... exact
  assert(hessian_method %in% available_hessians,
         "The hessian method must be one of [%s]!" %>%
           sprintf(., paste(available_hessians, collapse = " or ")))

  # check flow types
  between_flows <- !is.null(flow_type) && (flow_type == "between")
  impossible_intra <- use_intra && between_flows

  assert(!impossible_intra, warn = TRUE,
         "Intra model option is only available for within network flows." %p%
         "The option was deactivated.")

  return(list(
    "estimation_method" = estimation_method,
    "hessian_method" = hessian_method,
    "sdm_variables" = sdm_variables,
    "instrumental_variables" = instrumental_variables,
    "decorrelate_instruments" = decorrelate_instruments,
    "reduce_pair_instruments" = reduce_pair_instruments,
    "use_intra" = use_intra,
    "use_sdm" = use_sdm,
    "model" = model,
    "formulation" = formulation,
    "flow_type" = flow_type
  ))
}

# TODO create validate_control function
