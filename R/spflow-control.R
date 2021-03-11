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
#' @param weight_variable
#'   A character indicating the name of a variable that should be used to
#'   weight the origin-destination pairs
#' @param reduce_pair_instruments
#'   A logical that indicates whether the number of instruments that are
#'   derived from pair attributes should be reduced or not (default is TRUE
#'   because constructing these instruments is often the most demanding part of
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
  use_sdm = TRUE,
  sdm_variables = "same",
  instrumental_variables = "same",
  weight_variable = NULL,
  decorrelate_instruments = FALSE,
  reduce_pair_instruments = TRUE,
  hessian_method = "mixed") {

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
  # can be within or between once the rectangular case is available
  flow_type <- NULL
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
    "model" = model
  ))
}

# TODO create validate_control function

#' @keywords internal
sp_model_type <- function(cntrl) {

  has_lagged_x <- cntrl$use_sdm & cntrl$sdm_variables != "none"

  if (cntrl$model == "model_1") {
    model_type <- ifelse(has_lagged_x,"SLM","OLM")
  }

  if (cntrl$model != "model_1") {
    model_type <- ifelse(has_lagged_x,"SDM","LAG")
  }

  return(model_type)

}
