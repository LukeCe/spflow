#' Define details of the estimation procedure with the [spflow()] function.
#'
#' @description
#' This function creates a list to fine tune the estimation of a spatial
#' interaction model with [spflow()].
#'
#' @param estimation_method A character which indicates the estimation method
#' @param model A character indicating the model; one of paste0("model_", 1:9).
#' @param formulation A character indicating the formulation; "matrix" or "vector"
#' @param hessian_method A character which indicates the method for hessian calculation
#' @param sdm_variables A formula which can be used explicitly declare the variables that should be used as sdm variables.
#' @param instrumental_variables A formula which can be used explicitly declare the variables that should be used as instruments during s2sls estimation.
#' @param decorrelate_instruments A logical whether to perform a procedure that removes (linear) correlation from the instruments.
#' @param use_sdm A logical which adds spatial lags of origin and destination attributes as explanatory variables to the model.
#' @param use_intra A logical which adds separate set of coefficients for intra-observational flows (origin == destination) to the model.
#' @param flow_type A character indicating the type of flows; "within" or "between"
#'
#' @seealso spflow
#'
#' @return A list of control parameters
#' @export
spflow_control <- function(
  estimation_method = "s2sls",
  model = "model_9",
  formulation = "matrix",
  use_intra = TRUE,
  use_sdm = TRUE,
  sdm_variables = "same",
  instrumental_variables = "same",
  decorrelate_instruments = FALSE,
  hessian_method = "mixed",
  flow_type = NULL) {

  available_estimators <- c("s2sls", "mle","mcmc")
  assert(estimation_method %in% available_estimators,
         "The estimation method must be one of [%s]!" %>%
           sprintf(., paste(available_estimators, collapse = " or ")))

  possible_models <- ("model_" %p% 1:9)
  assert(model %in% possible_models,
         "The model can only be one of:\\n" %p%
           paste0(possible_models,collapse =  "\\n"))

  possible_formulations <- c("matrix", "vector")
  assert(formulation %in% possible_formulations,
         "The estimation method must be one of [%s]!" %>%
           sprintf(., paste(possible_formulations, collapse = " or ")))

  assert(is_single_logical(use_intra),
         "The use_intra option must be a logical!")

  assert(is_single_logical(use_sdm),
         "The use_sdm option must be a logical!")

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
    "use_intra" = use_intra,
    "use_sdm" = use_sdm,
    "model" = model,
    "formulation" = formulation,
    "flow_type" = flow_type
  ))
}
