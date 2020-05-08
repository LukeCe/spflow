#' Create a control object for the estimation with [spflow()]
#'
#' This function creates a list to fine tune the estimation of a spatial
#' interaction model with [spflow()].
#'
#' @param estimation_method A character which indicates the estimation method
#' @param hessian_method A character which indicates the method for hessian calculation
#' @param sdm_variables A formula which can be used explicitly delcare the variables that should be used as sdm variables.
#' @param instrumental_variables A formula which can be used explicitly delcare the variables that should be used as instruments during s2sls estimation.
#'
#' @seealso spflow
#'
#' @return A list of control parameters
#' @export
spflow_control <- function(
  estimation_method = "s2sls",
  hessian_method = "mixed",
  sdm_variables = "same",
  instrumental_variables = "same"
) {

  # check estimator
  available_estimators <- c("s2sls") # mle, mcmc
  assert(estimation_method %in% available_estimators,
         "The estimation method must be one of [%s]!" %>%
           sprintf(., paste(available_estimators, collapse = " or ")))

  # check hessian (only relevant for mle estimator)
  if (estimation_method != "mle")
    hessian_method <- "mixed"

  available_hessians <- c("mixed") # finite_diff, exact
  assert(hessian_method %in% available_hessians,
         "The estimation method must be one of [%s]!" %>%
           sprintf(., paste(available_estimators, collapse = " or ")))

  # check sdm variables
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

  return(list(
    "estimation_method" = estimation_method,
    "hessian_method" = hessian_method,
    "sdm_variables" = sdm_variables,
    "instrumental_variables" = instrumental_variables
  ))
}
