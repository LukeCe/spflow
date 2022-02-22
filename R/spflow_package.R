# ---- pkg_doc ----------------------------------------------------------------

#' @title Spatial econometric interaction models in R
#'
#' @description
#' The `spflow` package provides function to estimate models of
#' origin-destination flows with spatial autocorrelation.
#' The implementation exploits the efficiency gains of the matrix formulation
#' that avoids duplication of the data that describes the origins and
#' the destinations.
#' It offers new data structures and a convenient formula interface
#' that allows to estimate the models with maximal efficiency and minimal
#' effort for the user.
#' The estimators are detailed in \insertCite{Dargel2021;textual}{spflow}.
#'
#'
#' @section Network data:
#' The package provides a new class for representing network data.
#' [These classes][spflow_network_classes()] help to exploit the relational
#' structure of the data and ensure that all origins and destinations are
#' correctly identified.
#'
#'
#' @section Model estimation:
#' The [spflow()] function is the main function for estimation and can be used
#' out of the box using the default configuration, which corresponds to the
#' most general model possible.
#' Fine grained control over the estimation procedure is given via the
#' [spflow_control()] argument through which the user can change, for example,
#' the estimation method or the way in which the covariates should be used.
#'
#'
#' @aliases spflow-package
#' @docType package
#' @importClassesFrom Matrix Matrix
#' @importFrom utils head tail
#' @importFrom methods as canCoerce is isGeneric new setClassUnion setGeneric
#'   slot validObject
#' @importFrom stats as.formula contrasts cor formula model.matrix optim predict pt
#'   quantile reformulate rgamma rnorm runif sd terms terms.formula
#' @importFrom Rdpack reprompt
#' @keywords internal
#' @references \insertAllCited{}
"_PACKAGE"

# ---- pkg_deprected ----------------------------------------------------------

#' @title Deprecated functions in package \pkg{spflow}.
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("<function>-deprecated")}.
#' @name spflow-deprecated
#' @keywords internal
NULL

# ---- pkg_defunct ------------------------------------------------------------

#' @title Defunct functions in package \pkg{spflow}.
#' @description The functions listed below are defunct. When possible,
#'   alternative functions with similar functionality are also mentioned. Help
#'   pages for deprecated functions are available at
#'   \code{help("<function>-defunct")}.
#' @name spflow-deprecated
#' @keywords internal
NULL

# ---- other ------------------------------------------------------------------

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL



# A trick to use the x %>% fun(.) notation without R CMD Note
utils::globalVariables(c(".","ORIG_ID","DEST_ID","ID"))
