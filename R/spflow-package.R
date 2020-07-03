#' spflow: A package for spatial econometric interaction models.
#'
#' The foo package provides function to estimate origin destination flows with
#' that exhibit spatial autocorrelation.
#'
#'
#' @section Network data:
#' The package provides a new class for representing network data.
#'
#' @section Model estimation:
#' The package provides functions to estimate spatial interaction models.
#'
#' @docType package
#' @import Matrix
#' @importFrom methods as canCoerce is isGeneric new setClassUnion setGeneric slot validObject
#' @importFrom stats as.formula contrasts formula model.matrix optim predict pt quantile reformulate rgamma rnorm runif sd terms terms.formula
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

# A trick to use the x %>% fun(.) notation without R CMD Note
utils::globalVariables(c(".","dest_id","orig_id"))
