# ---- magrittr pipes ---------------------------------------------------------

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Exposing pipe
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%$\%}} for details.
#'
#' @name %$%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %$%
#' @usage lhs \%$\% rhs
NULL

# ---- Elvis pipes ------------------------------------------------------------
#' Elvis operators
#'
#' Infix operators that reduce cyclomatic complexity of the source code.
#' In other words you will need fewer nested `if` statements.
#'
#' @param x A value or Null
#' @param y alternative return
#'
#' @return x or y
#' @keywords internal
#' @name elvis_operators
"%||%" <- function(x, y) {
  if (is.null(x)) y else x
}

#' @keywords internal
#' @rdname elvis_operators
"%|!|%" <- function(x, y) {
  if (is.null(x)) NULL else y
}

#' @keywords internal
#' @rdname elvis_operators
"%|0|%" <- function(x, y) {
  if (length(x) == 0) y else x
}

#' @keywords internal
#' @rdname elvis_operators
"%|!0|%" <- function(x, y) {
  if (length(x) != 0) y else NULL
}

#' @keywords internal
#' @rdname elvis_operators
"%T%" <- function(x, y) {
  if (y) x else NULL
}

# ---- String pipe ------------------------------------------------------------
#' @keywords internal
'%p%' <- function(x, y) {
  paste0(x,y)
}
