# assertions ------------------------------------------------------------------
assert <- function(expr, error_msg, warn = FALSE) {
  if (expr) {
    return(invisible(TRUE))
  }

  do.call(ifelse(warn,yes = "warning",no = "stop"),
          list(error_msg = error_msg, call. = FALSE))
}

# classes ---------------------------------------------------------------------
class_union_null <- function(class) {
  new <- "maybe_" %p% class
  setClassUnion(new, members = c(class,"NULL"))
}

savely_as <- function(obj, class, ...) {
  if (is.null(obj))
    return(NULL)

  return(as(obj,class, ...))
}

try_coercion <- function(obj,class) {

  obj_as_class <- try(savely_as(obj,class),silent = TRUE)
  name_obj <- deparse(substitute(obj))

  assert(class(obj_as_class) != "try-error",
         sprintf("Object [%s] must be coercible to a [%s]!",
                 name_obj,
                 class))

  return(obj_as_class)
}

# strings ---------------------------------------------------------------------
'%p%' <- function(x, y) {
  paste0(x,y)
}

# pipe ------------------------------------------------------------------------
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


# primitives ------------------------------------------------------------------
has_equal_elements <- function(obj) {
  length(unique(obj)) <= 1
}
