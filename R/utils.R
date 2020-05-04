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

savely_to_list <- function(obj) {
  if (is.list(obj)) return(obj)
  else return(list(obj))
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

replace_empty <- function(.x , .replace) {
  sub("^$", .replace, .x )
}

replace_NA_chr <- function(.x , .replace) {
  sub("NA.", .replace, .x )
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

"%||%" <- function(x, y) {
  if (is.null(x)) y else x
}

"%|!|%" <- function(x, y) {
  if (is.null(x)) x else y
}


# indexation ------------------------------------------------------------------
pull_slot <- function(.slot,.obj) {
  slot(.obj,.slot)
}

pull_slots <- function(.obj, .slots) {
  lapply(.slots, pull_slot, .obj)
}


# primitives ------------------------------------------------------------------
has_equal_elements <- function(obj) {
  length(unique(obj)) <= 1
}

has_distinct_elements <- function(obj) {
  length(unique(obj)) == length(obj)
}

is_one_of <- function(.obj, .classes) {
  return(any(class(.obj) %in% .classes))
}
