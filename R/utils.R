# assertions ------------------------------------------------------------------
assert <- function(expr, error_msg, warn = FALSE) {
  if (expr) {
    return(invisible(TRUE))
  }
  do.call(ifelse(warn,yes = "warning",no = "stop"),
          list(error_msg = error_msg, call. = FALSE))
}

assert_valid_case <- function(argument,cases) {
  assert(all(argument %in% cases),
         "The what argument can only be a subset of the character vector [%s]!" %>%
           sprintf(.,deparse(cases)))
}

# classes ---------------------------------------------------------------------
class_union_null <- function(class) {
  new <- "maybe_" %p% class
  setClassUnion(new, members = c(class,"NULL"))
}

coerce_to <- function(obj, class, ...) {

  assert(canCoerce(obj, class),
         "Object [%s] must be coercible to a [%s]!" %>%
           sprintf(.,
                   deparse(substitute(obj,parent.frame())),
                   class))

  return(as(obj,class,...))

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

setGenericVerif <- function(x,y) {
  if ( !isGeneric(x))  setGeneric(x,y)
}

try_coercion <- function(obj,class) {

  obj_as_class <- try(savely_as(obj,class),silent = TRUE)

  assert(!"try-error" %in% class(obj_as_class),
         sprintf("Object [%s] must be coercible to a [%s]!",
                 deparse(substitute(obj,parent.frame())),
                 class))

  return(obj_as_class)
}

# naming ----------------------------------------------------------------------
named_list <- function(names, init = NULL) {

  named_list <- vector("list", length(names))
  names(named_list) <- names
  named_list[] <- list(init)

  return(named_list)
}

get_all_var_names <- function(f) {
  labels(terms(tt$interactions))
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

# factors ---------------------------------------------------------------------
factor_in_order <- function(x) {
  factor(x,levels = as.character(unique(x)))
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
