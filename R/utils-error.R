# ---- assertions -------------------------------------------------------------
#' @keywords internal
assert <- function(expr, error_msg = "ERROR", warn = FALSE) {
  if (expr) {
    return(invisible(TRUE))
  }
  do.call(ifelse(warn,yes = "warning",no = "stop"),
          list(error_msg = error_msg, call. = FALSE))
}

#' @keywords internal
assert_is <- function(obj,.class){
  assert(is(obj,.class),
         sprintf("The input argument %s must be of class %s!",
                 deparse(substitute(obj)),.class))
}

#' @keywords internal
assert_is_one_of <- function(obj,.classes){
  assert(is_one_of(obj,.classes),
         sprintf("The input argument %s must be of class %s!",
                 deparse(substitute(obj)),paste(.classes, collapse = " or ")))
}

#' @keywords internal
assert_is_single_x <- function(obj,x){
  is_single_x <- match.fun("is_single_" %p% x)
  assert(is_single_x(obj),
         sprintf("The input argument %s must be a %s of length one!",
                 deparse(substitute(obj)),x))
}

# ---- primitives -------------------------------------------------------------
#' @keywords internal
is_one_of <- function(.obj, .classes) {
  return(any(class(.obj) %in% .classes))
}

#' @keywords internal
is_single_character <- function(x) {
  is.character(x) && (length(x) == 1L)
}

#' @keywords internal
is_single_logical <- function(x) {
  is.logical(x) && (length(x) == 1L)
}
