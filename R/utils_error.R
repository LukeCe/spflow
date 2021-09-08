# ---- assertions -------------------------------------------------------------
#' @title A set of functions to create customized error messages
#' @description The functions are build on top of [sprintf()]
#' @return Functions called for their side effect, raises warning or error
#' @keywords internal
#' @name assert_errors
#' @rdname assert_errors
#' @author Lukas Dargel
assert <- function(expr, error_msg = "ERROR", ..., warn = FALSE) {
  if (expr) {
    return(invisible(TRUE))
  }
  error_msg <- sprintf(error_msg, ...)
  do.call(ifelse(warn,yes = "warning",no = "stop"),
          list(error_msg = error_msg, call. = FALSE))
}

#' @rdname assert_errors
#' @keywords internal
assert_is <- function(obj,.class){
  assert(is(obj,.class),
         "The input argument %s must be of class %s!",
         deparse(substitute(obj)),.class)
}

#' @rdname assert_errors
#' @keywords internal
assert_inherits <- function(obj,.class){
  assert(inherits(obj,.class),
         "The input argument %s must inherit from class %s!",
         deparse(substitute(obj)),.class)
}

#' @rdname assert_errors
#' @keywords internal
assert_is_one_of <- function(obj,.classes){
  assert(is_one_of(obj,.classes),
         "The input argument %s must be of class %s!",
         deparse(substitute(obj)),paste(.classes, collapse = " or "))
}

#' @rdname assert_errors
#' @keywords internal
assert_is_single_x <- function(obj, x){
  is_single_x <- match.fun("is_single_" %p% x)
  assert(is_single_x(obj),
         "The input argument %s must be a %s of length one!",
         deparse(substitute(obj)), x)
}

#' @rdname assert_errors
#' @keywords internal
assert_valid_case <- function(argument,cases) {
  assert(all(argument %in% cases),
         "The %s argument must be a subset of the character vector [%s]!",
         deparse(substitute(argument)), deparse(cases))
}

# ---- primitives -------------------------------------------------------------
#' @title A set of primitive functions to test conditions
#' @keywords internal
#' @return A logical
#' @name custom_primitives
#' @rdname custom_primitives
#' @author Lukas Dargel
is_one_of <- function(.obj, .classes) {
  return(any(class(.obj) %in% .classes))
}

#' @rdname custom_primitives
#' @keywords internal
is_single_character <- function(x) {
  is.character(x) && (length(x) == 1L)
}

#' @rdname custom_primitives
#' @keywords internal
is_single_logical <- function(x) {
  is.logical(x) && (length(x) == 1L)
}

#' @rdname custom_primitives
#' @keywords internal
is_single_numeric <- function(x) {
  is.numeric(x) && (length(x) == 1L)
}

#' @rdname custom_primitives
#' @keywords internal
has_equal_elements <- function(obj) {
  length(unique(obj)) <= 1
}

#' @rdname custom_primitives
#' @keywords internal
has_distinct_elements <- function(obj) {
  length(unique(obj)) == length(obj)
}

#' @rdname custom_primitives
#' @keywords internal
none <- function(x){
  all(!x)
}
