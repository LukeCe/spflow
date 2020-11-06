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
