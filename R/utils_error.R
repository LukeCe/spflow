# ---- assertions -------------------------------------------------------------
#' @title A set of functions to create standardized error messages
#' @description
#'   These are internal function for the developers of the package.
#'   The assertions are build on top of [sprintf()] to handle line breaks and
#'   automate insertion of arguments.
#' @return
#'   - The assertions are called for their side effect, they raise warnings or errors
#'   - The primitives return `TRUE` or `FALSE`
#' @keywords internal
#' @name assertions_and_primitives
#' @rdname assertions_and_primitives
#' @author Lukas Dargel
#' @examples
#'
#' \dontrun{
#' assert(FALSE, "My %s error message!", "nice")
#' }
#'
assert <- function(expr, error_msg = "ERROR", ..., warn = FALSE) {
  if (expr) {
    return(invisible(TRUE))
  }
  error_msg <- sprintfwrap(error_msg, ...)
  do.call(ifelse(warn,yes = "warning",no = "stop"),
          list(error_msg = error_msg, call. = FALSE))
  return(invisible(FALSE))
}

#' @rdname assertions_and_primitives
#' @keywords internal
assert_is <- function(x,class){
  assert(is(x,class),
         "The input argument %s must be of class %s!",
         deparse(substitute(x)),class)
}

#' @rdname assertions_and_primitives
#' @keywords internal
assert_is_one_of <- function(x,classes){
  assert(is_one_of(x,classes),
         "The input argument %s must be of class %s!",
         deparse(substitute(x)),paste(classes, collapse = " or "))
}

#' @rdname assertions_and_primitives
#' @keywords internal
assert_is_single_x <- function(x, class){
  is_single_class <- match.fun(paste0("is_single_", class))
  assert(is_single_class(x),
         "The input argument %s must be a %s of length one!",
         deparse(substitute(x)), class)
}

#' @rdname assertions_and_primitives
#' @keywords internal
assert_inherits <- function(x, class){
  assert(inherits(x, class),
         "The input argument %s must inherit from class %s!",
         deparse(substitute(x)),class)
}

#' @rdname assertions_and_primitives
#' @keywords internal
assert_valid_option <- function(x, options) {
  assert(all(x %in% options),
         'The %s argument must be one of %s!',
         deparse(substitute(x)), deparse(options))
}

# ---- primitives -------------------------------------------------------------
#' @keywords internal
#' @rdname assertions_and_primitives
is_one_of <- function(x, classes) {
  return(any(class(x) %in% classes))
}

#' @rdname assertions_and_primitives
#' @keywords internal
is_column_subset <- function(
    df1,
    df2,
    interger_tolerance = TRUE,
    factor_tolerance = TRUE) {

  if (!all(colnames(df2) %in% colnames(df1)))
    return(FALSE)

  df2 <- as.data.frame(df2)[0,,drop = FALSE]
  df1 <- as.data.frame(df1)[0,colnames(df2),drop = FALSE]

  if (interger_tolerance) {
    df2 <- integercols2numeric(df2)
    df1 <- integercols2numeric(df1)
  }

  if (factor_tolerance) {
    df2 <- factorcols2character(df2)
    df1 <- factorcols2character(df1)
  }

  row.names(df2) <- row.names(df1) <- NULL
  identical(df2, df1, attrib.as.set = FALSE)
}

#' @rdname assertions_and_primitives
#' @keywords internal
is_single_character <- function(x) {
  is.character(x) && (length(x) == 1L)
}

#' @rdname assertions_and_primitives
#' @keywords internal
is_single_logical <- function(x) {
  is.logical(x) && (length(x) == 1L)
}

#' @rdname assertions_and_primitives
#' @keywords internal
is_single_numeric <- function(x) {
  is.numeric(x) && (length(x) == 1L)
}

#' @rdname assertions_and_primitives
#' @keywords internal
has_equal_elements <- function(x) {
  length(unique(x)) <= 1
}

#' @rdname assertions_and_primitives
#' @keywords internal
has_distinct_elements <- function(x) {
  length(unique(x)) == length(x)
}

#' @rdname assertions_and_primitives
#' @keywords internal
none <- function(..., na.rm = FALSE){
  !any(..., na.rm = FALSE)
}


# ---- helpers ----------------------------------------------------------------
#' @keywords internal
integercols2numeric <- function(df) {
  integer_cols <- sapply(df, is.integer)
  if (none(integer_cols))
    return(df)
  df[integer_cols] <- lapply(df[integer_cols], as.numeric)
  df
}

#' @keywords internal
factorcols2character <- function(df) {
  factor_cols <- sapply(df, is.factor)
  if (none(factor_cols))
    return(df)
  df[factor_cols] <- lapply(df[factor_cols], as.character)
  df
}
