#' @keywords internal
collect <- function(names){
  collection <- lapply(names, get, envir = parent.frame(1))
  names(collection) <- names

  return(collection)
}

#' @keywords internal
compact <- function(.x) {
  Filter(length, .x)
}

#' @keywords internal
flatten <- function(..., use.names = TRUE) {
  c(..., recursive = TRUE, use.names = use.names)
}

#' @keywords internal
flatlist <- function(lst, use.names = TRUE) {
  c2 <- function(...) c(..., use.names = use.names)
  do.call(c2, lapply(lst, function(x) if(is.list(x)) flatlist(x) else list(x)))
}

#' @keywords internal
lfilter <- function(lst, .f) {
  Filter(match.fun(.f), lst)
}

#' @keywords internal
lreduce <- function(lst, .f, ...) {
  .f <- match.fun(.f)
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, lst)
}

#' @keywords internal
named_list <- function(names, init = NULL) {

  named_list <- vector("list", length(names))
  names(named_list) <- names
  named_list[] <- list(init)

  return(named_list)
}

#' @keywords internal
none <- function(x){
  all(!x)
}

#' A less verbose `mapply()`
#'
#' @param ... Named arguments of the same length
#' @param .f A function
#' @param fix_args A list of named arguments which are held constant
#'
#' @family FP
#' @keywords internal
plapply <- function(..., .f, fix_args = NULL){
  mapply(FUN = match.fun(.f), ..., MoreArgs = fix_args,
         SIMPLIFY = FALSE, USE.NAMES = TRUE)
}

#' @keywords internal
translist <- function(.l) {

  all_inner_names <-
    lreduce(lapply(.l, names),c) %>%
    lreduce(c) %>%
    unique()

  .l_ordered <- lapply(.l, "[", all_inner_names)

  result <-
    lapply(seq_along(all_inner_names),
           function(i) lapply(.l_ordered, .subset2, i))

  names(result) <-  all_inner_names

  return(lapply(result, compact))
}

#' @keywords internal
safely_to_list <- function(obj) {
  if (is.list(obj)) return(obj)
  else return(list(obj))
}
