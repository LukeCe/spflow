# ---- infix operators --------------------------------------------------------
#' @title Additional infix operators for more concise code
#' @keywords internal
#' @rdname infix_operators
#' @author Lukas Dargel
'%p%' <- function(x, y) {
  paste0(x,y)
}

#' @keywords internal
#' @rdname infix_operators
"%T%" <- function(x, y) {
  if (isTRUE(y)) x else NULL
}

#' @keywords internal
#' @rdname infix_operators
"%||%" <- function(x, y) {
  if (length(x) == 0) y else x
}

#' @keywords internal
#' @rdname infix_operators
"%|!|%" <- function(x, y) {
  if (length(x) != 0) y else NULL
}

# ---- list operations --------------------------------------------------------
#' @title
#' Additional list operators that allow a more concise and
#' functional programming style
#' @keywords internal
#' @rdname list_operators
#' @author Lukas Dargel
translist <- function(.l) {

  all_inner_names <- Reduce("c", lapply(.l, "names"))
  all_inner_names <- Reduce("c", all_inner_names)
  all_inner_names <- unique(all_inner_names)

  .l_ordered <- lapply(.l, "[", all_inner_names)

  result <- lapply(seq_along(all_inner_names),
                   function(i) lapply(.l_ordered, .subset2, i))

  names(result) <-  all_inner_names
  result <- lapply(result, "compact")

  return(result %||% NULL)
}

#' @keywords internal
#' @rdname list_operators
compact <- function(.x) {
  Filter(length, .x)
}

#' @keywords internal
#' @rdname list_operators
ilapply <- function(.list, .f, MoreArgs = NULL) {
  i_arg <- names(.list) %||% seq_along(.list)
  mapply(.f, .list, i_arg, MoreArgs = MoreArgs,SIMPLIFY = FALSE)
}

#' @keywords internal
#' @rdname list_operators
ulapply <- function(.list, .f, ...,recursive = TRUE, use.names = FALSE) {
  unlist(lapply(.list, match.fun(.f), ...), recursive, use.names)
}


#' @keywords internal
#' @rdname list_operators
flatlist <- function(lst, use.names = TRUE) {
  c2 <- function(...) c(..., use.names = use.names)
  do.call(c2, lapply(lst, function(x) if(is.list(x)) flatlist(x) else list(x)))
}

# ---- lookups and naming -----------------------------------------------------
#' @title
#' Some helpers to create lookups that allow to program with key value pairs
#' @rdname lookup_operators
#' @keywords internal
#' @author Lukas Dargel
named_list <- function(names, init = NULL) {

  named_list <- vector("list", length(names))
  names(named_list) <- names
  named_list[] <- list(init)

  return(named_list %||% NULL)
}

#' @rdname lookup_operators
#' @keywords internal
lookup <- function(values, names = as.character(values)) {
  pair_nv <- data.frame(v = values, n = names)
  values %|!|% structure(pair_nv$v, names = pair_nv$n)
}

#' @rdname lookup_operators
#' @keywords internal
list_lookup <- function(values, names = as.character(values)) {
  as.list(lookup(names = names,values))
}

#' @rdname lookup_operators
#' @keywords internal
collect <- function(names){
  collection <- lapply(names, get, envir = parent.frame(1))
  names(collection) <- names

  return(collection)
}

#' @rdname lookup_operators
#' @keywords internal
sort_names <- function(x) {
  x[order(names(x))]
}

# ---- maybe ------------------------------------------------------------------
#' @title Functions that replace errors by `NULL`
#' @rdname maybe_operators
#' @keywords internal
#' @author Lukas Dargel
maybe <- function(expr){
  result <- try(expr,silent = TRUE)
  if (is(result,"try-error")) NULL else result
}
