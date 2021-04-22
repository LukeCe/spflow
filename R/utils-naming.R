#' @keywords internal
drop_lnames <- function(.obj){
  set_lnames(.obj, NULL)
}

#' @importFrom data.table setattr
#' @keywords internal
set_lnames <- function(.obj, value){
  setattr(.obj, "names",value)
}

#' @keywords internal
load_as <- function(file){
  load(file)
  get(ls()[ls() != "file"])
}

#' @keywords internal
lookup <- function(values, names = as.character(values)) {
  pair_nv <- data.frame(v = values, n = names)
  values %|!|% structure(pair_nv$v, names = pair_nv$n)
}

#' @keywords internal
list_lookup <- function(values, names = as.character(values)) {
  as.list(lookup(names = names,values))
}

#' @keywords internal
sort_names <- function(x) {
  x[order(names(x))]
}
