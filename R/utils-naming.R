#' @export
drop_lnames <- function(.obj){
  .obj %>% set_lnames(NULL)
}

#' @importFrom data.table setattr
#' @export
set_lnames <- function(.obj, value){
  .obj %>% setattr("names",value)
}

#' @export
load_as <- function(file){
  load(file)
  get(ls()[ls() != "file"])
}

#' @export
lookup <- function(values, names = as.character(values)) {
  pair_nv <- data.frame(v = values, n = names)
  values %|!|% structure(pair_nv$v, names = pair_nv$n)
}

#' @export
list_lookup <- function(values, names = as.character(values)) {
  as.list(lookup(names = names,names))
}
