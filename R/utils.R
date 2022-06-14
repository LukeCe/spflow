#' @keywords internal
factor_in_order <- function(x) {
  factor(x,levels = as.character(unique(x)))
}

#' @keywords internal
sequentialize_index <- function(index_list) {
  len <- unlist(lapply(index_list, length))
  shift <- cumsum(c(0,len))[1:length(index_list)]
  Map("+", index_list, as.list(shift))
}

#' @keywords internal
prefix_columns <- function(obj,prefix){
  `colnames<-`(obj, paste0(prefix, colnames(obj)))
}

#' @keywords internal
suffix_columns <- function(obj,suffix){
  `colnames<-`(obj, paste0(colnames(obj), suffix))
}
