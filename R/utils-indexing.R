#' @keywords internal
sequentialize_index <- function(index_list) {
  len <- unlist(lapply(index_list, length))
  shift <- cumsum(c(0,len))[1:length(index_list)]
  mapply("+", index_list, as.list(shift),SIMPLIFY = FALSE)
}
