#' @keywords internal
rbind_fill_left <- function(..., fill = NA){

  mat_list <- flatlist(list(...))
  mat_cols <- unlist(lapply(mat_list, "ncol"))

  assert(all(diff(mat_cols) <= 0),
         "The number of columns of the matrices must be weakly decreasing!")

  mat_col_missing <- mat_cols[1] - mat_cols
  cbind_fill <- function(mat,nb_cols){
    cbind(matrix(fill,ncol = nb_cols, nrow = nrow(mat)),mat)
  }

  filled_matrix <- Map("cbind_fill", mat_list, mat_col_missing)
  filled_matrix <- Reduce("rbind", filled_matrix)
  return(filled_matrix)
}

#' @keywords internal
trace_sequence <- function(W, max_power = 10 ) {

  W_traces <- vector(mode = "list", length = max_power)
  W_pow <- W
  W_traces[[1]] <- sum(diag(W_pow))

  for (pow in seq_len(max_power - 1)) {
    W_pow <- W %*% W_pow
    W_traces[[pow + 1]] <-  sum(diag(W_pow))
  }

  return(unlist(W_traces))
}

#' @keywords internal
#' @importFrom Matrix bdiag
block_diag <- function(...){
  as.matrix(bdiag(...))
}

#' @keywords internal
sort_columns <- function(mat) {
  mat[,sort(colnames(mat))]
}

#' @keywords  internal
stack_columns <- function(mat ,rows = "row", cols = "col", value = "value") {
  vec_form <- cbind(
    expand.grid(col = factor_in_order(colnames(mat)),
                row = factor_in_order(rownames(mat))),
    value = as.vector(mat))
  names(vec_form) <- c(cols,rows,value)
  vec_form
}

#' @keywords internal
colSums2mat <- function(x) {
  matrix(colSums(x),nrow = 1)
}

