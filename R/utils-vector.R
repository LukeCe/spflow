#' @keywords internal
center <- function(x){
  if (is.null(dim(x)))
    return(x - (sum(x)/length(x)))

  return(x - colMeans(x))
}

#' @keywords internal
#' @importFrom Matrix sparseMatrix
vec_to_matrix <- function(vec, complet, n_rows, n_cols, i_rows, j_cols) {
  if (complet == 1 ){
    mat <- matrix(vec, nrow = n_rows, ncol = n_cols)
  }
  if (complet >= .5) {
    mat <- matrix(0,nrow = n_rows, ncol = n_cols)
    mat[cbind(i_rows, j_cols)] <- vec
  }

  if (complet < 0.5 ){
    mat <- sparseMatrix(i= i_rows, j=j_cols, x= vec, dims = c(n_rows,n_cols))
  }
  return(mat)
}

#' @keywords internal
factor_in_order <- function(x) {
  factor(x,levels = as.character(unique(x)))
}
