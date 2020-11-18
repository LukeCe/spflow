#' @keywords internal
center <- function(x){
  if (is.null(dim(x)))
    return(x - (sum(x)/length(x)))

  return(x - colMeans(x))
}

#' @keywords internal
#' @importFrom data.table between
#' @importFrom Matrix sparseMatrix
vec_to_matrix <- function(vec,completeness,n_rows,n_cols,i_rows,j_cols) {
  if (completeness == 1 ){
    mat <- matrix(vec, nrow = n_rows, ncol = n_cols)
  }
  if (completeness %>% between(0.5, 1, incbounds = FALSE)) {
    mat <- matrix(0,nrow = n_rows, ncol = n_cols)
    mat[cbind(i_rows, j_cols)] <- vec
  }

  if (completeness <= 0.5 ){
    mat <- sparseMatrix(i= i_rows, j=j_cols, x= vec, dims = c(n_rows,n_cols))
  }
  return(mat)
}
