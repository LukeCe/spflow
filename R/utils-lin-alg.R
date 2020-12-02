#' @keywords internal
decorellate_matrix <- function(y, with_x) {
  y - linear_projection(y,with_x)
}

#' @keywords internal
impose_orthogonality <- function(mat,column_sets){

  # first block does not require orthogonal projection
  Mx_mat <- mat[,column_sets[[1]]]
  for (i in seq_along(column_sets)[-1]) {

    # Bind residual of orthogonal projection
    Px_mat <- linear_projection(mat[,column_sets[[i]]],Mx_mat)
    Mx_mat <- cbind(Mx_mat, mat[,column_sets[[i]]] - Px_mat)
  }
  return(Mx_mat)
}

#' @keywords internal
linear_dim_reduction <- function(mat, var_threshold = 0, n_comp = NULL) {

  svd_mat <- La.svd(mat)
  n_comp <- n_comp %||% sum(svd_mat$d >= var_threshold)

  S_trunc <- diag(svd_mat$d[seq_len(n_comp)])
  U_trunc <- svd_mat$u[,seq_len(n_comp)]

  return(U_trunc %*% S_trunc)
}

#' @keywords internal
linear_projection <- function(y, on_x){
  beta <- solve(crossprod(on_x),crossprod(on_x,y))
  Px_y <- on_x %*% beta
  return(Px_y)
}

#' @keywords internal
sandwich_prod <- function(w1,mat,w2=w1){
  w_mat <- w1 %|!|% as.matrix(w1 %*% mat) %||% mat
  w_mat_w <- w2 %|!|% as.matrix(tcrossprod(w_mat,w2)) %||% w_mat
  return(w_mat_w)
}
