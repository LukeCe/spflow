# ---- linear algebra ---------------------------------------------------------
#' @keywords internal
crossproduct_mat_list <- function(mat_l1, mat_l2 = NULL, force_sym = FALSE) {

  n_mat1 <- n_mat2 <- length(mat_l1)
  dim_mat1 <- dim_mat2 <- Reduce("rbind", lapply(mat_l1, dim))
  names1 <- names2 <- names(mat_l1)

  if (!is.null(mat_l2)) {
    n_mat2 <- length(mat_l2)
    dim_mat2 <- Reduce("rbind", lapply(mat_l2, dim))
    names2 <- names(mat_l2)
  }

  # symmetry: only possible when n1 = n2 + imposed when no m2
  force_sym <- force_sym && (n_mat1 == n_mat2)
  force_sym <- force_sym | is.null(mat_l2)

  dims <- rbind(dim_mat1,dim_mat2)
  # check that dims match + symmetry only works for square case...
  stopifnot(has_equal_elements(dims[,1]), has_equal_elements(dims[,2]))

  result <- matrix(0, nrow = n_mat1 , ncol = n_mat2,
                   dimnames = compact(list(names1, names2)))

  # loop over rows
  for (row in seq_len(n_mat1)) {
    cols_start <- ifelse(force_sym, row, 1)
    cols <- seq(cols_start,n_mat2,1)
    result[row,cols] <- ulapply(
      (mat_l2 %||% mat_l1)[cols], "hadamard_sum", mat_l1[[row]])
  }

  if (force_sym)
    result <- make_symmetric(result)

  return(result)
}

#' @keywords internal
make_symmetric <- function(mat){
  tri <- lower.tri(mat)
  mat[tri] <- t(mat)[tri]
  mat
}

#' @keywords internal
decorellate_matrix <- function(y, with_x) {
  y - linear_projection(y,with_x)
}

#' @keywords internal
hadamard_sum <- function(x,y = x) {
  sum( x * y )
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

# ---- combinatorics ----------------------------------------------------------

#' Compute the multinomial coefficient
#'
#' @details
#'   The coefficient is computed for each row in a data.frame where the
#'   rows correspond to the power and the columns for one element
#' @keywords internal
multinom_coef <- function(...) {

  k_args <- flatlist(list(...))
  t <- Reduce("+",k_args)

  # calculate the denominator
  chose_k_factorial <- Reduce("*", lapply(k_args , factorial))
  return(factorial(t)/chose_k_factorial)
}
