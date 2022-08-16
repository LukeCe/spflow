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
mprod_trace <- function(x, y = x) {
  sum(x * t(y))
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

#' @title Graphic display for correlation matrices
#' @importFrom grDevices hcl.colors
#' @importFrom graphics axis image text
#' @param cor_mat A matrix of pairwise correlations
#' @return A graphical representation of the correlation matrix
#' @export
#' @examples
#'
#' cor_mat <- cor(matrix(rnorm(40),ncol = 4))
#' corr_map(cor_mat)
corr_map <- function(cor_mat) {

  cor_mat[!is.finite(cor_mat)] <- NA
  assert(all(c(diag(cor_mat) == 1, cor_mat <= 1),na.rm = TRUE),
         "Make sure to provide a valid correlation matrix.
         All elements musst be in the interval from -1 to 1
         and the diagonal elements musst be 1!")

  if (is.null(rownames(cor_mat)))
    rownames(cor_mat) <- colnames(cor_mat)

  if (is.null(colnames(cor_mat)))
    colnames(cor_mat) <- rownames(cor_mat)


  nc_c <- max(c(nchar(colnames(cor_mat))^(.85),1.3))
  nc_r <- max(c(nchar(rownames(cor_mat))^(.85),1.3))
  opts <- par(mar = c(1, nc_r / 1.3, nc_c / 1.3, 1))
  cor_mat_rev <- cor_mat[,rev(seq(ncol(cor_mat)))]

  # color
  ncolor <- 200
  col_breaks <- seq(from = -1, to = 1, length.out = ncolor + 1)
  col_breaks <- sign(col_breaks) * sqrt(abs(col_breaks))
  col_pal <- hcl.colors(n = ncolor, palette = "Blue-Red 3",rev = T)

  # text and labels
  pos_xy <- seq(from = 0, to = 1, length.out = ncol(cor_mat))
  pos_x <- rep(pos_xy, ncol(cor_mat))
  pos_y <- rep(pos_xy, each = ncol(cor_mat))
  text_cor <- as.vector(cor_mat_rev)
  text_cor <- round(text_cor,2)
  text_col <- rep("white", length(text_cor))
  text_col[abs(text_cor) < .5] <- "black"
  top_labels <- rev(colnames(cor_mat_rev))
  left_labels <- rev(rownames(cor_mat_rev))
  backup_labels <- paste0("V", seq(ncol(cor_mat_rev)))

  dont_show <- sort(abs(col_breaks))[2]
  text_filter <- abs(text_cor) > dont_show


  image.default(
    z = cor_mat_rev,
    useRaster = FALSE,
    axes = FALSE,
    col = col_pal,
    breaks = col_breaks)
  axis(side = 2, # left
       at = seq(from = 0, to = 1, length.out = ncol(cor_mat)),
       labels = left_labels %||% backup_labels,
       las = 2)
  axis(side = 3, # top
       at = seq(from = 0, to = 1, length.out = ncol(cor_mat)),
       labels = top_labels %||% backup_labels,
       las = 2)
  text(x = pos_x[text_filter],
       y = pos_y[text_filter],
       labels = text_cor[text_filter],
       col = text_col[text_filter])
  par(opts)
}

# ---- combinatorics ----------------------------------------------------------
#' Create a table for  multinomial coefficient and parameter powers
#' @keywords internal
#' @importFrom utils combn
multinom_table <- function(max_power, coef_names) {

  nb_coefs <- length(coef_names)
  possible_powers <- seq(0, max_power)
  coef_powers <- combn(rep(possible_powers,nb_coefs),nb_coefs,simplify = FALSE)
  coef_powers <- as.data.frame(do.call("rbind",coef_powers),row.names = NULL)
  names(coef_powers) <- coef_names

  coef_powers[["POWER_ORDER"]] <- rowSums(coef_powers)
  coef_powers <- coef_powers[coef_powers$POWER_ORDER <= max_power & coef_powers$POWER_ORDER > 0,, drop = FALSE]
  coef_powers <- unique(coef_powers)
  coef_powers[["COEF_MULTINOM"]] <- multinom_coef(coef_powers[coef_names])
  row.names(coef_powers) <- NULL
  return(coef_powers)
}

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
