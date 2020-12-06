# ---- Variance Moment --------------------------------------------------------

#' @details
#' The moment (H'H) moment matrix is grouped into (4x4) blocks.
#' These 16 blocks are derived as interactions from the four blocks
#' {alpha, alpha_I, beta, gamma}.
#' Only ten blocks are unique and the remaining six are inferred by symmetry.
#'
#' @keywords internal
moment_empirical_var <- function(model_matrices) {

  ## ---- prepare weighting of the model matrices
  wt <- model_matrices$C_

  # prepare weighted neighborhood matrices
  # and derivatives that serve as instruments
  const_intra_wt <- wt %|!|% lapply(model_matrices$const_intra, "*", wt)

  # prepare the moment weighting for the site attributes (D,O,I)
  wt_odi <- derive_weights_ODI(wt,n_d,n_o)
  order_keys <- c("D_","O_","I_")
  X <- model_matrices[order_keys] %>% compact()

  # prepare weighted pair attributes
  G_wt <- wt %|!|% lapply(model_matrices$G_, "*", wt)


  ## ---- compute the 10 moment blocks

  # [alpha] blocks (4/10)
  alpha_blocks <- model_matrices$const %|!|% (list(
    var_block_alpha(wt, model_matrices$N),
    var_block_alpha_alpha_I(const_intra_wt %||% model_matrices$const_intra),
    var_block_alpha_beta(X,wt_odi),
    var_block_alpha_gamma(G_wt %||% model_matrices$G_)
    ) %>% lreduce(cbind))

  # [alpha_I] blocks (7/10)
  alpha_I_blocks <- model_matrices$const_intra %|!|% (list(
    var_block_alpha_I(const_intra_wt %||% model_matrices$const_intra),
    var_block_alpha_I_beta(const_intra_wt %||% model_matrices$const_intra,X),
    var_block_alpha_I_gamma(const_intra_wt %||% model_matrices$const_intra,
                            model_matrices$G)
    ) %>% lreduce(cbind))

  # [beta] blocks (9/10)
  beta_blocks <- X %|!|% (list(
    var_block_beta(X,wt_odi),
    var_block_beta_gamma(X, G_wt %||% model_matrices$G)
    ) %>% lreduce(cbind))

  # [gamma] block (10/10)
  gamma_block <- model_matrices$G %|!|% var_block_gamma(model_matrices$G, G_wt)
  #TODO continue here

  combined_blocks <-
    list(alpha_blocks,alpha_I_blocks,beta_blocks,gamma_block) %>%
    lreduce(rbind_fill0) %>%
    Matrix::forceSymmetric(x = ., uplo = "U") %>%
    as.matrix()

  return(combined_blocks)
}

# ---- Diagonal Blocks ----

var_block_alpha <- function(wt,N) {
  if (is.numeric(wt[1]))
    return(sum(wt))

  return(N)
}

var_block_alpha_I <- function(const_intra) {

  if (is.null(const_intra))
    return(NULL)

  # pre-allocate empty block matrix
  block_size <- length(const_intra)
  block_alpha_I <- matrix(nrow = block_size, ncol = block_size)

  block_alpha_I[1,1] <- nrow(const_intra$In)

  if (block_size == 1)
    return(block_alpha_I)

  # First row and columns are equal to the trace
  rows <- 1
  index <- cols <- 2:block_size
  block_alpha_I[rows,cols] <- block_alpha_I[cols, rows] <-
    rapply(const_intra[index], function(.w) sum(diag(.w)))

  # Remaining rows and columns correspond to the "hadamard sum"
  rows <- cols <- 2:block_size
  block_alpha_I[rows,cols] <- hadamard_sum_matrix(const_intra[index])

  return(block_alpha_I)
}

var_block_beta <- function(X) {

  # The diagonal sub blocks define the dimensions
  # They correspond to the scaled inner product of each matrix
  scaling <- derive_scalings(X)
  if (is.null(scaling))
    return(NULL)

  block_beta <-
    map2(X,scaling, function(.X , .scale) .scale * crossprod(.X)) %>%
    Matrix::bdiag()

  # Of diagonal blocks are filled by index
  indexes <-
    lapply(X, function(.X)  seq_len(ncol(.X))) %>%
    sequentialize_index()

  rows <- indexes$DX
  cols <- indexes$OX
  block_beta[rows,cols] <- tcrossprod(colSums(X$DX),colSums(X$OX))

  if (is.null(X$IX)) {
    block_beta <- Matrix::forceSymmetric(block_beta,"U")
    return(as.matrix(block_beta))
  }

  # The last off-diagonal blocks correspond to the inner products:
  # (OX'IX) and (DX'IX)
  od_keys <- names(X)[1:2]
  rows <- indexes[od_keys] %>% flatten()
  cols <- indexes$IX
  block_beta[rows,cols] <-
    lapply(X[od_keys], crossprod, X$IX) %>%
    lreduce(rbind)


  block_beta <- Matrix::forceSymmetric(block_beta,"U")
  return(as.matrix(block_beta))
}

var_block_gamma <- function(G) {
  G %|!|% hadamard_sum_matrix(G)
}

# ---- Off-diagonal Blocks ----
var_block_alpha_alpha_I <- function(const_intra) {
  const_intra %|!|% (rapply(const_intra,sum) %>% matrix(nrow = 1))
  }

var_block_alpha_beta <- function(X) {

  if (is.null(X))
    return(NULL)

  scaled_col_sums <-
    map2(
      .x = X %>% lapply(col_sums),
      .y = X %>% derive_scalings(),
      .f = `*`) %>%
    lreduce(cbind)

  return(scaled_col_sums)
}

var_block_alpha_gamma <- function(G) {
  G %|!|% (rapply(G,sum) %>% matrix(nrow = 1))
}

var_block_alpha_I_beta <- function(const_intra,X) {

  if (is.null(X) || is.null(const_intra))
    return(NULL)

  result <-
    lapply(const_intra, matrix_prod_O_D_I, X) %>%
    lreduce(rbind)

  return(result)
}

var_block_alpha_I_gamma <- function(const_intra,G) {

  if (is.null(G) || is.null(const_intra))
    return(NULL)

  size_gamma <- length(G)
  size_alpha_I <- length(const_intra)
  block_alpha_I_gamma <- matrix(0, nrow = size_alpha_I, ncol = size_gamma)

  block_alpha_I_gamma[1,] <- rapply(G, function(x) sum(diag(x)))

  if (size_alpha_I == 1)
    return(block_alpha_I_gamma)

  # sum of all element for each combination of two matrices
  had_sum_G <- function(.w) lapply(G, hadamard_sum, .w) %>% unlist()

  block_alpha_I_gamma[-1,] <-
    const_intra[-1] %>%
    lapply(had_sum_G) %>%
    lreduce(rbind)

  return(block_alpha_I_gamma)
}

var_block_beta_gamma <- function(X,G) {

  if (is.null(X) || is.null(G))
    return(NULL)

  result <-
    lapply(G, matrix_prod_O_D_I, X) %>%
    lapply(t) %>%
    lreduce(cbind)

  return(result)
}

# ---- Covariance Moments -------------------------------------------------
moment_empirical_covar <- function(Y, flow_model_matrices) {

  order_keys <- c("DX","OX","IX")
  X <- flow_model_matrices[order_keys] %>% compact()
  result <- c(
    cov_moment_block_alpha(Y),
    cov_moment_block_alpha_I(Y, flow_model_matrices$const_intra),
    cov_moment_block_beta(Y, X),
    cov_moment_block_gamma(Y, flow_model_matrices$G)
  ) %>% lreduce(c)
  return(result)
}

cov_moment_block_alpha <- function(Y) {
  Y %|!|% sum(Y)
}

cov_moment_block_alpha_I <- function(Y,const_intra) {

  if (is.null(const_intra))
    return(NULL)

  block_Y_alpha_I <- sum(diag(Y))

  if (length(const_intra) == 1)
    return(block_Y_alpha_I)

  block_Y_alpha_I <- c(
    block_Y_alpha_I,
    unlist(lapply(const_intra[-1], "hadamard_sum", Y))
  )

  return(block_Y_alpha_I)

}

cov_moment_block_beta <- function(Y, X) {
  matrix_prod_O_D_I(mat = Y,X = X) %>% as.vector()
}

cov_moment_block_gamma <- function(Y, G) {
  G %|!|% unlist(lapply(G,hadamard_sum,Y))
}


# ---- Helpers ------------------------------------------------------------
derive_scalings <- function(X) {

   if (is.null(X))
     return(NULL)

   scaling <- rapply(X[c("OX","DX","IX")], nrow)
   if (is.null(X$IX))
     return(scaling)

   scaling[3] <- 1
   return(scaling)
 }

matrix_prod_O_D_I <- function(mat,X) {

  if (is.null(X) || is.null(mat))
    return(NULL)

  result <- list(
    X$DX %|!|% (rowSums(mat) %*% X$DX),
    X$OX %|!|% (colSums(mat) %*% X$OX),
    X$IX %|!|% (diag(mat) %*% X$IX)
    ) %>%
    lreduce(cbind)

  return(result)
}
