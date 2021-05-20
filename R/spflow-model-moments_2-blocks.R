# ---- Variance Moment --------------------------------------------------------

#' @title
#' Compute the moment expression in an interaction model in matrix form
#'
#' @details
#' The moment (U'U) moment matrix is grouped into (4x4) blocks.
#' These 16 blocks are derived as interactions from the four blocks
#' {alpha, alpha_I, beta, gamma}.
#' Only ten blocks are unique and the remaining six are inferred by symmetry.
#' The developments follow the theory presented in
#' \insertCite{Dargel2021;textual}{spflow}.
#'
#' @references \insertAllCited{}
#' @importFrom Matrix forceSymmetric rowSums colSums diag
#' @keywords internal
moment_empirical_var <- function(model_matrices,N,n_d,n_o) {

  ## ---- prepare weighting of the model matrices
  wt <- model_matrices$wt
  weight_as_indicator <- is.logical(wt[1])
  if (weight_as_indicator) wt <- wt*1

  # prepare weighted neighborhood matrices
  # and derivatives that serve as instruments
  const_global <- model_matrices$constants$global
  const_intra <- model_matrices$constants$intra
  const_intra_wt <- wt %|!|% lapply(const_intra, "*", wt)

  # prepare the moment weighting for the site attributes (D,O,I)
  order_keys <- c("D_","O_","I_")
  X <- compact(model_matrices[order_keys])
  wt_odi <- derive_weights_ODI(wt,n_d,n_o)[names(X)]

  # prepare weighted pair attributes
  G_wt <- wt %|!|% lapply(model_matrices$G_, "*", wt)


  ## ---- compute the 10 moment blocks

  # [alpha] blocks (4/10)
  alpha_blocks <- const_global %|!|% Reduce("cbind",list(
    var_block_alpha(wt, N),
    var_block_alpha_alpha_I(const_intra_wt %||% const_intra),
    var_block_alpha_beta(X,wt_odi),
    var_block_alpha_gamma(G_wt %||% model_matrices$G_)
    ))
  if (!is.null(alpha_blocks))
    rownames(alpha_blocks) <- "(Intercept)"

  # [alpha_I] blocks (7/10)
  alpha_I_blocks <- const_intra %|!|% Reduce("cbind",list(
    var_block_alpha_I(const_intra,const_intra_wt),
    var_block_alpha_I_beta(const_intra_wt %||% const_intra,X),
    var_block_alpha_I_gamma(const_intra_wt %||% const_intra,
                            model_matrices$G)
    ))
  if (!is.null(alpha_I_blocks))
    rownames(alpha_I_blocks) <- names(const_intra)


  # [beta] blocks (9/10)
  beta_blocks <- X %|!|% Reduce("cbind",list(
    var_block_beta(X,wt_odi,wt),
    var_block_beta_gamma(X, G_wt %||% model_matrices$G)
    ))
  if (!is.null(beta_blocks))
    rownames(beta_blocks) <- ulapply(X,"colnames")


  # [gamma] block (10/10)
  gamma_block <- model_matrices$G %|!|% var_block_gamma(model_matrices$G, G_wt)
  if (!is.null(gamma_block))
    rownames(gamma_block) <- names(model_matrices$G)

  combined_blocks <- list(alpha_blocks,alpha_I_blocks,beta_blocks,gamma_block)
  combined_blocks <- rbind_fill_left(compact(combined_blocks))
  combined_blocks <- make_symmetric(combined_blocks)
  colnames(combined_blocks) <- rownames(combined_blocks)

  return(combined_blocks)
}

# ---- Diagonal Blocks ----

#' @keywords internal
var_block_alpha <- function(wt,N) {
  if (is.null(wt)) N else sum(wt)
}

#' @keywords internal
var_block_alpha_I <- function(const_intra, const_intra_wt) {
  const_intra %|!|% crossproduct_mat_list(const_intra, const_intra_wt, TRUE)
}

#' @keywords internal
var_block_beta <- function(X,wt_odi,wt) {

  od_names <- c("D","O") %p% "_"
  scalar_weights <- has_equal_elements(c(rapply(wt_odi, length),1))
  if (scalar_weights) {
    cross_prods <- Map("*", lapply(X, "crossprod"), wt_odi)
    outer_prods <- tcrossprod(colSums(X$D_),colSums(X$O_))
    intra_prods <- X$I_ %|!|% lapply(X[od_names], "crossprod", X$I_)
  }

  if (!scalar_weights) {
    cross_prods <- Map("*", X, lapply(wt_odi,"sqrt"))
    cross_prods <- lapply(cross_prods, "crossprod")
    outer_prods <- crossprod(X$D_, wt) %*% X$O_
    wt_XI <- X$I_ %|!|% wt_odi$I_ * X$I_
    intra_prods <- wt_XI %|!|% lapply(X[od_names], "crossprod",wt_XI)
  }

  # fill the block from top to bottom
  beta_block <- compact(list(
    "D_" = cbind(cross_prods$D_,outer_prods,intra_prods$D_),
    "O_" = cbind(cross_prods$O_,intra_prods$O_),
    "I_" = cbind(cross_prods$I_)))
  beta_block <- rbind_fill_left(beta_block)
  beta_block <- as.matrix(forceSymmetric(beta_block, "U"))
  return(beta_block)
}

#' @keywords internal
var_block_gamma <- function(G,G_wt) {
  G %|!|% crossproduct_mat_list(G, G_wt, TRUE)
}

# ---- Off-diagonal Blocks ----------------------------------------------------
#' @keywords internal
var_block_alpha_alpha_I <- function(const_intra) {
  const_intra %|!|% matrix(rapply(const_intra,sum), nrow = 1)
  }

#' @keywords internal
var_block_alpha_beta <- function(X, wt_odi) {

  if (is.null(X))
    return(NULL)

  scalar_weights <- has_equal_elements(c(rapply(wt_odi, length),1))
  if (scalar_weights) {
    scaled_col_sums <- Map("*", wt_odi, lapply(X, "colSums2mat"))
    scaled_col_sums <- Reduce("cbind", scaled_col_sums)
  }

  if (!scalar_weights) {
    scaled_col_sums <- Map("%*%", wt_odi, X)
    scaled_col_sums <- Reduce("cbind", scaled_col_sums)
  }
  return(scaled_col_sums)
}


#' @keywords internal
var_block_alpha_gamma <- function(G) {
  G %|!|% matrix(rapply(G,sum), nrow = 1)
}

#' @keywords internal
var_block_alpha_I_beta <- function(const_intra,X) {

  if (is.null(X) || is.null(const_intra))
    return(NULL)

  result <- Reduce("rbind", lapply(const_intra, "matrix_prod_ODI", X))
  return(result)
}

#' @keywords internal
var_block_alpha_I_gamma <- function(const_intra,G) {

  if (is.null(G) || is.null(const_intra))
    return(NULL)

  block_alpha_I_gamma <- crossproduct_mat_list(const_intra,G)

  return(block_alpha_I_gamma)
}

#' @keywords internal
var_block_beta_gamma <- function(X,G) {

  if (is.null(X) || is.null(G))
    return(NULL)

  result <- lapply(G, "matrix_prod_ODI", X)
  result <- Reduce("cbind", lapply(result, "t"))
  return(result)
}

# ---- Covariance Moments -----------------------------------------------------

#' @keywords internal
moment_empirical_covar <- function(Y, model_matrices) {

  order_keys <- c("D","O","I") %p% "_"
  X <- compact(model_matrices[order_keys])
  result <- Reduce("c", c(
    cov_moment_alpha(Y) %T% (1 == model_matrices$constants$global),
    cov_moment_alpha_I(Y, model_matrices$constants$intra),
    cov_moment_beta(Y, X),
    cov_moment_gamma(Y, model_matrices$G)
  ))
  return(result)
}

#' @keywords internal
cov_moment_alpha <- function(Y) {
  Y %|!|% sum(Y)
}

#' @keywords internal
cov_moment_alpha_I <- function(Y,const_intra) {

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

#' @keywords internal
cov_moment_beta <- function(Y, X) {
  as.vector(matrix_prod_ODI(mat = Y,X = X))
}

#' @keywords internal
cov_moment_gamma <- function(Y, G) {
  G %|!|% unlist(lapply(G,hadamard_sum,Y))
}


# ---- Helpers ----------------------------------------------------------------

#' @keywords internal
derive_weights_ODI <- function(wt,n_o,n_d) {

   if (is.null(wt)) # scalar weights
     return(list("D_" = n_o, "O_" = n_d, "I_" = 1))

  return(list("D_" = rowSums(wt), "O_" = colSums(wt), "I_" = diag(wt)))
 }

#' @keywords internal
matrix_prod_ODI <- function(mat,X) {

  if (is.null(X) || is.null(mat))
    return(NULL)

  result <- list(
    X$D_ %|!|% (rowSums(mat) %*% X$D_),
    X$O_ %|!|% (colSums(mat) %*% X$O_),
    X$I_ %|!|% (diag(mat) %*% X$I_))
  result <- Reduce("cbind", result)
  return(result)
}
