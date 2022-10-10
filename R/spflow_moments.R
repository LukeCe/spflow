#' @title Compute the moment matrices used during the estimation with [spflow()]
#' @description These are internal functions called within the estimation procedure
#' @details
#'
#' The implementation details are derived in
#' \insertCite{Dargel2021;textual}{spflow} and
#' \insertCite{Dargel2022;textual}{spflow}.
#'
#' ## The variance moment:
#'
#' The moment (U'U) moment matrix is grouped into 4 x 4 blocks.
#' These 16 blocks are derived as interactions from the four sets of variables
#' U_alpha, U_alpha_I, U_beta, U_gamma.
#' Only ten blocks are unique and the remaining six are inferred by symmetry.
#'
#' ## The covariance moment:
#'
#' The moment (U'y) is grouped into four blocks using the same arguments.
#' Here we can use the same function to compute the inner product of y and its
#' spatial lags with U.
#'
#' @return A list containing all moments required for the estimation
#' @author Lukas Dargel
#' @importFrom Matrix nnzero
#' @keywords internal
#' @references \insertAllCited{}
derive_spflow_moments <- function(
    spflow_matrices,
    n_o,
    n_d,
    N,
    wt,
    na_rm = FALSE) {


  ## ---- derive moments from the covariates (Z,H)
  mat_keys <- intersect(c("D_", "O_", "I_", "P_","Y_"), names(spflow_matrices))
  for (i in seq_along(mat_keys)) {
    mk <- mat_keys[[i]]
    if (mk %in% c("P_","Y_"))
      names(spflow_matrices[[mk]]) <- paste0(mk, names(spflow_matrices[[mk]]))
    if (mk %in% c("D_", "O_", "I_"))
      colnames(spflow_matrices[[mk]]) <- paste0(mk, colnames(spflow_matrices[[mk]]))
  }

  UU <- spflow_moment_var(spflow_matrices, wt, N, n_d, n_o)
  assert(is.null(wt) || nnzero(wt) > ncol(UU), "
         Estimation aborted!
         There are too few complete observations to identifiy the parameters.")

  # subset ZZ
  variable_order <- c("CONST", "D_", "O_", "I_", "P_")
  is_instrument <- rapply(spflow_matrices[variable_order],f = attr_inst_status)
  Z_index <- !as.logical(is_instrument)
  ZZ <- UU[Z_index, Z_index ,drop = FALSE]


  ## ---- derive moments from the response (UY, ZY, TSS)
  # ...weighted Y if required
  Y_wt <- wt %|!|% lapply(spflow_matrices$Y_,"*",wt)
  UY <- Y_wt %||% spflow_matrices$Y_
  UY <- lapply(UY, "spflow_moment_cov", spflow_matrices)
  UY <- Reduce("cbind", x = UY, init = matrix(nrow = nrow(UU),ncol = 0))
  dimnames(UY) <- list(rownames(UU), names(spflow_matrices$Y_))

  ZY <- UY[Z_index, , drop = FALSE]
  TSS <- crossproduct_mat_list(spflow_matrices$Y_, Y_wt)

  # covariance matrix
  TCORR <- rbind(cbind(UU,UY), cbind(t(UY), TSS))
  TCORR <- TCORR - (outer(TCORR[1,], TCORR[1,])/N)
  TCORR <- TCORR / outer(sqrt(diag(TCORR)), sqrt(diag(TCORR)))
  diag(TCORR[-1,-1]) <- 1

  model_moments <- compact(list(
    "n_d"   = n_d,
    "n_o"   = n_o,
    "N"     = N,
    "UU"    = UU,
    "ZZ"    = ZZ,
    "UY"    = UY,
    "ZY"    = ZY,
    "TSS"   = TSS,
    "TCORR" = TCORR))

  error_msg <- "
  The estimation is aborted because the %s variables contain
  NA|Inf|NaN values!
  <br>Check that all variables are well defined and that all
  tranformations are valid (e.g. avoid logarithms of 0)."
  assert(all(is.finite(UU)),error_msg, "explanatory")
  assert(all(is.finite(UY)), error_msg, "response")
  return(model_moments)
}

# ---- Variance Moment --------------------------------------------------------
#' @rdname derive_spflow_moments
#' @importFrom Matrix forceSymmetric rowSums colSums diag
#' @keywords internal
spflow_moment_var <- function(spflow_matrices, wt,N,n_d,n_o) {

  # prepare weighted neighborhood matrices
  const_global <- spflow_matrices[["CONST"]][["(Intercept)"]]
  const_intra <- spflow_matrices[["CONST"]][-1]
  const_intra_wt <- wt %|!|% lapply(const_intra, "*", wt)

  # prepare the moment weighting for the site attributes (D,O,I)
  X <- compact(spflow_matrices[c("D_","O_","I_")])
  wt_odi <- derive_weights_DOI(wt,n_o = n_o,n_d = n_d)[names(X)]

  # prepare weighted pair attributes
  P_wt <- wt %|!|% lapply(spflow_matrices[["P_"]], "*", wt)


  ## ---- compute the 10 moment blocks
  namerows <- function(rnames) matrix(numeric(),length(rnames), 0,dimnames =  list(rnames, NULL))

  # [alpha] blocks (4/10)
  alpha_blocks <- const_global %|!|% Reduce("cbind",list(
    namerows("(Intercept)"),
    var_block_alpha(wt, N),
    var_block_alpha_alpha_I(const_intra_wt %||% const_intra),
    var_block_alpha_beta(X,wt_odi),
    var_block_alpha_gamma(P_wt %||% spflow_matrices[["P_"]])))


  # [alpha_I] blocks (7/10)
  alpha_I_blocks <- const_intra %|!|% Reduce("cbind",list(
    namerows(names(const_intra)),
    var_block_alpha_I(const_intra,const_intra_wt),
    var_block_alpha_I_beta(const_intra_wt %||% const_intra,X),
    var_block_alpha_I_gamma(const_intra_wt %||% const_intra,
                            spflow_matrices[["P_"]])))

  # [beta] blocks (9/10)
  beta_blocks <- X %|!|% Reduce("cbind",list(
    namerows(ulapply(X,"colnames")),
    var_block_beta(X,wt_odi,wt),
    var_block_beta_gamma(X, P_wt %||% spflow_matrices[["P_"]])))

  # [gamma] block (10/10)
  gamma_block <- spflow_matrices[["P_"]] %|!|% cbind(
    namerows(names(spflow_matrices[["P_"]])),
    var_block_gamma(spflow_matrices[["P_"]], P_wt))

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

  od_names <- c("D_","O_")
  has_od <- !is.null(X[["D_"]]) & !is.null(X[["O_"]])
  scalar_weights <- has_equal_elements(c(rapply(wt_odi, length),1))


  if (scalar_weights) {
    cross_prods <- Map("*", lapply(X, "crossprod"), wt_odi)
    outer_prods <- tcrossprod(colSums(X[["D_"]]),colSums(X[["O_"]])) %T% has_od
    intra_prods <- X[["I_"]] %|!|% lapply(X[od_names], "crossprod", X[["I_"]])  %T% has_od
  }

  if (!scalar_weights) {
    cross_prods <- Map("*", X, lapply(wt_odi,"sqrt"))
    cross_prods <- lapply(cross_prods, "crossprod")
    outer_prods <- crossprod(X[["D_"]], wt) %*% X[["O_"]] %T% has_od
    wt_XI <- X[["I_"]] %|!|% wt_odi$I_ * X[["I_"]]
    intra_prods <- wt_XI %|!|% lapply(X[od_names], "crossprod",wt_XI)  %T% has_od
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
var_block_gamma <- function(P, P_wt) {
  P %|!|% crossproduct_mat_list(P, P_wt, TRUE)
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
var_block_alpha_gamma <- function(P) {
  P %|!|% matrix(rapply(P, sum), nrow = 1)
}

#' @keywords internal
var_block_alpha_I_beta <- function(const_intra,X) {

  if (is.null(X) || is.null(const_intra))
    return(NULL)

  result <- Reduce("rbind", lapply(const_intra, "matrix_prod_DOI", X))
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
var_block_beta_gamma <- function(X, P) {

  if (is.null(X) || is.null(P))
    return(NULL)

  result <- lapply(P, "matrix_prod_DOI", X)
  result <- Reduce("cbind", lapply(result, "t"))
  return(result)
}

# ---- Covariance Moments -----------------------------------------------------
#' @rdname derive_spflow_moments
#' @keywords internal
spflow_moment_cov <- function(Y, spflow_matrices) {

  order_keys <- c("D_","O_","I_")
  X <- compact(spflow_matrices[order_keys])
  if (!is.null(spflow_matrices$weights))
    Y <- Y * spflow_matrices$weights

  result <- Reduce("c", c(
    cov_moment_alpha(Y),
    cov_moment_alpha_I(Y, spflow_matrices[["CONST"]][-1] %||% NULL),
    cov_moment_beta(Y, X),
    cov_moment_gamma(Y, spflow_matrices[["P_"]])
  ))
  return(result)
}

#' @keywords internal
cov_moment_alpha <- function(Y) {
  Y %|!|% sum(Y)
}

#' @keywords internal
cov_moment_alpha_I <- function(Y, const_intra) {

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
  as.vector(matrix_prod_DOI(mat = Y,X = X))
}

#' @keywords internal
cov_moment_gamma <- function(Y, P) {
  P %|!|% unlist(lapply(P, hadamard_sum, Y))
}


# ---- Helpers ----------------------------------------------------------------

#' @keywords internal
derive_weights_DOI <- function(wt,n_o,n_d) {

  if (is.null(wt)) # scalar weights
    return(list("D_" = n_o, "O_" = n_d, "I_" = 1))

  return(list("D_" = rowSums(wt), "O_" = colSums(wt), "I_" = diag(wt)))
}

#' @keywords internal
matrix_prod_DOI <- function(mat,X) {

  if (is.null(X) | is.null(mat))
    return(NULL)

  result <- list(
    X[["D_"]] %|!|% (rowSums(mat) %*% X[["D_"]]),
    X[["O_"]] %|!|% (colSums(mat) %*% X[["O_"]]),
    X[["I_"]] %|!|% (diag(mat) %*% X[["I_"]]))
  result <- Reduce("cbind", result)
  return(result)
}
