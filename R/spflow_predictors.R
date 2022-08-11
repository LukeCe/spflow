#' @importFrom Matrix Diagonal
#' @keywords internal
compute_signal <- function(
    delta,
    spflow_matrices,
    spflow_indicators = NULL,
    keep_matrix_form = TRUE) {

  obs <- spflow_indicators %|!|% spflow_indicators2obs
  is_cartesian <- is.null(spflow_indicators) || obs[["N_cart"]] == obs[["N_pred"]]
  req_intra <- !all(is.null(spflow_matrices[["I_"]]), is.null(spflow_matrices[["CONST"]][["(Intra)"]]))

  if (is_cartesian) {
    n_o <- unique(c(
      ncol(spflow_matrices[["CONST"]][["(Intra)"]]),
      ncol(spflow_matrices[["P_"]][[1]]),
      ncol(spflow_matrices[["Y_"]][[1]]),
      nrow(spflow_matrices[["OX"]]),
      nrow(spflow_matrices[["IX"]])))
    assert_is_single_x(n_o, "numeric")

    n_d <- unique(c(
      nrow(spflow_matrices[["CONST"]][["(Intra)"]]),
      nrow(spflow_matrices[["P_"]][[1]]),
      nrow(spflow_matrices[["Y_"]][[1]]),
      nrow(spflow_matrices[["DX"]]),
      nrow(spflow_matrices[["IX"]])))
    assert_is_single_x(n_d, "numeric")


    o_index <- rep(seq(n_o), each = n_d)
    d_index <- rep(seq(n_d), times = n_o)
    intra_i <- as.vector(diag(n_o)) %T% (req_intra)
  }

  if (!is_cartesian) {
    filter_sig <- spflow_indicators[["IN_POP"]] %||% TRUE
    spflow_indicators <- spflow_indicators[filter_sig,,drop = FALSE]
    n_o <- obs[["n_orig"]]
    n_d <- obs[["n_dest"]]
    o_index <- as.numeric(spflow_indicators[[2]])
    d_index <- as.numeric(spflow_indicators[[1]])
    intra_i <- as.numeric(spflow_indicators[[1]] == spflow_indicators[[2]]) %T% req_intra
  }

  SIG <- 0
  id_coef <- 0
  if (!is.null(spflow_matrices[["CONST"]][["(Intercept)"]])) {
    id_coef <- id_coef + 1
    SIG <- SIG + delta[id_coef]
  }

  if (!is.null(spflow_matrices[["CONST"]][["(Intra)"]])) {
    id_coef <- id_coef + 1
    SIG <- SIG + delta[id_coef] * intra_i
  }

  if (!is.null(spflow_matrices[["D_"]])) {
    id_coef <- seq_len(ncol(spflow_matrices[["D_"]])) + max(id_coef)
    SIG <- SIG + (spflow_matrices[["D_"]]  %*%  delta[id_coef])[d_index]
  }

  if (!is.null(spflow_matrices[["O_"]])) {
    id_coef <- seq_len(ncol(spflow_matrices[["O_"]])) + max(id_coef)
    SIG <- SIG + (spflow_matrices[["O_"]]  %*%  delta[id_coef])[o_index]
  }

  if (!is.null(spflow_matrices[["I_"]])) {
    id_coef <- seq_len(ncol(spflow_matrices[["I_"]])) + max(id_coef)
    SIG_I <- spflow_matrices[["I_"]]  %*%  delta[id_coef]
    SIG <- SIG + (SIG_I[o_index] * intra_i)
  }

  if (!is.null(spflow_matrices[["P_"]])) {
    id_coef <- seq_along(spflow_matrices[["P_"]]) + max(id_coef)

    SIG_G <- Reduce("+", Map("*", spflow_matrices[["P_"]], delta[id_coef]))
    if (keep_matrix_form & is_cartesian)
      SIG <- matrix(SIG, nrow = n_d, ncol = n_o)
    if (keep_matrix_form & !is_cartesian)
      SIG <- spflow_indicators2mat(spflow_indicators, do_filter = "IN_POP", do_values = SIG)
    if (!keep_matrix_form & !is_cartesian)
      SIG_G <- SIG_G[spflow_indicators2pairindex(spflow_indicators, do_filter = "IN_POP")]
    if (!keep_matrix_form & is_cartesian)
      SIG_G <- as.vector(SIG_G)
    SIG <- SIG + SIG_G
  }
  return(SIG)
}


#' @keywords internal
compute_expectation <- function(
  signal_matrix,
  DW,
  OW,
  rho,
  model,
  M_indicator = NULL,
  approximate = TRUE,
  max_it = 10,
  keep_matrix_form = TRUE) {


  if (approximate) {

    update_signal <- function(signal_matrix) {
      decomposed_signal <- lag_flow_matrix(
        Y = signal_matrix,
        M_indicator = M_indicator,
        model = model,
        OW = OW,
        DW = DW,
        name = "SIG")

      signal <- Reduce("+", Map("*",decomposed_signal[-1], -rho))
      return(signal)
    }

    Yhat <- new_sig <- signal_matrix
    for (i in seq(max_it)) {
      new_sig <- new_sig + update_signal(new_sig)
      Yhat <- Yhat + new_sig
    }
  }

  if (!approximate) {
    WF_parts <- expand_spflow_neighborhood(DW = DW, OW = OW, model = model)
    A <- spatial_filter(weight_matrices = WF_parts, autoreg_parameters = rho)
    Yhat <- solve(A, as.vector(signal_matrix))
    Yhat <- matrix(Yhat, nrow = nrow(signal_matrix), ncol = ncol(signal_matrix))
  }

  if (keep_matrix_form)
    return(Yhat)

  if (is.null(M_indicator))
    return(as.vector(Yhat))

  return(Yhat[as.logical(M_indicator)])
}

#' @keywords internal
compute_diag_precision_mat <- function(
    DW,
    OW,
    rho,
    model,
    n_o,
    n_d,
    M_indicator = NULL) {


  req_o <- "rho_o" %in% names(rho)
  req_d <- "rho_d" %in% names(rho)
  req_w <- "rho_w" %in% names(rho)

  rho_o <- rho["rho_o"]
  rho_d <- rho["rho_d"]
  rho_w <- rho["rho_w"]

  DW2 <- DW * DW
  OW2 <- OW * OW

  # cartesian
  if (is.null(M_indicator)) {
    SDW <- colSums(DW)
    SDW2 <- colSums(DW2)
    SOW <- colSums(OW)
    SOW2 <- colSums(OW2)

    result <- 1
    if (req_d) result <- result + (rho_d*rho_d * tcrossprod(SDW2, rep(1,n_o)))
    if (req_o) result <- result + (rho_o*rho_o * tcrossprod(rep(1,n_d), SOW2))
    if (req_w) result <- result + (rho_w*rho_w * tcrossprod(SDW2, SOW2))
  return(result)
  }

  # non-cartesian
  result <- M_indicator
  if (req_d) result <- result + (rho_d*rho_d * crossprod(DW2, M_indicator))
  if (req_o) result <- result + (rho_o*rho_o * M_indicator %*% OW2)
  if (req_w) result <- result + (rho_w*rho_w * crossprod(DW2, M_indicator) %*% OW2)
  return(result * M_indicator)
}

