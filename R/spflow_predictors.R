#' @importFrom Matrix Diagonal
#' @keywords internal
compute_signal <- function(
    delta,
    spflow_matrices,
    spflow_indicators = NULL,
    keep_matrix_form = TRUE) {

  obs <- spflow_indicators %|!|% spflow_indicators2obs
  is_cartesian <- is.null(spflow_indicators) || obs[["N_cart"]] == obs[["N_pop"]]
  req_intra <- !all(is.null(spflow_matrices[["I_"]]), is.null(spflow_matrices[["CONST"]][["(Intra)"]]))

  if (!is_cartesian) {
    filter_sig <- spflow_indicators[["IN_POP"]] %||% TRUE
    spflow_indicators <- spflow_indicators[filter_sig,,drop = FALSE]
    n_o <- obs[["n_orig"]]
    n_d <- obs[["n_dest"]]
    o_index <- as.numeric(spflow_indicators[[2]])
    d_index <- as.numeric(spflow_indicators[[1]])
    intra_i <- as.numeric(spflow_indicators[[1]] == spflow_indicators[[2]]) %T% req_intra
  }

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

  signal <- 0
  id_coef <- 0
  delta <- delta[!is.na(delta)]
  coef_mlt <- function(X_part = "D_") {
    X_delta <- delta[grep(paste0("^", X_part),names(delta))]
    X_mat <- spflow_matrices[[X_part]]

    if (is.list(X_mat)) {
      names(X_mat) <- paste0(X_part, names(X_mat))
      return(Reduce("+", Map("*", X_mat[names(X_delta)], X_delta)))
    }

    colnames(X_mat) <- paste0(X_part, colnames(X_mat))
    return(X_mat[,names(X_delta),drop = FALSE] %*% X_delta)
  }
  if ("(Intercept)" %in% names(delta))
    signal <- signal + delta["(Intercept)"]

  if ("(Intra)" %in% names(delta))
    signal <- signal + delta["(Intra)"] * intra_i

  if (any(grepl("^D_", names(delta))))
    signal <- signal + coef_mlt("D_")[d_index]

  if (any(grepl("^O_", names(delta))))
    signal <- signal +  coef_mlt("O_")[o_index]

  if (any(grepl("^I_", names(delta))))
    signal <- signal + coef_mlt("I_")[o_index] * intra_i

  if (keep_matrix_form & !is_cartesian)
    signal <- spflow_indicators2mat(spflow_indicators, do_filter = "IN_POP", do_values = signal)

  if (keep_matrix_form & is_cartesian)
    signal <- matrix(signal, nrow = n_d, ncol = n_o)

  if (any(grepl("^P_", names(delta)))) {
    signal_P <- coef_mlt("P_")

    if (!keep_matrix_form & is_cartesian)
      signal_P <- as.vector(signal_P)
    if (!keep_matrix_form & !is_cartesian)
      signal_P <- signal_P[spflow_indicators2pairindex(spflow_indicators, do_filter = "HAS_ZZ")]
    signal <- signal + signal_P
  }

  return(signal)
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

      signal <- Reduce("+", Map("*",decomposed_signal[-1], rho))
      return(signal)
    }

    Y_TC <- signal_t <- signal_matrix
    for (i in seq(max_it)) {
      signal_t <- update_signal(signal_t)
      Y_TC <- Y_TC + signal_t
    }
  }

  if (!approximate) {
    WF_parts <- expand_spflow_neighborhood(DW = DW, OW = OW, model = model, M_indicator = M_indicator)
    A <- spatial_filter(weight_matrices = WF_parts, autoreg_parameters = rho)

    if (is.null(M_indicator))
      Y_TC <- matrix(solve(A, as.vector(signal_matrix)), nrow = nrow(signal_matrix), ncol = ncol(signal_matrix))

    if (!is.null(M_indicator)) {
      Y_TC <- M_indicator * 1
      suppressMessages(Y_TC[as.logical(M_indicator)] <- as.numeric(solve(A, signal_matrix[as.logical(M_indicator)])))
    }
  }

  if (keep_matrix_form)
    return(Y_TC)

  if (is.null(M_indicator))
    return(as.vector(Y_TC))

  return(Y_TC[as.logical(M_indicator)])
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

