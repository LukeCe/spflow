#' @importFrom Matrix Diagonal
#' @keywords internal
compute_signal <- function(model_matrices, delta) {

  number_of_coefs <- compact(list(
    "const" = model_matrices$const %|!|% length,
    "const_intra" = model_matrices$const_intra %|!|% length,
    "D_" = model_matrices$D_ %|!|% curry(seq, ncol),
    "O_" = model_matrices$O_ %|!|% curry(seq, ncol),
    "I_" = model_matrices$I_ %|!|% curry(seq, ncol),
    "G_" = model_matrices$G_ %|!|% curry(seq, length)))
  id_coefs <- sequentialize_index(number_of_coefs)
  id_coefs <- lapply(id_coefs, function(x) as.numeric(delta[x]))

  # add the components of the signal
  # the flow indicator (fi) accounts for sparsity
  vmult <- function(.id) as.vector(model_matrices[[.id]] %*% id_coefs[[.id]])
  n_d <- nrow(model_matrices$Y_[[1]])
  fi <- model_matrices$flow_indicator
  sig <- 0

  cf <- "const"
  if (!is.null(id_coefs[[cf]]))
    sig <- sig + id_coefs[[cf]] * (fi %||% 1)

  cf <- "const_intra"
  if (!is.null(id_coefs[[cf]]))
    sig <- sig + model_matrices[[cf]][[1]] * id_coefs[[cf]]

  cf <- "D_"
  if (!is.null(id_coefs[[cf]]))
    sig <- sig + vmult(cf) * (fi %||% 1)

  cf <- "O_"
  if (!is.null(id_coefs[[cf]]) & is.null(fi))
    sig <- sig + matrix(rep(vmult(cf),n_d),nrow = n_d,byrow = TRUE)
  if (!is.null(id_coefs[[cf]]) & !is.null(fi))
    sig <- sig + t(vmult(cf) * t(fi))

  cf <- "I_"
  if (!is.null(id_coefs[[cf]]))
    sig <- sig + Diagonal(n_d, vmult(cf) * (diag(fi) %||% 1))

  cf <- "G_"
  if (!is.null(id_coefs[[cf]]))
    sig <- sig + Reduce("+", Map("*", model_matrices[[cf]], id_coefs[[cf]]))

  return(sig)
}


#' @keywords internal
compute_expectation <- function(
  signal_matrix,
  DW,
  OW,
  rho,
  model,
  flow_indicator = NULL,
  approximate = TRUE,
  max_it = 10) {


  if (approximate) {

    update_signal <- function(signal_matrix) {


      decomposed_signal <- lag_flow_matrix(
        Y = signal_matrix,
        flow_indicator = flow_indicator,
        model = model,
        OW = OW,
        DW = DW,
        name = "SIG")

      tau <- c(1, -rho)
      signal <- Reduce("+", Map("*",decomposed_signal, tau))
      return(signal)

    }

    signal_sum <- signal_update <- signal_matrix
    for (i in seq(max_it)) {

      signal_update <- update_signal(signal_update)
      signal_sum <- signal_sum + signal_update
    }

    return(signal_sum)
  }


  # exact expectation
  assert(is.null(flow_indicator),
         "The exact expectation is not yet implemented for non-cartesian flows.")
  WF_parts <- expand_flow_neighborhood(DW = DW, OW = OW, model = model)
  A <- spatial_filter(weight_matrices = WF_parts, autoreg_parameters = rho)
  yhat <- solve(A, as.vector(signal_matrix))
  Yhat <- matrix(yhat, nrow = nrow(signal_matrix), ncol = ncol(signal_matrix))
  return(Yhat)
}


#' @keywords internal
compute_bp_insample <- function(
  signal_matrix,
  DW,
  OW,
  rho,
  model,
  flow_indicator = NULL,
  approximate = TRUE,
  max_it = 10) {

  stop("Best Prediction for in sample not yet implemented")
}

