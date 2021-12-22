#' @importFrom Matrix Diagonal
#' @keywords internal
compute_signal <- function(model_matrices, delta) {

  # index the coefficient vectors according to the model segments
  sub_index <- list(
    "const" = model_matrices$constants$global %||% 0,
    "const_intra" = 1 - is.null(model_matrices$constants$intra),
    "D_" = seq_len(ncol(model_matrices$D_) %||% 0) %||% 0,
    "O_" = seq_len(ncol(model_matrices$O_) %||% 0) %||% 0,
    "I_" = seq_len(ncol(model_matrices$I_) %||% 0) %||% 0,
    "G_" = seq_len(length(model_matrices$G)) %||% 0)
  sub_index <- sequentialize_index(sub_index)

  ## Calculate the components of the signal.
  # Missing components are set to zero and do not affect the final sum.
  # Number of destinations is required because of recycling.
  vector_or_null <- function(part_id) {
    .v <- model_matrices[[part_id]]
    .v %|!|% as.vector(.v %*% delta[sub_index[[part_id]]])
  }
  n_d <- nrow(model_matrices$Y_[[1]])

  # constant
  const <- delta[sub_index$const]

  # intra constant
  const_intra <- model_matrices$constants$intra %|!|%
    Diagonal(delta[sub_index$const_intra], n = n_d)
  const_intra <- const_intra %||% 0

  # destination part - (vector is recycled)
  dest <- vector_or_null("D_")
  dest <- dest %||% 0

  # origin part - vector is not recycled correctly ...
  orig <- vector_or_null("O_")
  orig <- orig %|!|% matrix(rep(orig,n_d),nrow = n_d,byrow = TRUE)
  orig <- orig %||% 0

  # intra part - only for diagonal elements
  intra <- vector_or_null("I_")
  intra <- intra %|!|% Diagonal(intra,n = length(intra))
  intra <- intra %||% 0

  # G part - (origin-destination pair attributes)
  g_term <- 0
  if (length(model_matrices$G_) != 0 ) {
    g_term <- Map("*", model_matrices$G_, delta[sub_index$G_])
    g_term <- as.matrix(Reduce("+", g_term))
  }

  signal <- const + const_intra + dest + orig + intra + g_term
  return(signal)
}


#' @keywords internal
compute_expectation <- function(
  signal_matrix,
  DW,
  OW,
  rho,
  model,
  Y_indicator = NULL,
  approximate = TRUE,
  max_it = 10) {


  if (approximate) {

    update_signal <- function(signal_matrix) {


      decomposed_signal <- lag_flow_matrix(
        Y = signal_matrix,
        Y_indicator = Y_indicator,
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
  assert(is.null(Y_indicator),
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
  Y_indicator = NULL,
  approximate = TRUE,
  max_it = 10) {

  stop("Best Prediction for in sample not yet implemented")
}

