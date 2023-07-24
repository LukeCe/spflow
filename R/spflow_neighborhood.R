#' @title Derive the OD-level neighborhood matrices
#'
#' @description
#' Use the neighborhood matrices of origins and destinations to derive the
#' up to three neighborhood matrices of the origin-destination pairs:
#' - `W_d` relates to flows starting from the same origin going to neighbors of the destination
#' - `W_o` relates to flows starting from the neighbors of the origin going the same destination
#' - `W_w` relates to flows starting from the neighbors of the origin going the neighbors of the destination
#'
#' @param OW The origin neighborhood matrix
#' @param DW The destination neighborhood matrix
#' @param n_o A numeric indicating the number of origins
#' @param n_d A numeric indicating the number of destinations
#' @param M_indicator A matrix of binary indicators for origin-destination
#'   pairs that should be included in the model. When the argument is NULL or
#'   when all entries of this matrix are different from zero we consider the
#'   Cartesian product of all origins and destinations as OD pairs.
#' @inheritParams spflow_control
#'
#' @return A list of at most three (sparse) matrices with names
#'   given by `c("Wd", "Wo", "Ww")`.
#' @importFrom Matrix Diagonal bdiag drop0
#' @author Lukas Dargel
#' @export
#' @examples
#' expand_spflow_neighborhood(DW = 1-diag(2),OW = 1-diag(2))
expand_spflow_neighborhood <- function(
  OW,
  DW,
  n_o = nrow(OW),
  n_d = nrow(DW),
  M_indicator = NULL,
  model = "model_9") {

  model_number <- as.integer(substr(model,7,7))
  require_Wd <- model_number %in% c(2,5:9)
  require_Wo <- model_number %in% c(3,5:9)
  require_Ww <- model_number %in% c(4,6,8,9)

  error_msg <- "
  Construction of the %s weight matrix
  requires non-null input arguments for %s!"
  if (require_Wd)
    assert(!is.null(n_o) & !is.null(DW), error_msg, "destination", "n_o and DW")
  if (require_Wo)
    assert(!is.null(n_d) & !is.null(OW), error_msg, "origin", "n_d and OW")
  if (require_Ww)
    assert(!is.null(DW) & !is.null(OW), error_msg, "origin-to-destination", "OW and DW")

  ### Cartesian expansion
  if (all(M_indicator != 0))
    M_indicator <- NULL
  return_nbs <- c("Wd","Wo","Ww")[c(require_Wd,require_Wo,require_Ww)]
  return_nbs <- named_list(return_nbs)

  if (is.null(M_indicator)) {

      if (require_Wd) return_nbs$Wd <- Diagonal(n_o) %x% DW
      if (require_Wo) return_nbs$Wo <- OW %x% Diagonal(n_d)
      if (require_Ww) return_nbs$Ww <- OW %x% DW

      return(return_nbs)
    }


  ### non-Cartesian expansion
  # when Wo or Ww are required it is most efficient to reduce the problem
  # first to its minimal Cartesian representation
  M_indicator <- as(M_indicator, "lMatrix")
  if (require_Wd) {
    return_nbs$Wd <- apply(M_indicator, 2, function(d_index) DW[d_index,d_index])
    return_nbs$Wd <- bdiag(return_nbs$Wd)
  }

  if (require_Wo) {
    min_cartesian <- drop_superfluent_nodes(OW, DW, M_indicator)
    nb_true_origs <- ncol(min_cartesian$OW)

    diag_DW <- Diagonal(nrow(min_cartesian$flow_indicator))
    diag_template <- lapply(seq(nb_true_origs), function(orig_i) {
      dest_list <- min_cartesian$flow_indicator[,orig_i]
      diag_DW[,dest_list, drop = FALSE]})

    return_nbs$Wo <- lapply(seq(nb_true_origs), function(orig_i) {

      orig_weights <- min_cartesian$OW[orig_i,]
      dest_appears <- min_cartesian$flow_indicator[,orig_i]

      result_orig_i <- do.call("cbind", Map("*",diag_template, orig_weights))
      drop0(result_orig_i[dest_appears,, drop = FALSE])
    })
    return_nbs$Wo <- do.call("rbind", return_nbs$Wo)
  }

  if (require_Ww) {

    if (!require_Wo) {
      min_cartesian <- drop_superfluent_nodes(OW, DW, M_indicator)
      nb_true_origs <- ncol(min_cartesian$OW)
    }

    DW_template <- lapply(seq(nb_true_origs), function(orig_i) {
      dest_list <- min_cartesian$flow_indicator[,orig_i]
      min_cartesian$DW[,dest_list, drop = FALSE]})
    return_nbs$Ww <- lapply(seq(nb_true_origs), function(orig_i) {
      orig_weights <- min_cartesian$OW[orig_i,]
      dest_appears <- min_cartesian$flow_indicator[,orig_i]

      result_orig_i <- do.call("cbind", Map("*",DW_template, orig_weights))
      drop0(result_orig_i[dest_appears,, drop = FALSE])

    })
    return_nbs$Ww <- do.call("rbind", return_nbs$Ww)
  }

  return(return_nbs)
}


#' @title Derive the filter matrix the spatial interaction model
#'
#' @description
#' Use the neighborhood matrices of origins and destinations to derive the
#' three neighborhood matrices of the origin-destination flows.
#'
#' @param weight_matrices
#'   A list of OD-pair neighborhood matrices
#' @param autoreg_parameters
#'   A numeric containing values for the autoregression parameters
#'
#' @importFrom Matrix Diagonal Matrix
#' @keywords internal
#' @noRd
spatial_filter <- function(
  weight_matrices,
  autoreg_parameters) {

  if (!is.list(weight_matrices))
    weight_matrices <- list(weight_matrices)

  combined_weight_matrices <- Map("*", weight_matrices,autoreg_parameters)
  combined_weight_matrices <- Matrix(Reduce("+", combined_weight_matrices))

  N <- nrow(combined_weight_matrices)
  A <- Diagonal(N) - combined_weight_matrices
  return(A)
}


#' @title Derive the order of the spatial model
#'
#' @description
#' The order of a spatial model corresponds to the number of neighborhood
#' matrices that is used to describe the spatial dependence.
#'
#' @inheritParams spflow_control
#' @return An integer corresponding to the number of weight matrices
#' @keywords internal
#' @noRd
spatial_model_order <- function(model = "model_9") {

  model_number <- substr(model,7,7)

  order0_models <- lookup(0,1)
  order1_models <- lookup(1,2:6)
  order2_models <- lookup(2,7)
  order3_models <- lookup(3,c(8,9))

  model_order <- c(order0_models,order1_models,order2_models,order3_models
  )[model_number]

  return(as.integer(model_order))
}



#' @keywords internal
normalize_neighborhood <- function(mat, by_row = FALSE) {

  assert(has_equal_elements(dim(mat)),
         "Neighborhood matrices musst be square!")

  diag(mat) <- 0
  mat <- drop0(mat)
  if (by_row) {
    m_scale <- rowSums(mat)
    m_scale[m_scale == 0] <- 1
    mat <- mat / m_scale
  }

  c_spec <- charactrize_spectrum(mat)
  spectral_radius <- abs(c_spec["LM"])
  if (!by_row & spectral_radius != 1) {
    c_spec <- c_spec / spectral_radius
    mat <- mat / spectral_radius
  }

  attr_spectral_character(mat) <- c_spec
  return(mat)
}


#' @keywords internal
pull_spflow_neighborhood <-  function(spflow_network_multi, id_net_pair) {
  od_id <- id(spflow_network_multi@pairs[[id_net_pair]])
  neighbor_mats <- lapply(c("OW" = "orig", "DW" = "dest"), function(.key) {
    m <- neighborhood(spflow_network_multi, od_id[.key])
    dimnames(m) <- list(NULL,NULL)
    return(m)
  })

  return(compact(neighbor_mats))
}

# ---- helpers ----------------------------------------------------------------
#' @keywords internal
drop_superfluent_nodes <- function(OW,DW,M_indicator) {
  # Remove origins or destinations from the neighborhood matrix that do
  # not appear in any of the OD pairs.
  true_origs <- colSums(M_indicator) != 0
  true_dests <- rowSums(M_indicator) != 0

  return(list(OW = DW %|!|% OW[true_origs,true_origs],
              DW = DW %|!|% DW[true_dests,true_dests],
              flow_indicator = M_indicator[true_dests, true_origs]))
}

#' @keywords internal
get_flow_indicator <- function(spflow_pairs) {

  do_indexes <- get_do_indexes(dat(spflow_pairs))
  n_nodes <- nnodes(spflow_pairs)
  indicator_mat <- matrix_format_d_o(
    values = NULL,
    dest_index = do_indexes[,1],
    orig_index = do_indexes[,2],
    num_dest = n_nodes["dest"],
    num_orig = n_nodes["orig"],
    assume_ordered = TRUE)
  return(indicator_mat)
}

#' @importMethodsFrom Matrix isSymmetric
#' @importFrom RSpectra eigs eigs_sym
#' @keywords internal
charactrize_spectrum <- function(mat) {

  stopifnot(nrow(mat) == ncol(mat))

  eigenvalues <- c(
    "LM" = "Largest magnitude",
    "LR" = "Largest real",
    "SR" = "Smallest real")

  if (nrow(mat) < 3){
    ev_obs <- eigen(mat)$values
    ev_obs <- lookup(ev_obs[c(1,1,2)], names(eigenvalues))
    return(ev_obs)
  }


  ev_methods <- lookup(names(eigenvalues))
  sym_method <- c("matrix", "dgeMatrix", "dgCMatrix", "dgRMatrix")
  if (isSymmetric(mat) && class(mat) %in% sym_method) {
    ev_methods[2:3] <- c("LA","SA")
    eigs <- eigs_sym
  }

  eigenvalues <- unlist(lapply(
    ev_methods,
    function(.w) eigs(mat,which = .w, k = 1, opts = list(retvec = FALSE))[["values"]]))

  return(eigenvalues)
  }


#' @keywords internal
attr_spectral_character <- function(mat) {
  attr(mat, "spectral_character")
}

#' @keywords internal
`attr_spectral_character<-` <- function(mat, value) {
  attr(mat, "spectral_character") <- value
  mat
}

