#' @title Derive the flow neighborhood matrices
#'
#' @description
#' Use the neighborhood matrices of origins and destinations to derive the
#' three neighborhood matrices of the origin-destination flows.
#' This function is used for simulations or for comparisons with the vectorized
#' formulations of the model.
#'
#' @param OW Origin neighborhood matrix
#' @param DW Destination neighborhood matrix
#' @param n_o A numeric indicating the number of origins
#' @param n_d A numeric indicating the number of destinations
#' @param model A character indicating the model identifier
#' @param flow_indicator A matrix of binary indicators for origin-destination
#'   pairs that should be included in the model. When the argument is NULL or
#'   when all entries of this matrix are different from zero we consider the
#'   Cartesian product of all origins and destinations as OD pairs.
#'
#' @return A list of at most three (sparse) matrices with names
#'   given by c("Wd", "Wo", "Ww").
#' @family spflow simulation functions
#' @importFrom Matrix Diagonal bdiag drop0
#' @export
expand_spflow_neighborhood <- function(
  OW,
  DW,
  n_o = nrow(OW),
  n_d = nrow(DW),
  flow_indicator = NULL,
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
  if (all(flow_indicator == 1))
    flow_indicator <- NULL
  return_nbs <- c("Wd","Wo","Ww")[c(require_Wd,require_Wo,require_Ww)]
  return_nbs <- named_list(return_nbs)

  if (is.null(flow_indicator)) {

      if (require_Wd) return_nbs$Wd <- Diagonal(n_o) %x% DW
      if (require_Wo) return_nbs$Wo <- OW %x% Diagonal(n_d)
      if (require_Ww) return_nbs$Ww <- OW %x% DW

      return(return_nbs)
    }


  ### non-Cartesian expansion
  # when Wo or Ww are required it is most efficient to reduce the problem
  # first to its minimal Cartesian representation
  flow_indicator <- as(flow_indicator, "lgCMatrix")
  if (require_Wd) {
    return_nbs$Wd <- apply(flow_indicator, 2, function(d_index) DW[d_index,d_index])
    return_nbs$Wd <- bdiag(return_nbs$Wd)
  }

  if (require_Wo) {
    min_cartesian <- drop_superfluent_nodes(OW, DW, flow_indicator)
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
      min_cartesian <- drop_superfluent_nodes(OW, DW, flow_indicator)
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
#'
#' @param weight_matrices List of flow neighborhood matrices
#' @param autoreg_parameters A numeric containing values for the
#'     auto-regressive parameters
#'
#' @family spflow simulation functions
#' @importFrom Matrix Diagonal Matrix
#' @keywords internal
spatial_filter <- function(
  weight_matrices,
  autoreg_parameters) {

  combined_weight_matrices <-
    Map("*", safely_to_list(weight_matrices),autoreg_parameters)
  combined_weight_matrices <-
    Matrix(Reduce("+", combined_weight_matrices))

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
pull_spflow_neighborhood <-  function(spflow_multinet, id_spflow_pairs) {
  od_id <- id(spflow_multinet@pairs[[id_spflow_pairs]])
  neighbor_mats <- lapply(c("OW" = "orig", "DW" = "dest"), function(.key) {
    m <- neighborhood(spflow_multinet, od_id[.key])
    dimnames(m) <- list(NULL,NULL)
    return(m)
  })

  return(compact(neighbor_mats))
}

#' @keywords interal
valdiate_spflow_neighborhood <- function(
    spflow_neighborhood,
    model,
    do_normalisation = TRUE) {

  model_num <- as.numeric(substr(model,7,7))
  req_OW <- model_num %in% c(3:9)
  req_DW <- model_num %in% c(2,4:9)

  assert(!req_OW  || !is.null(spflow_neighborhood[["OW"]]),
         "For model_%s you the origin neighborhood musst be available!",
         model_num)
  assert(!req_DW || !is.null(spflow_neighborhood[["DW"]]),
         "For model_%s you the destination neighborhood musst be available!",
         model_num)


  spectral_radi <- lapply(spflow_neighborhood, function(.XW) abs(attr_spectral_character(.XW)["LM"]))
  tol <- sqrt(.Machine$double.eps)
  unit_radi <- all(abs(unlist(spectral_radi) - 1) < tol)
  if (unit_radi)
    return(spflow_neighborhood)

  row_sums <- lapply(spflow_neighborhood, rowSums)
  row_sums_0or1 <- lapply(row_sums, function(.rs) all(abs(abs(.rs - .5) - .5) < tol))
  row_normalized <- all(unlist(row_sums_0or1))
  if (row_normalized)
    return(spflow_neighborhood)

  assert(do_normalisation, "The neighborhood matrices should be normalized!")
  spflow_neighborhood <- Map(
    function(.W, .sr) {
      .W <- .W / .sr
      attr_spectral_character(.W) <- attr_spectral_character(.W) / .sr
      .W
    },
    .W = spflow_neighborhood,
    .sr = spectral_radi)

  return(spflow_neighborhood)
}



# ---- helpers ----------------------------------------------------------------
#' @title Remove origins or destinations from the neighborhood matrix that do
#'   not appear in any of the OD pairs.
#' @keywords internal
drop_superfluent_nodes <- function(OW,DW,flow_indicator) {

  true_origs <- colSums(flow_indicator) != 0
  true_dests <- rowSums(flow_indicator) != 0

  return(list(OW = DW %|!|% OW[true_origs,true_origs],
              DW = DW %|!|% DW[true_dests,true_dests],
              flow_indicator = flow_indicator[true_dests, true_origs]))
}

#' @keywords internal
get_flow_indicator <- function(sp_net_pair) {

  do_indexes <- get_do_indexes(dat(sp_net_pair))
  n_nodes <- nnodes(sp_net_pair)
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
#' @importFrom RSpectra eigs
#' @keywords internal
charactrize_spectrum <- function(mat) {

  stopifnot(nrow(mat) == ncol(mat))

  eigenvalues <- c(
    "LM" = "Largest magnitude",
    "LR" = "Largest real",
    "SR" = "Smallest real")

  if (nrow(mat) < 3){
    ev_obs <- eigen(mat)$values
    eigenvalues[1:3] <- ev_obs[1]
    eigenvalues[2:3] <- ev_obs
    return(eigenvalues)
  }



  ev_methods <- lookup(names(eigenvalues))
  if (isSymmetric(mat))
    ev_methods[2:3] <- c("LA","SA")

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

