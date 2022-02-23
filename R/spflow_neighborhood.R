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
expand_flow_neighborhood <- function(
  OW,
  DW,
  n_o = OW %|!|% nrow(OW),
  n_d = DW %|!|% nrow(DW),
  flow_indicator = NULL,
  model = "model_9") {


  model_number <- as.integer(substr(model,7,7))
  require_Wd <- model_number %in% c(2,5:9)
  require_Wo <- model_number %in% c(3,5:9)
  require_Ww <- model_number %in% c(4,6,8,9)

  check_args <- "Construction of the %s weight matrix requires non-null input arguments for %s!"
  assert(require_Wd & !any(is.null(n_o),is.null(DW)),
         check_args, "destination", "n_o and DW")
  assert(require_Wo & !any(is.null(n_d),is.null(OW)),
         check_args, "origin", "n_d and OW")
  assert(require_Ww & !any(is.null(DW),is.null(OW)),
         check_args, "origin-to-destination", "OW and DW")


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

  n_nodes <- nnodes(sp_net_pair)
  complete <-  npairs(sp_net_pair) / prod(n_nodes)
  od_indexes <- lapply(get_keys(sp_net_pair), "as.integer")

  if (complete >= 1)
    return(1)

  if (complete >= .5) {
    indicator_mat <- matrix(0L,nrow = n_nodes["dest"], ncol = n_nodes["orig"])
    indicator_mat[cbind(od_indexes[[2]],od_indexes[[1]])] <- 1L
  }

  if (complete < .5) {
    indicator_mat <- sparseMatrix(i = od_indexes[[2]],
                                  j = od_indexes[[1]],
                                  dims = nnodes(sp_net_pair)[2:1])
  }
  return(indicator_mat)
}
