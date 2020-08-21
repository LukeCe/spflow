#' Derive the flow neighborhood matrices
#'
#' @description
#' Use the neighborhood matrices of origins and destinations to derive the
#' three neighborhood matrices of the origin-destination flows.
#'
#' @param OW Origin neighborhood matrix
#' @param DW Destination neighborhood matrix
#' @param n_o A numeric indicating the number of origins
#' @param n_d A numeric indicating the number of destinations
#' @param model A character indicating the model identfyer
#'
#' @export
expand_flow_neighborhood <- function(
  OW,
  DW,
  n_o = OW %|!|% nrow(OW),
  n_d = DW %|!|% nrow(DW),
  model = "model_9") {

  # initialize weight matrices to NULL
  # then replace those that are required for the chosen model
  Wd <- NULL
  Wo <- NULL
  Ww <- NULL

  model_number <- substr(model,7,7) %>% as.integer()

  d_models <- c(2,5:9)
  o_models <- c(3,5:9)
  w_models <- c(4,6,8,9)

  if (model_number %in% d_models) {
    assert(!is.null(n_o) && !is.null(DW),
           "Construction the destination weight matrix requires arguments" %p%
           " for n_o and DW!")
    Wd <- Matrix::Diagonal(n_o) %x% DW
  }

  if (model_number %in% o_models) {
    assert(!is.null(n_o) && !is.null(DW),
           "Construction the origin weight matrix requires arguments" %p%
             " for n_d and OW!")
    Wo <- OW %x% Matrix::Diagonal(n_d)

  }

  if (model_number %in% w_models) {
    assert(!is.null(n_o) && !is.null(DW),
           "Construction the origin weight matrix requires arguments" %p%
             " for DW and OW!")
    Ww <- OW %x% DW
  }

  return(list("Wd" = Wd, "Wo" = Wo, "Ww" = Ww) %>% compact())
}

spatial_filter <- function(
  weight_matrices,
  autoreg_parameters
) {

  combined_weight_matrices <-
    weight_matrices %>%
    safely_to_list() %>%
    mapply(FUN = "*", ., autoreg_parameters, SIMPLIFY = FALSE) %>%
    Reduce(f = "+", x = .) %>%
    Matrix::Matrix()

  N <- nrow(combined_weight_matrices)

  return(Matrix::Diagonal(N) - combined_weight_matrices)
}


spatial_model_order <- function(model = "model 9") {

  model_number <- substr(model,7,7)

  order0_models <- c(1)   %>% lookup(rep(0, length(.)), .)
  order1_models <- c(2:6) %>% lookup(rep(1, length(.)), .)
  order2_models <- c(7)   %>% lookup(rep(2, length(.)), .)
  order3_models <- c(8,9) %>% lookup(rep(3, length(.)), .)

  model_order <- c(order0_models,order1_models,order2_models,order3_models
                   )[model_number]

  return(as.integer(model_order))
}
