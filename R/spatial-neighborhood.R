#' Derive the flow neighborhood matrices
#'
#' @description
#' Use the neighborhood matrices of origins and destinations to derive the
#' three neighborhood matrices of the origin-destination flows.
#'
#' @param OW Origin neighborhood matrix
#' @param DW Destination neighborhood matrix
#'
#' @export
expand_flow_neighborhood <- function(
  OW,
  DW = OW) {

  n_o <- nrow(OW)
  n_d <- nrow(DW)

  Wd <- Matrix::Diagonal(n_o) %x% DW
  Wo <- OW %x% Matrix::Diagonal(n_d)
  Ww <- OW %x% DW

  return(list("Wd" = Wd, "Wo" = Wo, "Ww" = Ww))
}
