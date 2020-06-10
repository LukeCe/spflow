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
