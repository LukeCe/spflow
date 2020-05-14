#' @export
expand_flow_neighborhood <- function(
  OW,
  DW = OW) {

  n_o <- nrow(OW)
  n_d <- nrow(DW)

  Wd <- Diagonal(n_o) %x% DW
  Wo <- OW            %x% Diagonal(n_d)
  Ww <- OW            %x% DW

  return(list("Wd" = Wd, "Wo" = Wo, "Ww" = Ww))
}
