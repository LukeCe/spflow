#' @keywords internal
drop_names <- function(.obj){
  .obj %>% data.table::setattr("names",NULL)
}
