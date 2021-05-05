#' @keywords internal
left_pad <- function(vec, len = 5,  pad = "") {
  pad <- vapply(vec,
                function(.x) paste0("", rep(pad, min(0, len - nchar(.x)))),
                FUN.VALUE = character(1), USE.NAMES = FALSE)
  paste0(vec,pad)
}

#' @keywords internal
print_line <- function(n_lines = 60, line_symbol = "-") {

  line <- paste(rep(line_symbol,n_lines),collapse = "")
  return(line)
}

#' @keywords internal
format_percent <- function(x) {
  sprintf(x * 100,fmt = "%1.2f%%")
}
