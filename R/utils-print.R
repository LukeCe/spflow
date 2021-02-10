#' @keywords internal
print_line <- function(n_lines = 60, line_symbol = "-") {

  line <- paste(rep(line_symbol,n_lines),collapse = "")
  return(line)
}

#' @keywords internal
format_percent <- function(x) {
  sprintf(x * 100,fmt = "%1.2f%%")
}
