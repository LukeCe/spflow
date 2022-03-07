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

#' @keywords internal
sort_chars <- function(charvec){
  unlist(lapply(charvec, function(string) {
    paste0(sort(strsplit(string, "*")[[1]]),collapse = "")
  }))
}

#' @keywords internal
count_pattern <- function(charvec, pattern){

  charvec_p <- unlist(lapply(charvec, "strsplit", "*"), recursive = FALSE)
  charvec_p <- unlist(lapply(charvec_p, function(x) length(grep(pattern, x))))
  return(charvec_p)
}
