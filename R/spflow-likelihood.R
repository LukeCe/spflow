#' @keywords internal
draw_initial_guess <- function(n_param) {
  init <- runif(n_param)

  if (n_param > 1) {
    norm <- (0.7 / 3) * n_param
    init <- norm * init / sum(init)
  }
  return(init)
}


#' @keywords internal
update_RSS <- function(TSS,ZZ,ZY,delta, tau){
  (tau %*% TSS - 2 * delta %*% ZY) %*% tau + delta %*% ZZ %*% delta
}
