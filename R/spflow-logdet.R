trace_sequence <- function(W, max_power = 10 ) {

  W_traces <- vector(mode = "list", length = max_power + 1)
  W_pow <- W
  W_traces[[1]] <- sum(diag(W_pow))

  for (pow in seq_len(max_power)) {
    W_pow <- W %*% W_pow
    W_traces[[pow + 1]] <-  sum(diag(W_pow))
  }

  return(unlist(W_traces))
}

#' Log determinant ???
#'
#' @param parms  ??
#' @param traces ??
#' @param n ??
#' @param model ??
#'
#' @return ??
#' @keywords internal
lndetmc <- function(
  parms,
  traces,
  n,
  model) {

  # condition
  stopifnot(length(parms) == 1)

  # initialization
  m <- length(traces)
  res <- parms^(1:m)/(1:m)

  if (model %in% c("model_2", "model_3")) {
    res <- -n * sum(traces * res)
  }

  if (model == "model_4") {
    res <- - sum(traces ^ 2 * res)
  }

  if (model == "model_5") {
    # we add n to the values of traces
    traces_new <- c(n, traces)
    tr_W_s <- numeric(m)
    for (k in 1:m) {
      for (j in 0:k) {
        tr_W_s[k] <- tr_W_s[k] + choose(k, j) * traces_new[k - j + 1] * traces_new[j + 1]
      }
      tr_W_s[k] <- tr_W_s[k] * (1/2) ^ k
    }
    res <- - sum(tr_W_s * res)
  }

  if (model == "model_6") {
    res <- - sum(res * (2 * traces + traces ^ 2) / 3)
  }

  return(res)
}




#' Fast determinant calculation
#'
#' @param parms ?
#' @param traces ?
#' @param n ?
#' @param dev ?
#'
#' @keywords internal
#' @return ?
fodet1 <- function(parms, traces, n, dev = T) {

  # initialization
  nb_rho <- length(parms)
  # condition
  stopifnot(nb_rho == 3) #, all(parms != 0))

  # initialization
  titer <- 800
  rez <- numeric(titer)

  # precision machine
  eps <- .Machine$double.eps

  # initialisation
  miter <- length(traces)
  tw <- c(traces, n)
  op <- rep(1, 3)

  p <- parms
  vvv <- p

  if (!dev) {
    for (miteri in 2:miter) {
      vprod <- rep(1, 3^miteri)
      vis <- rep(0, 3^miteri)
      for (k in 1:miteri) {
        vvv_temp <- rep(parms, each = 3^(miteri - k))
        vprod <- vprod * vvv_temp
        vis <- vis + as.numeric(vvv_temp == p[1])
      }
      vks <- rev(vis)
      vjs <- miteri - vis - vks

      va <- vis + vks
      vb <- vjs + vks

      vaa <- va + (va == 0) * (miter + 1)
      vbb <- vb + (vb == 0) * (miter + 1)

      ts <- tw[vaa] * tw[vbb]
      tparts <- vprod * ts

      tracei <- sum(tparts)
      rez[miteri] <- tracei/miteri
    }
  } else {
    if (miter >= 2)
      rez[2] <- n * tw[2] * parms[2] ^ 2 + tw[1] * tw[1] * parms[1] * parms[2] * 2 + tw[1] * tw[2] * parms[3] * parms[2] * 2 +
        tw[2] * n * parms[1] ^ 2 + tw[2] * tw[1] * parms[3] * parms[1] * 2 + tw[2] * tw[2] * parms[3] ^ 2

    if (miter >= 3)
      rez[3] <- n * tw[3] * parms[2] ^ 3 + tw[1] * tw[2] * parms[1] * parms[2] ^ 2 * 3 + tw[1] * tw[3] * parms[3] * parms[2] ^ 2 * 3 +
        tw[2] * tw[1] * parms[1] ^ 2 * parms[2] * 3 + tw[2] * tw[2] * parms[3] * parms[1] * parms[2] * 6 + tw[2] * tw[3] * parms[3] ^ 2 * parms[2] * 3 +
        tw[3] * n * parms[1] ^ 3 * 1 + tw[3] * tw[1] * parms[3] * parms[1] ^ 2 * 3 + tw[3] * tw[2] * parms[3] ^ 2 * parms[1] * 3 +
        tw[3] * tw[3] * parms[3] ^ 3

    if (miter >= 4)
      rez[4] <- n * tw[4] * parms[2] ^ 4 + tw[1] * tw[3] * parms[1] * parms[2] ^ 3 * 4 +
        tw[1] * tw[4] * parms[3] * parms[2] ^ 3 * 4 + tw[2] * tw[2] * parms[1] ^ 2 * parms[2] ^ 2 * 6 +
        tw[2] * tw[3] * parms[3] * parms[1] * parms[2] ^ 2 * 12 + tw[2] * tw[4] * parms[3] ^ 2 * parms[2] ^ 2 * 6 +
        tw[3] * tw[1] * parms[1] ^ 3 * parms[2] * 4 + tw[3] * tw[2] * parms[3] * parms[1] ^ 2 * parms[2] * 12 +
        tw[3] * tw[3] * parms[3] ^ 2 * parms[1] * parms[2] * 12 + tw[3] * tw[4] * parms[3] ^ 3 * parms[2] * 4 +
        tw[4] * n * parms[1] ^ 4 + tw[4] * tw[1] * parms[3] * parms[1] ^ 3 * 4 + tw[4] * tw[2] * parms[3] ^ 2 * parms[1] ^ 2 * 6 +
        tw[4] * tw[3] * parms[3] ^ 3 * parms[1] * 4 + tw[4] * tw[4] * parms[3] ^ 4

    if (miter >= 5)
      rez[5] <- n * tw[5] * parms[2] ^ 5 + tw[1] * tw[4] * parms[1] * parms[2] ^ 4 * 5 +
        tw[1] * tw[5] * parms[3] * parms[2] ^ 4 * 5 + tw[2] * tw[3] * parms[1] ^ 2 * parms[2] ^ 3 * 10 +
        tw[2] * tw[4] * parms[3] * parms[1] * parms[2] ^ 3 * 20 + tw[2] * tw[5] * parms[3] ^ 2 * parms[2] ^ 3 * 10 +
        tw[3] * tw[2] * parms[1] ^ 3 * parms[2] ^ 2 * 10 + tw[3] * tw[3] * parms[3] * parms[1] ^ 2 * parms[2] ^ 2 * 30 +
        tw[3] * tw[4] * parms[3] ^ 2 * parms[1] * parms[2] ^ 2 * 30 + tw[3] * tw[5] * parms[3] ^ 3 * parms[2] ^ 2 * 10 +
        tw[4] * tw[1] * parms[1] ^ 4 * parms[2] * 5 + tw[4] * tw[2] * parms[3] * parms[1] ^ 3 * parms[2] * 20 +
        tw[4] * tw[3] * parms[3] ^ 2 * parms[1] ^ 2 * parms[2] * 30 + tw[4] * tw[4] * parms[3] ^ 3 * parms[1] * parms[2] * 20 +
        tw[4] * tw[5] * parms[3] ^ 4 * parms[2] * 5 + tw[5] * n * parms[1] ^ 5 * 1 + tw[5] * tw[1] * parms[3] * parms[1] ^ 4 * 5 +
        tw[5] * tw[2] * parms[3] ^ 2 * parms[1] ^ 3 * 10 + tw[5] * tw[3] * parms[3] ^ 3 * parms[1] ^ 2 * 10 +
        tw[5] * tw[4] * parms[3] ^ 4 * parms[1] * 5 + tw[5] * tw[5] * parms[3] ^ 5

    if (miter >= 6)
      rez[6] <- n * tw[6] * parms[2] ^ 6 + tw[1] * tw[5] * parms[1] * parms[2] ^ 5 * 6 +
        tw[1] * tw[6] * parms[3] * parms[2] ^ 5 * 6 + tw[2] * tw[4] * parms[1] ^ 2 * parms[2] ^ 4 * 15 +
        tw[2] * tw[5] * parms[3] * parms[1] * parms[2] ^ 4 * 30 + tw[2] * tw[6] * parms[3] ^ 2 * parms[2] ^ 4 * 15 +
        tw[3] * tw[3] * parms[1] ^ 3 * parms[2] ^ 3 * 20 + tw[3] * tw[4] * parms[3] * parms[1] ^ 2 * parms[2] ^ 3 * 60 +
        tw[3] * tw[5] * parms[3] ^ 2 * parms[1] * parms[2] ^ 3 * 60 + tw[3] * tw[6] * parms[3] ^ 3 * parms[2] ^ 3 * 20 +
        tw[4] * tw[2] * parms[1] ^ 4 * parms[2] ^ 2 * 15 + tw[4] * tw[3] * parms[3] * parms[1] ^ 3 * parms[2] ^ 2 * 60 +
        tw[4] * tw[4] * parms[3] ^ 2 * parms[1] ^ 2 * parms[2] ^ 2 * 90 + tw[4] * tw[5] * parms[3] ^ 3 * parms[1] * parms[2] ^ 2 * 60 +
        tw[4] * tw[6] * parms[3] ^ 4 * parms[2] ^ 2 * 15 + tw[5] * tw[1] * parms[1] ^ 5 * parms[2] * 6 +
        tw[5] * tw[2] * parms[3] * parms[1] ^ 4 * parms[2] * 30 + tw[5] * tw[3] * parms[3] ^ 2 * parms[1] ^ 3 * parms[2] * 60 +
        tw[5] * tw[4] * parms[3] ^ 3 * parms[1] ^ 2 * parms[2] * 60 + tw[5] * tw[5] * parms[3] ^ 4 * parms[1] * parms[2] * 30 +
        tw[5] * tw[6] * parms[3] ^ 5 * parms[2] * 6 + tw[6] * n * parms[1] ^ 6 * 1 + tw[6] * tw[1] * parms[3] * parms[1] ^ 5 * 6 +
        tw[6] * tw[2] * parms[3] ^ 2 * parms[1] ^ 4 * 15 + tw[6] * tw[3] * parms[3] ^ 3 * parms[1] ^ 3 * 20 +
        tw[6] * tw[4] * parms[3] ^ 4 * parms[1] ^ 2 * 15 + tw[6] * tw[5] * parms[3] ^ 5 * parms[1] * 6 +
        tw[6] * tw[6] * parms[3] ^ 6 * 1

    if (miter >= 7)
      rez[7] <- n * tw[7] * parms[2] ^ 7 * 1 + tw[1] * tw[6] * parms[1] * parms[2] ^ 6 * 7 + tw[1] * tw[7] * parms[3] * parms[2] ^ 6 * 7 +
        tw[2] * tw[5] * parms[1] ^ 2 * parms[2] ^ 5 * 21 + tw[2] * tw[6] * parms[3] * parms[1] * parms[2] ^ 5 * 42 +
        tw[2] * tw[7] * parms[3] ^ 2 * parms[2] ^ 5 * 21 + tw[3] * tw[4] * parms[1] ^ 3 * parms[2] ^ 4 * 35 +
        tw[3] * tw[5] * parms[3] * parms[1] ^ 2 * parms[2] ^ 4 * 105 + tw[3] * tw[6] * parms[3] ^ 2 * parms[1] * parms[2] ^ 4 * 105 +
        tw[3] * tw[7] * parms[3] ^ 3 * parms[2] ^ 4 * 35 + tw[4] * tw[3] * parms[1] ^ 4 * parms[2] ^ 3 * 35 +
        tw[4] * tw[4] * parms[3] * parms[1] ^ 3 * parms[2] ^ 3 * 140 + tw[4] * tw[5] * parms[3] ^ 2 * parms[1] ^ 2 * parms[2] ^ 3 * 210 +
        tw[4] * tw[6] * parms[3] ^ 3 * parms[1] * parms[2] ^ 3 * 140 + tw[4] * tw[7] * parms[3] ^ 4 * parms[2] ^ 3 * 35 +
        tw[5] * tw[2] * parms[1] ^ 5 * parms[2] ^ 2 * 21 + tw[5] * tw[3] * parms[3] * parms[1] ^ 4 * parms[2] ^ 2 * 105 +
        tw[5] * tw[4] * parms[3] ^ 2 * parms[1] ^ 3 * parms[2] ^ 2 * 210 + tw[5] * tw[5] * parms[3] ^ 3 * parms[1] ^ 2 * parms[2] ^ 2 * 210 +
        tw[5] * tw[6] * parms[3] ^ 4 * parms[1] * parms[2] ^ 2 * 105 + tw[5] * tw[7] * parms[3] ^ 5 * parms[2] ^ 2 * 21 +
        tw[6] * tw[1] * parms[1] ^ 6 * parms[2] * 7 + tw[6] * tw[2] * parms[3] * parms[1] ^ 5 * parms[2] * 42 +
        tw[6] * tw[3] * parms[3] ^ 2 * parms[1] ^ 4 * parms[2] * 105 + tw[6] * tw[4] * parms[3] ^ 3 * parms[1] ^ 3 * parms[2] * 140 +
        tw[6] * tw[5] * parms[3] ^ 4 * parms[1] ^ 2 * parms[2] * 105 + tw[6] * tw[6] * parms[3] ^ 5 * parms[1] * parms[2] * 42 +
        tw[6] * tw[7] * parms[3] ^ 6 * parms[2] * 7 + tw[7] * n * parms[1] ^ 7 * 1 + tw[7] * tw[1] * parms[3] * parms[1] ^ 6 * 7 +
        tw[7] * tw[2] * parms[3] ^ 2 * parms[1] ^ 5 * 21 + tw[7] * tw[3] * parms[3] ^ 3 * parms[1] ^ 4 * 35 +
        tw[7] * tw[4] * parms[3] ^ 4 * parms[1] ^ 3 * 35 + tw[7] * tw[5] * parms[3] ^ 5 * parms[1] ^ 2 * 21 +
        tw[7] * tw[6] * parms[3] ^ 6 * parms[1] * 7 + tw[7] * tw[7] * parms[3] ^ 7

    if (miter >= 8)
      rez[8] <- n * tw[8] * parms[2] ^ 8  + tw[1] * tw[7] * parms[1] * parms[2] ^ 7 * 8 +
        tw[1] * tw[8] * parms[3] * parms[2] ^ 7 * 8 + tw[2] * tw[6] * parms[1] ^ 2 * parms[2] ^ 6 * 28 +
        tw[2] * tw[7] * parms[3] * parms[1] * parms[2] ^ 6 * 56 + tw[2] * tw[8] * parms[3] ^ 2 * parms[2] ^ 6 * 28 +
        tw[3] * tw[5] * parms[1] ^ 3 * parms[2] ^ 5 * 56 + tw[3] * tw[6] * parms[3] * parms[1] ^ 2 * parms[2] ^ 5 * 168 +
        tw[3] * tw[7] * parms[3] ^ 2 * parms[1] * parms[2] ^ 5 * 168 + tw[3] * tw[8] * parms[3] ^ 3 * parms[2] ^ 5 * 56 +
        tw[4] * tw[4] * parms[1] ^ 4 * parms[2] ^ 4 * 70 + tw[4] * tw[5] * parms[3] * parms[1] ^ 3 * parms[2] ^ 4 * 280 +
        tw[4] * tw[6] * parms[3] ^ 2 * parms[1] ^ 2 * parms[2] ^ 4 * 420 + tw[4] * tw[7] * parms[3] ^ 3 * parms[1] * parms[2] ^ 4 * 280 +
        tw[4] * tw[8] * parms[3] ^ 4 * parms[2] ^ 4 * 70 + tw[5] * tw[3] * parms[1] ^ 5 * parms[2] ^ 3 * 56 +
        tw[5] * tw[4] * parms[3] * parms[1] ^ 4 * parms[2] ^ 3 * 280 + tw[5] * tw[5] * parms[3] ^ 2 * parms[1] ^ 3 * parms[2] ^ 3 * 560 +
        tw[5] * tw[6] * parms[3] ^ 3 * parms[1] ^ 2 * parms[2] ^ 3 * 560 + tw[5] * tw[7] * parms[3] ^ 4 * parms[1] * parms[2] ^ 3 * 280 +
        tw[5] * tw[8] * parms[3] ^ 5 * parms[2] ^ 3 * 56 + tw[6] * tw[2] * parms[1] ^ 6 * parms[2] ^ 2 * 28 + tw[6] * tw[3] * parms[3] * parms[1] ^ 5 * parms[2] ^ 2 * 168 +
        tw[6] * tw[4] * parms[3] ^ 2 * parms[1] ^ 4 * parms[2] ^ 2 * 420 + tw[6] * tw[5] * parms[3] ^ 3 * parms[1] ^ 3 * parms[2] ^ 2 * 560 +
        tw[6] * tw[6] * parms[3] ^ 4 * parms[1] ^ 2 * parms[2] ^ 2 * 420 + tw[6] * tw[7] * parms[3] ^ 5 * parms[1] * parms[2] ^ 2 * 168 +
        tw[6] * tw[8] * parms[3] ^ 6 * parms[2] ^ 2 * 28 + tw[7] * tw[1] * parms[1] ^ 7 * parms[2] * 8 +
        tw[7] * tw[2] * parms[3] * parms[1] ^ 6 * parms[2] * 56 + tw[7] * tw[3] * parms[3] ^ 2 * parms[1] ^ 5 * parms[2] * 168 +
        tw[7] * tw[4] * parms[3] ^ 3 * parms[1] ^ 4 * parms[2] * 280 + tw[7] * tw[5] * parms[3] ^ 4 * parms[1] ^ 3 * parms[2] * 280 +
        tw[7] * tw[6] * parms[3] ^ 5 * parms[1] ^ 2 * parms[2] * 168 + tw[7] * tw[7] * parms[3] ^ 6 * parms[1] * parms[2] * 56 +
        tw[7] * tw[8] * parms[3] ^ 7 * parms[2] * 8 + tw[8] * n * parms[1] ^ 8 + tw[8] * tw[1] * parms[3] * parms[1] ^ 7 * 8 +
        tw[8] * tw[2] * parms[3] ^ 2 * parms[1] ^ 6 * 28 + tw[8] * tw[3] * parms[3] ^ 3 * parms[1] ^ 5 * 56 +
        tw[8] * tw[4] * parms[3] ^ 4 * parms[1] ^ 4 * 70 + tw[8] * tw[5] * parms[3] ^ 5 * parms[1] ^ 3 * 56 +
        tw[8] * tw[6] * parms[3] ^ 6 * parms[1] ^ 2 * 28 + tw[8] * tw[7] * parms[3] ^ 7 * parms[1] * 8 + tw[8] * tw[8] * parms[3] ^ 8

    if (miter >= 9)
      rez[9] <- n * tw[9] * parms[2] ^ 9 * 1 + tw[1] * tw[8] * parms[1] * parms[2] ^ 8 * 9 + tw[1] * tw[9] * parms[3] * parms[2] ^ 8 * 9 +
        tw[2] * tw[7] * parms[1] ^ 2 * parms[2] ^ 7 * 36 + tw[2] * tw[8] * parms[3] * parms[1] * parms[2] ^ 7 * 72 +
        tw[2] * tw[9] * parms[3] ^ 2 * parms[2] ^ 7 * 36 + tw[3] * tw[6] * parms[1] ^ 3 * parms[2] ^ 6 * 84 +
        tw[3] * tw[7] * parms[3] * parms[1] ^ 2 * parms[2] ^ 6 * 252 + tw[3] * tw[8] * parms[3] ^ 2 * parms[1] * parms[2] ^ 6 * 252 +
        tw[3] * tw[9] * parms[3] ^ 3 * parms[2] ^ 6 * 84 + tw[4] * tw[5] * parms[1] ^ 4 * parms[2] ^ 5 * 126 +
        tw[4] * tw[6] * parms[3] * parms[1] ^ 3 * parms[2] ^ 5 * 504 + tw[4] * tw[7] * parms[3] ^ 2 * parms[1] ^ 2 * parms[2] ^ 5 * 756 +
        tw[4] * tw[8] * parms[3] ^ 3 * parms[1] * parms[2] ^ 5 * 504 + tw[4] * tw[9] * parms[3] ^ 4 * parms[2] ^ 5 * 126 +
        tw[5] * tw[4] * parms[1] ^ 5 * parms[2] ^ 4 * 126 + tw[5] * tw[5] * parms[3] * parms[1] ^ 4 * parms[2] ^ 4 * 630 +
        tw[5] * tw[6] * parms[3] ^ 2 * parms[1] ^ 3 * parms[2] ^ 4 * 1260 + tw[5] * tw[7] * parms[3] ^ 3 * parms[1] ^ 2 * parms[2] ^ 4 * 1260 +
        tw[5] * tw[8] * parms[3] ^ 4 * parms[1] * parms[2] ^ 4 * 630 + tw[5] * tw[9] * parms[3] ^ 5 * parms[2] ^ 4 * 126 +
        tw[6] * tw[3] * parms[1] ^ 6 * parms[2] ^ 3 * 84 + tw[6] * tw[4] * parms[3] * parms[1] ^ 5 * parms[2] ^ 3 * 504 +
        tw[6] * tw[5] * parms[3] ^ 2 * parms[1] ^ 4 * parms[2] ^ 3 * 1260 + tw[6] * tw[6] * parms[3] ^ 3 * parms[1] ^ 3 * parms[2] ^ 3 * 1680 +
        tw[6] * tw[7] * parms[3] ^ 4 * parms[1] ^ 2 * parms[2] ^ 3 * 1260 + tw[6] * tw[8] * parms[3] ^ 5 * parms[1] * parms[2] ^ 3 * 504 +
        tw[6] * tw[9] * parms[3] ^ 6 * parms[2] ^ 3 * 84 + tw[7] * tw[2] * parms[1] ^ 7 * parms[2] ^ 2 * 36 +
        tw[7] * tw[3] * parms[3] * parms[1] ^ 6 * parms[2] ^ 2 * 252 + tw[7] * tw[4] * parms[3] ^ 2 * parms[1] ^ 5 * parms[2] ^ 2 * 756 +
        tw[7] * tw[5] * parms[3] ^ 3 * parms[1] ^ 4 * parms[2] ^ 2 * 1260 + tw[7] * tw[6] * parms[3] ^ 4 * parms[1] ^ 3 * parms[2] ^ 2 * 1260 +
        tw[7] * tw[7] * parms[3] ^ 5 * parms[1] ^ 2 * parms[2] ^ 2 * 756 + tw[7] * tw[8] * parms[3] ^ 6 * parms[1] * parms[2] ^ 2 * 252 +
        tw[7] * tw[9] * parms[3] ^ 7 * parms[2] ^ 2 * 36 + tw[8] * tw[1] * parms[1] ^ 8 * parms[2] * 9 +
        tw[8] * tw[2] * parms[3] * parms[1] ^ 7 * parms[2] * 72 + tw[8] * tw[3] * parms[3] ^ 2 * parms[1] ^ 6 * parms[2] * 252 +
        tw[8] * tw[4] * parms[3] ^ 3 * parms[1] ^ 5 * parms[2] * 504 + tw[8] * tw[5] * parms[3] ^ 4 * parms[1] ^ 4 * parms[2] * 630 +
        tw[8] * tw[6] * parms[3] ^ 5 * parms[1] ^ 3 * parms[2] * 504 + tw[8] * tw[7] * parms[3] ^ 6 * parms[1] ^ 2 * parms[2] * 252 +
        tw[8] * tw[8] * parms[3] ^ 7 * parms[1] * parms[2] * 72 + tw[8] * tw[9] * parms[3] ^ 8 * parms[2] * 9 + tw[9] * n * parms[1] ^ 9 * 1 +
        tw[9] * tw[1] * parms[3] * parms[1] ^ 8 * 9 + tw[9] * tw[2] * parms[3] ^ 2 * parms[1] ^ 7 * 36 + tw[9] * tw[3] * parms[3] ^ 3 * parms[1] ^ 6 * 84 +
        tw[9] * tw[4] * parms[3] ^ 4 * parms[1] ^ 5 * 126 + tw[9] * tw[5] * parms[3] ^ 5 * parms[1] ^ 4 * 126 + tw[9] * tw[6] * parms[3] ^ 6 * parms[1] ^ 3 * 84 +
        tw[9] * tw[7] * parms[3] ^ 7 * parms[1] ^ 2 * 36 + tw[9] * tw[8] * parms[3] ^ 8 * parms[1] * 9 + tw[9] * tw[9] * parms[3] ^ 9

    if (miter >= 10)
      rez[10] <- tw[10] * tw[10] * parms[3] ^ 10 * 1 + tw[10] * n * parms[1] ^ 10 * 1 + tw[10] * tw[1] * parms[3] * parms[1] ^ 9 * 10 +
        tw[10] * tw[2] * parms[3] ^ 2 * parms[1] ^ 8 * 45 + tw[10] * tw[3] * parms[3] ^ 3 * parms[1] ^ 7 * 120 +
        tw[10] * tw[4] * parms[3] ^ 4 * parms[1] ^ 6 * 210 + tw[10] * tw[5] * parms[3] ^ 5 * parms[1] ^ 5 * 252 +
        tw[10] * tw[6] * parms[3] ^ 6 * parms[1] ^ 4 * 210 + tw[10] * tw[7] * parms[3] ^ 7 * parms[1] ^ 3 * 120 +
        tw[10] * tw[8] * parms[3] ^ 8 * parms[1] ^ 2 * 45 + tw[10] * tw[9] * parms[3] ^ 9 * parms[1] ^ 1 * 10 +
        n * tw[10] * parms[2] ^ 10 * 1 + tw[1] * tw[10] * parms[3] ^ 1 * parms[2] ^ 9 * 10 + tw[1] * tw[9] * parms[1] * parms[2] ^ 9 * 10 +
        tw[2] * tw[10] * parms[3] ^ 2 * parms[2] ^ 8 * 45 + tw[2] * tw[8] * parms[1] ^ 2 * parms[2] ^ 8 * 45 +
        tw[2] * tw[9] * parms[3] * parms[1] ^ 1 * parms[2] ^ 8 * 90 + tw[3] * tw[10] * parms[3] ^ 3 * parms[2] ^ 7 * 120 +
        tw[3] * tw[7] * parms[1] ^ 3 * parms[2] ^ 7 * 120 + tw[3] * tw[8] * parms[3] ^ 1 * parms[1] ^ 2 * parms[2] ^ 7 * 360 +
        tw[3] * tw[9] * parms[3] ^ 2 * parms[1] ^ 1 * parms[2] ^ 7 * 360 + tw[4] * tw[10] * parms[3] ^ 4 * parms[2] ^ 6 * 210 +
        tw[4] * tw[6] * parms[1] ^ 4 * parms[2] ^ 6 * 210 + tw[4] * tw[7] * parms[3] ^ 1 * parms[1] ^ 3 * parms[2] ^ 6 * 840 +
        tw[4] * tw[8] * parms[3] ^ 2 * parms[1] ^ 2 * parms[2] ^ 6 * 1260 + tw[4] * tw[9] * parms[3] ^ 3 * parms[1] ^ 1 * parms[2] ^ 6 * 840 +
        tw[5] * tw[10] * parms[3] ^ 5 * parms[2] ^ 5 * 252 + tw[5] * tw[5] * parms[1] ^ 5 * parms[2] ^ 5 * 252 +
        tw[5] * tw[6] * parms[3] ^ 1 * parms[1] ^ 4 * parms[2] ^ 5 * 1260 + tw[5] * tw[7] * parms[3] ^ 2 * parms[1] ^ 3 * parms[2] ^ 5 * 2520 +
        tw[5] * tw[8] * parms[3] ^ 3 * parms[1] ^ 2 * parms[2] ^ 5 * 2520 + tw[5] * tw[9] * parms[3] ^ 4 * parms[1] ^ 1 * parms[2] ^ 5 * 1260 +
        tw[6] * tw[10] * parms[3] ^ 6 * parms[2] ^ 4 * 210 + tw[6] * tw[4] * parms[1] ^ 6 * parms[2] ^ 4 * 210 +
        tw[6] * tw[5] * parms[3] ^ 1 * parms[1] ^ 5 * parms[2] ^ 4 * 1260 + tw[6] * tw[6] * parms[3] ^ 2 * parms[1] ^ 4 * parms[2] ^ 4 * 3150 +
        tw[6] * tw[7] * parms[3] ^ 3 * parms[1] ^ 3 * parms[2] ^ 4 * 4200 + tw[6] * tw[8] * parms[3] ^ 4 * parms[1] ^ 2 * parms[2] ^ 4 * 3150 +
        tw[6] * tw[9] * parms[3] ^ 5 * parms[1] ^ 1 * parms[2] ^ 4 * 1260 + tw[7] * tw[10] * parms[3] ^ 7 * parms[2] ^ 3 * 120 +
        tw[7] * tw[3] * parms[1] ^ 7 * parms[2] ^ 3 * 120 + tw[7] * tw[4] * parms[3] ^ 1 * parms[1] ^ 6 * parms[2] ^ 3 * 840 +
        tw[7] * tw[5] * parms[3] ^ 2 * parms[1] ^ 5 * parms[2] ^ 3 * 2520 + tw[7] * tw[6] * parms[3] ^ 3 * parms[1] ^ 4 * parms[2] ^ 3 * 4200 +
        tw[7] * tw[7] * parms[3] ^ 4 * parms[1] ^ 3 * parms[2] ^ 3 * 4200 + tw[7] * tw[8] * parms[3] ^ 5 * parms[1] ^ 2 * parms[2] ^ 3 * 2520 +
        tw[7] * tw[9] * parms[3] ^ 6 * parms[1] ^ 1 * parms[2] ^ 3 * 840 + tw[8] * tw[10] * parms[3] ^ 8 * parms[2] ^ 2 * 45 +
        tw[8] * tw[2] * parms[1] ^ 8 * parms[2] ^ 2 * 45 + tw[8] * tw[3] * parms[3] ^ 1 * parms[1] ^ 7 * parms[2] ^ 2 * 360 +
        tw[8] * tw[4] * parms[3] ^ 2 * parms[1] ^ 6 * parms[2] ^ 2 * 1260 + tw[8] * tw[5] * parms[3] ^ 3 * parms[1] ^ 5 * parms[2] ^ 2 * 2520 +
        tw[8] * tw[6] * parms[3] ^ 4 * parms[1] ^ 4 * parms[2] ^ 2 * 3150 + tw[8] * tw[7] * parms[3] ^ 5 * parms[1] ^ 3 * parms[2] ^ 2 * 2520 +
        tw[8] * tw[8] * parms[3] ^ 6 * parms[1] ^ 2 * parms[2] ^ 2 * 1260 + tw[8] * tw[9] * parms[3] ^ 7 * parms[1] ^ 1 * parms[2] ^ 2 * 360 +
        tw[9] * tw[10] * parms[3] ^ 9 * parms[2] ^ 1 * 10 + tw[9] * tw[1] * parms[1] ^ 9 * parms[2] ^ 1 * 10 +
        tw[9] * tw[2] * parms[3] ^ 1 * parms[1] ^ 8 * parms[2] ^ 1 * 90 + tw[9] * tw[3] * parms[3] ^ 2 * parms[1] ^ 7 * parms[2] * 360 +
        tw[9] * tw[4] * parms[3] ^ 3 * parms[1] ^ 6 * parms[2] ^ 1 * 840 + tw[9] * tw[5] * parms[3] ^ 4 * parms[1] ^ 5 * parms[2] ^ 1 * 1260 +
        tw[9] * tw[6] * parms[3] ^ 5 * parms[1] ^ 4 * parms[2] ^ 1 * 1260 + tw[9] * tw[7] * parms[3] ^ 6 * parms[1] ^ 3 * parms[2] ^ 1 * 840 +
        tw[9] * tw[8] * parms[3] ^ 7 * parms[1] ^ 2 * parms[2] ^ 1 * 360 + tw[9] * tw[9] * parms[3] ^ 8 * parms[1] * parms[2] * 90

    rez[1:miter] <- rez[1:miter]/(1:miter)
  }

  scalarparm <- sum(p)
  ti <- rez[miter] * miter

  for (miteri in (miter + 1):titer) {
    ti <- scalarparm * ti
    rez[miteri] <- ti/miteri
    if (abs(rez[miteri] - rez[miteri - 1]) < eps)
      break
  }

  return(-sum(rez))
}