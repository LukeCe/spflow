#' @keywords internal
spflow_indicators2obs <- function(spflow_indicators) {

  n_o <- nlevels(spflow_indicators[[2]])
  n_d <- nlevels(spflow_indicators[[1]])
  N <- nrow(spflow_indicators)
  filter_len <- function(.f) if (is.null(spflow_indicators[[.f]])) N else sum(spflow_indicators[[.f]])

  list(
    N_orig = n_o,
    N_dest = n_d,
    N_cart = n_o * n_d,
    N_pair = N,
    N_fit = filter_len("HAS_Y"),
    N_pred = filter_len("HAS_SIG"))
}

#' @keywords internal
spflow_indicators2pairindex <- function(spflow_indicators, do_filter) {

  if (is.character(do_filter))
    do_filter <- spflow_indicators[[do_filter]]
  if (!is.null(do_filter))
    spflow_indicators <- spflow_indicators[do_filter,,drop = FALSE]

  n_d <- nlevels(spflow_indicators[[1]])
  pair_index <- as.numeric(spflow_indicators[[1]]) + (as.numeric(spflow_indicators[[2]]) - 1) * n_d
  return(pair_index)
}


#' @keywords internal
spflow_indicators2matlist <- function(do_keyed_data) {

  .do_keys <- do_keyed_data[,(1:2), drop = FALSE]
  .data <- do_keyed_data[,-(1:2), drop = FALSE]
  if (ncol(.data) == 0)
    return(NULL)

  n_o <- nlevels(.do_keys[[2]])
  n_d <- nlevels(.do_keys[[1]])
  is_cartesian <- nrow(.do_keys) == n_o * n_d
  if (is_cartesian)
    return(lapply(.data, matrix, nrow = n_d, ncol = n_o))

  mform <- function(vec) matrix_format_d_o(
    values = vec,
    dest_index = as.numeric(.do_keys[[1]]),
    orig_index = as.numeric(.do_keys[[2]]),
    num_dest = n_d,
    num_orig = n_o)
  return(lapply(.data, mform))
}

#' @keywords internal
spflow_indicators2mat <- function(do_keys, do_filter = NULL, do_values = NULL) {

  if (is.character(do_values))
    do_values <- do_keys[[do_values]]
  if (is.character(do_filter))
    do_filter <- do_keys[[do_filter]]
  if (!is.null(do_filter))
    do_keys <- do_keys[do_filter, (1:2),drop = FALSE]

  n_o <- nlevels(do_keys[[2]])
  n_d <- nlevels(do_keys[[1]])
  is_cartesian <- nrow(do_keys) == n_o * n_d
  if (is_cartesian & is.null(do_values))
    return(NULL)

  return(matrix_format_d_o(
    values = do_values,
    dest_index = as.numeric(do_keys[[1]]),
    orig_index = as.numeric(do_keys[[2]]),
    num_dest = n_d,
    num_orig = n_o))
}

#' @keywords internal
spflow_indicators2format <-  function(do_keys_val, type = "V", do_filter = NULL) {

  if (type == "OD")
    return(do_keys_val)

  do_keys_val <- do_keys_val[do_filter %||% TRUE,]
  if (type == "V")
    return(do_keys_val[[3]])

  if (type == "M")
    return(matrix_format_d_o(
      values = do_keys_val[[3]],
      dest_index = as.numeric(do_keys_val[[1]]),
      orig_index = as.numeric(do_keys_val[[2]]),
      num_dest = nlevels(do_keys_val[[1]]),
      num_orig = nlevels(do_keys_val[[2]])))

  stop("Argument format musst be V, OD or M!")
}
