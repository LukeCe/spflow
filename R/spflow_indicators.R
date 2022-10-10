#' @keywords internal
spflow_indicators2obs <- function(spflow_indicators) {

  n_o <- nlevels(spflow_indicators[[2]])
  n_d <- nlevels(spflow_indicators[[1]])
  N <- N_pop <- N_sample <- nrow(spflow_indicators)

  in_pop <- spflow_indicators[["HAS_ZZ"]]
  if (!is.null(in_pop))
    N_pop <- sum(in_pop)

  in_sample <- update_logicals(
    spflow_indicators[["HAS_ZZ"]],
    spflow_indicators[["HAS_ZY"]],
    spflow_indicators[["WEIGHT"]] > 0)
  if (!is.null(in_sample))
    N_sample <- sum(in_sample)

  obs <- list(
    N_orig = n_o,
    N_dest = n_d,
    N_sample = N_sample,
    N_pop = N_pop,
    N_pair = N,
    N_cart = n_o * n_d)




}


#' @keywords internal
spflow_indicators2pairindex <- function(spflow_indicators, do_filter = NULL) {

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

  if (is.character(do_filter))
    do_filter <- do_keys[[do_filter]]
  if (!is.null(do_filter))
    do_keys <- do_keys[do_filter, (1:2),drop = FALSE]
  if (is.character(do_values))
    do_values <- do_keys[[do_values]]

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
spflow_indicators2wtmat <- function(do_keys, as_binary = FALSE) {
  do_filter <- update_logicals(do_keys[["HAS_ZZ"]], do_keys[["HAS_ZY"]], do_keys[["WEIGHT"]] > 0)
  spflow_indicators2mat(do_keys, do_filter, "WEIGHT" %T% (!as_binary))
}


#' @keywords internal
spflow_indicators2format <-  function(do_keys_val, return_type = "V", do_filter = NULL) {

  assert_valid_option(return_type, c("V","M", "OD"))

  if (return_type == "OD")
    return(do_keys_val)

  do_keys_val <- do_keys_val[do_filter %||% TRUE,]
  if (return_type == "V")
    return(do_keys_val[[3]])

  if (return_type == "M")
    return(matrix_format_d_o(
      values = do_keys_val[[3]],
      dest_index = as.numeric(do_keys_val[[1]]),
      orig_index = as.numeric(do_keys_val[[2]]),
      num_dest = nlevels(do_keys_val[[1]]),
      num_orig = nlevels(do_keys_val[[2]])))

}


#' @keywords internal
spflow_mat2format <- function(mat, do_keys, return_type = "M", name = "OD_VAR") {

  assert_valid_option(return_type, c("V","M", "OD", "m"))

  if (return_type == "M")
    return(mat)

  if (return_type == "m")
    return(as.matrix(mat))

  is_cartesian <- nrow(do_keys) == length(mat)
  if (is_cartesian)
    vec <- as.vector(mat)

  if (!is_cartesian)
    vec <- mat[spflow_indicators2pairindex(do_keys)]

  if (return_type == "V")
    return(vec)

  do_keys[[name]] <- vec
  if (return_type == "OD")
    return(do_keys)
}


#' @importFrom Matrix sparseMatrix
#' @keywords internal
matrix_format_d_o <- function(
    values = NULL,
    dest_index,
    orig_index,
    num_dest = max(dest_index),
    num_orig = max(orig_index),
    assume_ordered = TRUE) {

  Ns <- length(dest_index)
  N <- num_dest * num_orig
  fill_ratio <- Ns/N

  assert(length(orig_index) == Ns,"
         The length of the origin and destination index musst be identical!")
  assert(any(length(values) == c(0,1,Ns)),"
         The length of the values musst match those of the indexes!")
  assert(fill_ratio <= 1, "
         The number of supplied values is to large
         for the dimension of the matrix representation!")

  if (fill_ratio < .5) {
    args <- named_list(c("i","j","x","dims"))
    args[["i"]] <- dest_index
    args[["j"]] <- orig_index
    args[["x"]] <- values
    args[["dims"]] <- c(num_dest, num_orig)
    return(do.call("sparseMatrix", args))
  }

  values <- values %||% 1L
  if (fill_ratio == 1 & assume_ordered)
    return(matrix(values,nrow = num_dest, ncol = num_orig))

  if (fill_ratio <= 1) {
    result_mat <- matrix(0, nrow = num_dest, ncol = num_orig)
    result_mat[dest_index + num_dest * (orig_index - 1)] <- values
    return(result_mat)
  }

  stop("Failed to generate matrix format. Make sure that the indexes are integers!")
}

#' @keywords internal
matrix_format_o_d <- function(
    values,
    dest_index,
    orig_index,
    num_dest = max(dest_index),
    num_orig = max(orig_index),
    assume_ordered = TRUE) {

  t(matrix_format_d_o(
    values = values,
    dest_index = dest_index,
    orig_index = orig_index,
    num_dest = num_orig,
    num_orig = num_dest,
    assume_ordered = assume_ordered))

}
