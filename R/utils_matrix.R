#' @keywords internal
rbind_fill_left <- function(..., fill = NA){

  mat_list <- flatlist(list(...))
  mat_cols <- unlist(lapply(mat_list, "ncol"))

  assert(all(diff(mat_cols) <= 0),
         "The number of columns of the matrices must be weakly decreasing!")

  mat_col_missing <- mat_cols[1] - mat_cols
  cbind_fill <- function(mat,nb_cols){
    cbind(matrix(fill,ncol = nb_cols, nrow = nrow(mat)),mat)
  }

  filled_matrix <- Map("cbind_fill", mat_list, mat_col_missing)
  filled_matrix <- Reduce("rbind", filled_matrix)
  return(filled_matrix)
}

#' @keywords internal
trace_sequence <- function(W, max_power = 10 ) {

  W_traces <- vector(mode = "list", length = max_power)
  W_pow <- W
  W_traces[[1]] <- sum(diag(W_pow))

  for (pow in seq_len(max_power - 1)) {
    W_pow <- W %*% W_pow
    W_traces[[pow + 1]] <-  sum(diag(W_pow))
  }

  return(unlist(W_traces))
}

#' @keywords internal
#' @importFrom Matrix bdiag
block_diag <- function(...){
  as.matrix(bdiag(...))
}

#' @keywords internal
sort_columns <- function(mat) {
  mat[,sort(colnames(mat))]
}

#' @keywords  internal
stack_columns <- function(mat ,rows = "row", cols = "col", value = "value") {
  vec_form <- cbind(
    expand.grid(col = factor_in_order(colnames(mat)),
                row = factor_in_order(rownames(mat))),
    value = as.vector(mat))
  names(vec_form) <- c(cols,rows,value)
  vec_form
}

#' @keywords internal
colSums2mat <- function(x) {
  matrix(colSums(x),nrow = 1)
}

#' @keywords internal
matrix2binary <- function(mat) {

  if (is(mat, "matrix")) {
    ind <- matrix(1L, nrow = nrow("mat"), ncol = ncol(mat))
    ind[mat == 0] <- 0L
    return(ind)
  }

  if (is(mat, "Matrix")) {
    mat@x <- rep(1L, length(mat@x))
    return(mat)
  }
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
    args <- compact(list(
      i = dest_index, j = orig_index, x = values,
      dims = c(num_dest, num_orig)))
    return(do.call("sparseMatrix", args))
  }

  values <- values %||% 1L
  if (fill_ratio == 1 & assume_ordered)
    return(matrix(values,nrow = num_dest, ncol = num_orig))

  if (fill_ratio <= 1) {
    result_mat <- matrix(0, nrow = num_dest, ncol = num_orig)
    result_mat[cbind(dest_index, orig_index)] <- values
    return(result_mat)
  }

  stop("No result could be obtained.
       Please check that your indexes are integer vectors!")

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

