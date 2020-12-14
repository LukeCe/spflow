#' @keywords internal
rbind_fill_left <- function(..., fill = NA){

  mat_list <- list(...) %>% flatlist()
  mat_cols <- lapply(mat_list, "ncol") %>% unlist()

  assert(all(diff(mat_cols) <= 0),
         "The number of columns of the matrices must be weakly decreasing!")

  mat_col_missing <- mat_cols[1] - mat_cols
  cbind_fill <- function(mat,nb_cols){
    cbind(matrix(fill,ncol = nb_cols, nrow = nrow(mat)),mat)
  }

  filled_matrix <- plapply(mat_list, mat_col_missing,.f = "cbind_fill") %>%
    lreduce("rbind")
  return(filled_matrix)
}

#' @keywords internal
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

