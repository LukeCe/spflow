#' @export
cols_drop <- function(obj_2dim,cols_drop){
  cols_select(obj_2dim,index_cols = cols_drop, drop = TRUE)
}

#' @export
cols_keep <- function(obj_2dim,cols_keep){
  cols_select(obj_2dim,index_cols = cols_keep, drop = FALSE)
}

#' @importFrom data.table :=
#' @keywords internal
cols_select <- function(obj_2dim,index_cols, drop = TRUE){

  if (is.null(obj_2dim)) return(NULL)

  assert(is_one_of(obj_2dim, c("data.frame","matrix","data.table")),
         "Function not implemented for objects of class" %p% class(obj_2dim))

  # convert to numerical index
  if (is.character(index_cols))
    index_cols <- which(colnames(obj_2dim) %in% index_cols)
  if (is.logical(index_cols)) {
    assert(length(index_cols) == ncol(obj_2dim),
           "A logical index musst be the same legth as the number of columns!")
    index_cols <- which(index_cols)
  }


  # only use valid column indexes
  possible_indexes <- seq_len(ncol(obj_2dim))
  valid_indexes <- index_cols[index_cols %in% possible_indexes]

  # when using keep invert the logic and use drop instead
  if (drop) index_drop <- valid_indexes
  if (!drop) index_drop <- setdiff(possible_indexes,valid_indexes)
  index_drop <- unique(index_drop)

  # sub setting based on drop_index
  index_drop <- unique(index_drop)
  if (length(index_drop) == 0)   return(obj_2dim)
  if (is(obj_2dim,"data.table")) return(obj_2dim[, (index_drop) := NULL])
  if (is(obj_2dim,"matrix"))     return(obj_2dim[,-index_drop, drop = FALSE])
  if (is(obj_2dim,"data.frame")) return(obj_2dim[-index_drop])
}


#' @export
prefix_columns <- function(obj,prefix){
  colnames(obj) <- prefix %p% colnames(obj)
  obj
}

#' @export
suffix_columns <- function(obj,suffix){
  colnames(obj) <- colnames(obj) %p% suffix
  obj
}


