# ---- diverse ----------------------------------------------------------------
#' @keywords internal
drop_na <- function(x) {

  if (is.numeric(x)) {
    x[is.na(x)] <- 0
    return(x)
  }

  if (inherits(x, "Matrix")) {
    x@x[is.na(x@x)] <- 0
    return(x)
  }

  stop("Input musst be numeric or Matrix!")
}

#' @keywords internal
factor_in_order <- function(x) {
  factor(x,levels = as.character(unique(x)))
}

#' @keywords internal
sequentialize_index <- function(index_list) {
  len <- unlist(lapply(index_list, length))
  shift <- cumsum(c(0,len))[1:length(index_list)]
  Map("+", index_list, as.list(shift))
}

#' @keywords internal
pprint_df <- function(df, lim = 20, digits = 2) {
  if (nrow(df) <= lim)
    print(df)

  if (nrow(df) > lim) {

    nice_chr <- function(x) as.character(if (is.numeric(x)) round(x,digits) else x)
    h_df <- head(df)
    h_df <- data.frame(lapply(h_df, nice_chr),row.names = row.names(h_df))
    h_df <- rbind(h_df, "---")
    t_df <- tail(df)
    t_df <- data.frame(lapply(t_df, nice_chr),row.names = row.names(t_df))

    pp_df <- rbind(h_df, t_df)
    row.names(pp_df)[7] <- "---"
    print(pp_df)
  }
}

#' @keywords internal
try_coercion <- function(obj, class) {

  obj_as_class <- try(as(obj,class),silent = TRUE)
  assert(!"try-error" %in% class(obj_as_class),
         "Object %s must be coercible to a %s!",
         deparse(substitute(obj,parent.frame())),
         class)

  return(obj_as_class)
}

#' @keywords internal
update_logicals <- function(..., by = "&") {

  logicals <- compact(list(...))
  if (length(logicals) == 0)
    return(NULL)
  return(Reduce(by, logicals))
}

# ---- geographic -------------------------------------------------------------
#' @keywords internal
haversine_distance <- function(lon1, lat1, lon2, lat2) {

  dg2rad <- pi/180
  earth_radius_km <- 6378.137

  diff_lon <- (lon2 - lon1) * (dg2rad/2)
  diff_lat <- (lat2 - lat1) * (dg2rad/2)

  arc_dist <- sin(diff_lat)^2 + cos(lat1 * dg2rad) * cos(lat2 * dg2rad) * sin(diff_lon)^2
  arc_dist[arc_dist > 1] <- 1
  arc_dist <- asin(sqrt(arc_dist)) * (2 * earth_radius_km)

  return(arc_dist)
}

#' @title Convert the `nb-class` from spdep to a sparse Matrix
#' @description
#'    This function genereates a sparse Matrix from a neighborhood matrix.
#'    It bypasses `spdep::nb2mat()` which leads to a dense matrix representation.
#'
#' @param nb A `nb-class` from spdep
#' @return A sparse matrix
#' @importFrom Matrix sparseMatrix
#' @examples
#' nb <- spdep::poly2nb(germany_grid)
#' nb2Mat(nb)
nb2Mat <- function(nb) {

  assert_inherits(nb, "nb")
  n <- length(nb)
  nonzeros <- !unlist(lapply(nb, "identical", 0L))
  rows_index <- seq_along(nb)[nonzeros]
  nb <- nb[nonzeros]

  gi <- unlist(nb)
  gj <- unlist(Map("rep", seq_along(nb), lapply(nb, length)))
  Matrix::sparseMatrix(
    i = unlist(nb),
    j = unlist(Map("rep", seq_along(nb), lapply(nb, length))),
    dims = list(n, n),
    repr = "C")
}

# ---- infix operators --------------------------------------------------------
#' @keywords internal
"%T%" <- function(x, y) {
  if (isTRUE(y)) x else NULL
}


#' @keywords internal
"%||%" <- function(x, y) {
  if (length(x) == 0) y else x
}


#' @keywords internal
"%|!|%" <- function(x, y) {

  if (length(x) == 0)
    return(NULL)

  if (is.function(y))
    return(y(x))

  return(y)
}


# ---- linear algebra ---------------------------------------------------------
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
crossproduct_mat_list <- function(mat_l1, mat_l2 = NULL, force_sym = FALSE) {

  n_mat1 <- n_mat2 <- length(mat_l1)
  dim_mat1 <- dim_mat2 <- Reduce("rbind", lapply(mat_l1, dim))
  names1 <- names2 <- names(mat_l1)

  if (!is.null(mat_l2)) {
    n_mat2 <- length(mat_l2)
    dim_mat2 <- Reduce("rbind", lapply(mat_l2, dim))
    names2 <- names(mat_l2)
  }

  # symmetry: only possible when n1 = n2 + imposed when no m2
  force_sym <- force_sym && (n_mat1 == n_mat2)
  force_sym <- force_sym | is.null(mat_l2)

  dims <- rbind(dim_mat1,dim_mat2)
  # check that dims match + symmetry only works for square case...
  stopifnot(has_equal_elements(dims[,1]), has_equal_elements(dims[,2]))

  result <- matrix(0, nrow = n_mat1 , ncol = n_mat2,
                   dimnames = compact(list(names1, names2)))

  # loop over rows
  for (row in seq_len(n_mat1)) {
    cols_start <- ifelse(force_sym, row, 1)
    cols <- seq(cols_start,n_mat2,1)
    result[row,cols] <- unlist(lapply(
      (mat_l2 %||% mat_l1)[cols], "hadamard_sum", mat_l1[[row]]))
  }

  if (force_sym)
    result <- make_symmetric(result)

  return(result)
}

#' @keywords internal
make_symmetric <- function(mat){
  tri <- lower.tri(mat)
  mat[tri] <- t(mat)[tri]
  mat
}

#' @keywords internal
decorellate_matrix <- function(y, with_x) {
  y - linear_projection(y,with_x)
}

#' @keywords internal
hadamard_sum <- function(x,y = x) {
  sum( x * y )
}

#' @keywords internal
mprod_trace <- function(x, y = x) {
  sum(x * t(y))
}

#' @keywords internal
impose_orthogonality <- function(mat,column_sets){

  # first block does not require orthogonal projection
  Mx_mat <- mat[,column_sets[[1]]]
  for (i in seq_along(column_sets)[-1]) {

    # Bind residual of orthogonal projection
    Px_mat <- linear_projection(mat[,column_sets[[i]]],Mx_mat)
    Mx_mat <- cbind(Mx_mat, mat[,column_sets[[i]]] - Px_mat)
  }
  return(Mx_mat)
}

#' @keywords internal
linear_dim_reduction <- function(mat, var_threshold = 0, n_comp = NULL) {

  svd_mat <- La.svd(mat)
  n_comp <- n_comp %||% sum(svd_mat$d >= var_threshold)

  S_trunc <- diag(svd_mat$d[seq_len(n_comp)])
  U_trunc <- svd_mat$u[,seq_len(n_comp)]
  return(U_trunc %*% S_trunc)
}

#' @keywords internal
linear_projection <- function(y, on_x){
  beta <- solve(crossprod(on_x),crossprod(on_x,y))
  Px_y <- on_x %*% beta
  return(Px_y)
}

# ---- list operations --------------------------------------------------------

#' @author Lukas Dargel
translist <- function(.l) {

  all_inner_names <- Reduce("c", lapply(.l, "names"))
  all_inner_names <- Reduce("c", all_inner_names)
  all_inner_names <- unique(all_inner_names)

  .l_ordered <- lapply(.l, "[", all_inner_names)

  result <- lapply(seq_along(all_inner_names),
                   function(i) lapply(.l_ordered, .subset2, i))

  names(result) <-  all_inner_names
  result <- lapply(result, "compact")

  return(result %||% NULL)
}


#' @keywords internal
compact <- function(.x) {
  Filter(length, .x)
}


#' @keywords internal
flatlist <- function(lst, use.names = TRUE) {
  c2 <- function(...) c(..., use.names = use.names)
  do.call(c2, lapply(lst, function(x) if(is.list(x)) flatlist(x) else list(x)))
}


#' @keywords internal
ulapply <- function(.list, .f, ...,recursive = TRUE, use.names = FALSE) {
  unlist(lapply(.list, match.fun(.f), ...), recursive, use.names)
}

# ---- matrix operations ------------------------------------------------------
#' @keywords internal
rbind_fill_left <- function(..., fill = NA){

  mat_list <- flatlist(list(...))
  mat_cols <- unlist(lapply(mat_list, "ncol"))

  assert(all(diff(mat_cols) <= 0),
         "The number of columns of the matrices must be weakly decreasing!")

  mat_col_missing <- mat_cols[1] - mat_cols
  cbind_fill <- function(mat,nb_cols) cbind(matrix(fill, nrow(mat), nb_cols),mat)
  filled_matrix <- Map(cbind_fill, mat_list, mat_col_missing)
  filled_matrix <- Reduce("rbind", filled_matrix)

  return(filled_matrix)
}

#' @keywords internal
#' @importFrom Matrix bdiag
block_diag <- function(...){
  as.matrix(bdiag(...))
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



# ---- lookups and naming -----------------------------------------------------

#' @keywords internal
named_list <- function(names, init = NULL) {

  named_list <- vector("list", length(names))
  names(named_list) <- names
  named_list[] <- list(init)

  return(named_list %||% NULL)
}


#' @keywords internal
lookup <- function(values, names = as.character(values)) {
  pair_nv <- data.frame(v = as.vector(values), n = names)
  values %|!|% structure(pair_nv$v, names = pair_nv$n)
}


#' @keywords internal
list_lookup <- function(values, names = as.character(values)) {
  as.list(lookup(names = names,values))
}


#' @keywords internal
sort_names <- function(x) {
  x[order(names(x))]
}

#' @keywords internal
prefix_columns <- function(x,prefix){
  `colnames<-`(x, paste0(prefix, colnames(x)))
}

#' @keywords internal
suffix_columns <- function(x,suffix){
  `colnames<-`(x, paste0(colnames(x), suffix))
}

#' @keywords internal
sort_columns <- function(x) {
  x[,sort(colnames(x)), drop = FALSE]
}

# ---- strings ----------------------------------------------------------------
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

#' @keywords internal
sprintfwrap <- function(msg, ... , linebreak = "<br>") {
  wrap_str <- sprintf(strwrap(msg, width = 10000), ...)
  gsub(pattern = linebreak,
       replacement = "\n",
       x = wrap_str,
       fixed = TRUE)
}

