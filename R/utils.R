# ---- assertions -------------------------------------------------------------
assert <- function(expr, error_msg, warn = FALSE) {
  if (expr) {
    return(invisible(TRUE))
  }
  do.call(ifelse(warn,yes = "warning",no = "stop"),
          list(error_msg = error_msg, call. = FALSE))
}

assert_valid_case <- function(argument,cases) {
  assert(all(argument %in% cases),
         "The what argument can only be a subset of the character vector [%s]!" %>%
           sprintf(deparse(cases)))
}

# ---- classes ----------------------------------------------------------------
coerce_to <- function(obj, class, ...) {

  assert(methods::canCoerce(obj, class),
         "Object [%s] must be coercible to a [%s]!" %>%
           sprintf(deparse(substitute(obj,parent.frame())), class))

  return(as(obj,class,...))

}

savely_as <- function(obj, class, ...) {
  if (is.null(obj))
    return(NULL)

  return(as(obj,class, ...))
}

safely_to_list <- function(obj) {
  if (is.list(obj)) return(obj)
  else return(list(obj))
}

setGenericVerif <- function(x,y) {
  if ( !isGeneric(x))  setGeneric(x,y)
}

try_coercion <- function(obj,class) {

  obj_as_class <- try(savely_as(obj,class),silent = TRUE)

  assert(!"try-error" %in% class(obj_as_class),
         sprintf("Object [%s] must be coercible to a [%s]!",
                 deparse(substitute(obj,parent.frame())),
                 class))

  return(obj_as_class)
}

# ---- combinatorics ----------------------------------------------------------
multinom_coef <- function(...) {

  k_args <- list(...) %>% flatlist()
  t <- Reduce("+",k_args)

  # calculate the denominator
  chose_k_factorial <- k_args %>%
    lapply(factorial) %>%
    Reduce(f = "*", .)

  return(factorial(t)/chose_k_factorial)
}

count_trinom_coefs <- function(n) {
  (n + 1) * (n + 2) / 2
}

sum_trinom_coefs <- function(n) {
  lapply(seq_len(n), count_trinom_coefs) %>%
    Reduce("+",.) %>%
    as.integer()
}

# ---- formulas ---------------------------------------------------------------
combine_formulas <- function(..., intercept = FALSE) {

  labels <- list(...) %>%
    flatlist() %>%
    lapply(extract_terms_labels) %>%
    unlist() %>%
    unique()

  if (length(labels) > 0)
    return(reformulate(labels, intercept = intercept))

  return(~ -1)
}

extract_terms_labels <- function(formula, fake_data = data.frame("." = ".")) {
  labels(terms(formula, data = fake_data))
}

extract_matrix_vars <- function(formula,fake_data) {
  model.matrix(formula,fake_data[0,],
               drop.unused.levels = FALSE) %>% colnames()
}


remove_intercept <- function(formula) {
  reformulate(
    labels(terms(formula, data = data.frame("." = ".")) ),
    intercept = FALSE)
}

remove_vars <- function(.formula,.vars) {
  c(.formula %>% as.character(),
    "-" %p% .vars
    ) %>% paste0(collapse = "") %>%
    as.formula()
}

pull_rhs <- function(formula) {

  if (is_two_sided_formula(formula))
    return(formula[c(1,3)])

  if (is(formula,"formula"))
    return(formula)

  stop("Object is not a formula!")
}

pull_lhs <- function(formula) {

  if (is_two_sided_formula(formula))
    return(formula[c(1,2)])

  stop("Object is not a two sided formula!")
}

to_rhs_formula <- function(variables) {
  formula("~ " %p% paste(unlist(variables), collapse = " + "))
}

fix_contrast_model_matrix <- function(
  formula = ~ .,
  data ) {
  if (is(data,"data.table")) {
    factor_contrasts <-
      data[,sapply(data, is.factor), with = FALSE] %>%
      lapply(contrasts, contrasts = FALSE)
  } else {
    factor_contrasts <-
      data[,sapply(data, is.factor), drop = FALSE] %>%
      lapply(contrasts, contrasts = FALSE)
  }
  model.matrix(formula, data,contrasts.arg = factor_contrasts)
}


# ---- factors ----------------------------------------------------------------
factor_in_order <- function(x) {
  factor(x,levels = as.character(unique(x)))
}

# ---- FP sytle ---------------------------------------------------------------
# collection of functions that help to program in a functional style
# most of these come from or are inspiried by compact purrr files found
# in rlang and feasts packages.
# https://github.com/tidyverts/feasts/blob/master/R/compact-purrr.R
# https://github.com/r-lib/rlang/blob/master/R/compat-purrr.R

compact <- function(.x) {
  Filter(length, .x)
}

flatten <- function(..., use.names = TRUE) {
  c(..., recursive = TRUE, use.names = use.names)
}

flatlist <- function(lst) {
  do.call(c, lapply(lst, function(x) if( is.list(x)) flatlist(x) else list(x)))
}

map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
}

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}

translist <- function(.l) {

  all_inner_names <-
    reduce(lapply(.l, names),c) %>%
    reduce(c) %>%
    unique()

  .l_ordered <- lapply(.l, "[", all_inner_names)

  result <-
    lapply(seq_along(all_inner_names),
           function(i) lapply(.l_ordered, .subset2, i))

  names(result) <-  all_inner_names

  return(lapply(result, compact))
}

# ---- indexation ------------------------------------------------------------------
drop_elements <- function(object, drop_index) {
  object[!drop_index]
}

pull_slot <- function(.slot,.obj) {
  slot(.obj,.slot)
}

pull_slots <- function(.obj, .slots) {
  lapply(.slots, pull_slot, .obj)
}

sequentialize_index <- function(index_list) {
  len <- unlist(lapply(index_list, length))
  shift <- cumsum(c(0,len))[1:length(index_list)]
  mapply("+", index_list, as.list(shift),SIMPLIFY = FALSE)
}


# ---- linear algebra ---------------------------------------------------------
impose_orthogonality <- function(mat,column_sets){

  # first block does not require orthogonal projection
  Mx_mat <- mat[,column_sets[[1]]]
  for (i in seq_along(column_sets)[-1]) {

    # Bind residual of orthogonal projection
    Px_mat <- projec_onto(mat[,column_sets[[i]]],Mx_mat)
    Mx_mat <- cbind(Mx_mat, mat[,column_sets[[i]]] - Px_mat)
  }

  return(Mx_mat)

}

decorellate_matrix <- function(y, with_x) {
  y - projec_onto(y,with_x)
}

projec_onto <- function(y, onto_x){
  beta <- solve(crossprod(onto_x),crossprod(onto_x,y))
  Px_y <- onto_x %*% beta
  return(Px_y)
}

linear_dim_reduction <- function(mat, var_threshold = 1, n_comp = NULL) {

  svd_mat <- La.svd(mat)
  n_comp <- n_comp %||% sum(svd_mat$d >= 1)

  S_trunc <- diag(svd_mat$d[seq_len(n_comp)])
  U_trunc <- svd_mat$u[,seq_len(n_comp)]

  return(U_trunc %*% S_trunc)
}

# ---- lists ------------------------------------------------------------------
collect <- function(names){
  collection <- lapply(names, get, envir = parent.frame(1))
  names(collection) <- names

  return(collection)
}



# ---- math operations --------------------------------------------------------
hadamard_sum <- function(x,y = x) {
  sum( x * y )
}

hadamard_sum_matrix <- function(matrix_list) {

  n_matrixes <- length(matrix_list)
  result <- matrix(0, nrow = n_matrixes , ncol = n_matrixes)

  for (i in seq_len(n_matrixes)) {
    # diagonal elements
    result[i,i] <- sum(matrix_list[[i]] * matrix_list[[i]])

    for (j in seq_len(n_matrixes - i)) {
      # exploit symmetry of Q for off diagonal elements
      c <- i + j
      result[i, c] <- sum(matrix_list[[i]] * matrix_list[[c]])
      result[c, i] <- result[i, c]

    }
  }
  return(result)
}

col_sums <- function(x) {
  matrix(colSums(x),nrow = 1)
}


# ---- naming -----------------------------------------------------------------
named_list <- function(names, init = NULL) {

  named_list <- vector("list", length(names))
  names(named_list) <- names
  named_list[] <- list(init)

  return(named_list)
}

lookup <- function(values, names = as.character(values)) {
  structure(values, names = names)
}

list_lookup <- function(values, names = as.character(values)) {
  as.list(lookup(names = names,names))
}

prefix_columns <- function(obj,prefix){
  colnames(obj) <- prefix %p% colnames(obj)
  obj
}

# ---- strings ----------------------------------------------------------------
'%p%' <- function(x, y) {
  paste0(x,y)
}

concat_by <- function(string = "_", ..., add_spaces = TRUE) {
  if (add_spaces) string <- " " %p% string %p% " "
  paste(..., sep = string)
}

map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
}

pairwise_ids <- function(ids) {
  nb_ids <- length(ids)
  pair_ids <- concat_by("_",rep(ids, nb_ids), rep(ids, each = nb_ids),
                        add_spaces = FALSE)
  return(pair_ids)
}

replace_empty <- function(.x , .replace) {
  sub("^$", .replace, .x )
}

replace_NA_chr <- function(.x , .replace) {
  sub("NA.", .replace, .x )
}

# ---- matrices ---------------------------------------------------------------
block_diag <- function(...){
  as.matrix(Matrix::bdiag(...))
}


stack_cols <- function(mat ,rows = "row", cols = "col", value = "value") {
  cbind(expand.grid(row = factor_in_order(rownames(mat)),
                    col = factor_in_order(colnames(mat))),
        value = as.vector(mat)) %>%
    `names<-`(c(rows,cols,value))
}

rbind_fill0 <- function(mat_a, mat_b) {

  if (is.null(mat_a) || is.null(mat_b) || (ncol(mat_a) == ncol(mat_b)))
    return(rbind(mat_a,mat_b))

  col_diff <- ncol(mat_a) - ncol(mat_b)
  if (col_diff < 0) {
    mat_0 <- matrix(nrow = nrow(mat_a),ncol = abs(col_diff))
    return(rbind(cbind(mat_a,mat_0),mat_b))
  }

  mat_0 <- matrix(nrow = nrow(mat_b),ncol = col_diff)
  return(rbind(mat_a,cbind(mat_0,mat_b)))

}

drop_matrix_columns <- function(matrix, drop_index) {

  if (is.logical(drop_index))
    return(matrix[,!drop_index, drop = FALSE])

  if (is.numeric(drop_index))
    return(matrix[,-drop_index, drop = FALSE])

}


# pipe ------------------------------------------------------------------------
#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Exposing pipe
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%$\%}} for details.
#'
#' @name %$%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %$%
#' @usage lhs \%$\% rhs
NULL


"%||%" <- function(x, y) {
  if (is.null(x)) y else x
}

"%|!|%" <- function(x, y) {
  if (is.null(x)) x else y
}

# ---- primitives -------------------------------------------------------------
has_equal_elements <- function(obj) {
  length(unique(obj)) <= 1
}

has_distinct_elements <- function(obj) {
  length(unique(obj)) == length(obj)
}

is_one_of <- function(.obj, .classes) {
  return(any(class(.obj) %in% .classes))
}

is_one_sided_formula <- function(formula) {
  is(formula,"formula") && (length(formula) == 2)
}

is_single_character <- function(x) {
  is.character(x) && (length(x) == 1L)
}

is_single_logical <- function(x) {
  is.logical(x) && (length(x) == 1L)
}

is_two_sided_formula <- function(formula) {
  is(formula,"formula") && (length(formula) == 3)
}

# ---- print ------------------------------------------------------------------
print_line <- function(n_lines = 60, line_symbol = "-") {

  line <- paste(rep(line_symbol,n_lines),collapse = "")
  return(line)
}

format_percent <- function(x) {
  sprintf(x * 100,fmt = "%1.2f%%")
}


# ---- vectors ----------------------------------------------------------------
insert_after <- function(vec,what,where,replace = TRUE) {
  new_vec <- append(vec,what,where)
  if (replace) new_vec[where] <- NULL
  return(new_vec)
}

insert_after_value <- function(vec,what,value,replace = TRUE) {
  insert_after(vec,what,where = which(vec == value), replace)
}
