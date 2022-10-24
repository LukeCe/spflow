#' @keywords internal
lag_flow_matrix <- function(
    Y,
    model,
    OW,
    DW,
    name = "Y",
    M_indicator = NULL) {


  Y_lag0 <- named_list(name, Y)
  if (model == "model_1")
    return(Y_lag0)

  names_rho <- define_spatial_lag_params(model)
  need_d <- any(names_rho %in% c("rho_d","rho_od","rho_odw"))
  need_o <- any(names_rho %in% c("rho_o","rho_od","rho_odw"))
  need_w <- any(names_rho %in% c("rho_w","rho_odw"))
  if (need_d)
    WY <- DW %*% Y
  if (need_o)
    YW <- tcrossprod(Y,OW)
  # The two sided lag may exploit the existing one-sided ones for efficiency
  if (need_w) {
    if (need_d)
      WYW <- tcrossprod(WY, OW)
    if (!need_d & need_o)
      WYW <- DW %*% YW
    if (!need_d & !need_o)
      WYW <-  DW %*% tcrossprod(Y,OW)
  }

  Y_lag1 <- switch(substr(model, 7, 7),
    "9" = list("d"   = WY, "o" = YW, "w" = WYW),
    "8" = list("d"   = WY, "o" = YW, "w" = WYW),
    "7" = list("d"   = WY, "o" = YW),
    "6" = list("odw" = (WY + YW + WYW)/3),
    "5" = list("od"  = (WY + YW)/2),
    "4" = list("w"   = WYW),
    "3" = list("o"   = YW),
    "2" = list("d"   = WY))
  names(Y_lag1) <- paste0(name, ".", names(Y_lag1))

  if (!is.null(M_indicator))
    Y_lag1 <- lapply(Y_lag1, "*", M_indicator)


  return(c(Y_lag0, Y_lag1))
}


#' @keywords internal
filter_flow_matrix <- function(
    Y,
    model,
    OW,
    DW,
    name = "Y",
    M_indicator = NULL,
    rho) {

  if (model == "model_1")
    return(Y)

  Y <- lag_flow_matrix(Y, model,OW, DW,name,M_indicator)
  Y <- Reduce("+", Map("*", c(1, -rho), Y))
  return(Y)
}


#' @importFrom Matrix tcrossprod t
#' @keywords internal
double_lag_matrix <- function(
    M,
    DW,
    OW,
    name = "M",
    key = "M",
    M_indicator = NULL,
    symmetric_lags = FALSE,
    lag_order = 2,
    return_all_lags = FALSE,
    lags_are_instruments = TRUE) {

  if (is.null(M))
    return(NULL)

  if (!is.null(M_indicator))
    M <- M * M_indicator

  ### order 0 lag ###
  lags_0 <- named_list(name, M)
  if (lags_are_instruments)
    attr_inst_status(lags_0[[1]]) <- FALSE

  if ((is.null(OW) & is.null(DW)) | lag_order == 0)
    return(lags_0)

  ### lags of order 1 ###
  lags_1 <- named_list(c("w%s", "%sw", "w%sw"))

  # define controls
  left_lag <- !is.null(DW)
  right_lag <- !is.null(OW)
  double_lag <- left_lag & right_lag
  symmetric_lags <- symmetric_lags & double_lag
  return_all_lags <- return_all_lags | !double_lag

  wM <- (DW %*% M) %T% left_lag
  wMw <- tcrossprod(wM, OW) %T% double_lag
  # skip right lags if not needed
  Mw <- t(wM) %T% (symmetric_lags & right_lag & return_all_lags)
  Mw <- Mw %||% tcrossprod(M, OW) %T% (right_lag & return_all_lags)

  if (!is.null(M_indicator)) {
    account_for_sparsity <- function(x) x * M_indicator
    wMw <- wMw %|!|% account_for_sparsity
    Mw <- Mw %|!|% account_for_sparsity
    wM <- wM %|!|% account_for_sparsity
  }

  lags_1[["w%s"]]  <- wM %T% return_all_lags
  lags_1[["%sw"]]  <- Mw
  lags_1[["w%sw"]]  <- wMw
  names(lags_1) <- paste0(name, ".", sprintf(names(lags_1), key))

  if (lags_are_instruments)
    lags_1 <- lapply(lags_1, "attr_inst_status<-", TRUE)

  if (lag_order == 1)
    return(c(lags_0,lags_1))

  ### lags of order 2 ###
  lags_2 <- c("ww%s", "%sww", "ww%sw", "w%sww", "ww%sww")
  lags_2 <- named_list(lags_2)

  wwMw <- (DW %*% wMw) %T% double_lag
  wwMww <- tcrossprod(wwMw, OW) %T% double_lag
  # skip double left lag and all right lags if not needed
  wwM <- (DW %*% wM) %T% (left_lag & return_all_lags)
  Mww <- t(wwM) %T% (symmetric_lags & right_lag & return_all_lags)
  Mww <- Mww %||% tcrossprod(Mw, OW) %T% (right_lag & return_all_lags)
  wMww <- t(wwMw) %T% (symmetric_lags & double_lag  & return_all_lags)
  wMww <- wMww %||% (DW %*% Mww) %T% (double_lag  & return_all_lags)

  lags_2[["ww%s"]] <- wwM
  lags_2[["%sww"]] <- Mww
  lags_2[["ww%sw"]] <- wwMw %T% return_all_lags
  lags_2[["w%sww"]] <- wMww
  lags_2[["ww%sww"]] <- wwMww
  names(lags_2) <- paste0(name, ".", sprintf(names(lags_2), key))

  if (!is.null(M_indicator))
    lags_2 <- lapply(lags_2, "account_for_sparsity")

  if (lags_are_instruments)
    lags_2 <- lapply(lags_2, "attr_inst_status<-", TRUE)

  return(c(lags_0, lags_1, lags_2))
}


#' @keywords internal
add_lagged_cols <- function(df, W, col_lags) {

  cols2lag <- Filter(function(x) x >= 1, col_lags)
  cols2lag <- cols2lag[names(cols2lag) %in% colnames(df)]
  if (length(cols2lag) == 0 | is.null(W))
    return(df)

  lagged_cols <- Map(
    function(.lag_num, .var) {
      .lags <- vector("list", .lag_num)
      .lags[[1]] <-  W %*% df[,.var]

      for (i in seq_len(.lag_num - 1))
        .lags[[i + 1]] <-  W %*% .lags[[i]]

      .lags <- Reduce("cbind", .lags, init = df[,.var, drop = FALSE])
      colnames(.lags)[-1] <- paste0(.var, ".lag", seq_len(.lag_num))
      as.matrix(.lags)
    },
    .lag_num = cols2lag,
    .var = names(cols2lag))


  non_lags <- !colnames(df) %in% names(cols2lag)
  return(Reduce("cbind", lagged_cols, init = df[,non_lags, drop = FALSE]))
}


#' @keywords internal
spatial_lag <- function(
    M,
    W,
    na_handling = "propagate",
    left = TRUE,
    na_M = is.na(M)) {

  na_lag_options <- c("propagate", "ignore", "reweight")
  assert_valid_option(na_handling, na_lag_options)

  mltp <- if (left) function(w, z) w %*% z else function(w, z) tcrossprod(z, w)
  if (sum(na_M) == 0)
    return(mltp(W, M))

  if (na_handling == "propagate")
    return(mltp(W,M))

  M[na_M] <- 0
  if (na_handling == "ignore")
    return(mltp(W,M))

  assert(left, "Right lags and rweight does not work!")
  rw_W <- rowSums(W)
  rw_W <- rw_W / (rw_W - mltp(W,na_M))
  return(mltp(W, M) * rw_W)
}


#' @keywords internal
spatial_do_lag <- function(
    M,
    OW,
    DW,
    na_M = drop0(is.na(M)),
    lag_keys = c("d","o","w"),
    na_handling = "propagate",
    pair_index = NULL,
    dow_rowsums = na_M %|!|% derive_dow_rowsums(OW,DW,lag_keys,pair_index, ncol(M), nrow(M))) {

  assert_valid_option(lag_keys, c("d","o","w"))
  if (sum(na_M) == 0)
    na_M <- NULL # complete data

  if (!is.null(na_M) & na_handling != "propagate")
    M <- drop_na(M)

  lags <- dow_lags(M = M, OW = OW, DW = DW, na_M = na_M, lag_keys = lag_keys)
  lags <- subset_pair_index(lags, pair_index,M = M)
  if (na_handling == "reweight" & !is.null(na_M)) {
    lagweights_na <- dow_lags(M = na_M, OW = OW, DW = DW, na_M = NULL, lag_keys = lag_keys)
    lagweights_na <- subset_pair_index(lagweights_na,pair_index, TRUE)

    lagweights_na <- Map(
      function(.lw_na, .lw_complet) .lw_complet - .lw_na,
      .lw_na = lagweights_na,
      .lw_complet = dow_rowsums)

    for (i in seq_along(lags)) {
      lags[[i]]@x <- if (identical(dow_rowsums[[i]], 1)) {
        lags[[i]]@x / lagweights_na[[i]]
        } else {
        lags[[i]]@x * dow_rowsums[[i]] / lagweights_na[[i]]
        }
    }
  }
  return(lags)
}


#' @keywords internal
spatial_do_lag2 <- function(
    M,
    OW,
    DW,
    na_M = drop0(is.na(M)),
    lag_keys2 = c("d","o","w", "dd","do","dw","od","oo","ow","wd","wo","ww"),
    na_handling = "propagate",
    pair_index = NULL,
    dow_rowsums) {

  lag_options <- c("d","o","w", "dd","do","dw","od","oo","ow","wd","wo","ww")
  assert_valid_option(lag_keys2, lag_options)
  if (is.null(pair_index))
    lag_keys2 <- setdiff(lag_keys2, c("od", "wd", "wo"))

  # order 1 lags
  declared_lag1 <- lag_keys2[nchar(lag_keys2) == 1] %||% NULL
  undeclared_lag1 <- unique(substr(lag_keys2[nchar(lag_keys2) == 2],1,1))
  undeclared_lag1 <- setdiff(undeclared_lag1, declared_lag1)
  lag1_keys <- c(declared_lag1,undeclared_lag1)

  if (missing(dow_rowsums))
    dow_rowsums <- na_M %|!|% derive_dow_rowsums(OW,DW,lag1_keys,pair_index, ncol(M), nrow(M))
  lags1 <- spatial_do_lag(M, OW, DW, na_M, lag1_keys, na_handling, pair_index, dow_rowsums)

  # order 2 lags
  lag_keys2 <- lag_keys2[nchar(lag_keys2) == 2]
  get_l2 <- function(k) {

    k2 <- substr(Filter(function(x) substr(x,1,1) == k, lag_keys2),2,2)
    if (length(k2) == 0)
      return(NULL)

    l2 <- spatial_do_lag(
      lags1[[k]], OW = OW, DW = DW,
      lag_keys = k2, na_handling = na_handling,
      pair_index = pair_index, dow_rowsums = dow_rowsums)
    names(l2) <- paste0(k,k2)
    l2
  }

  lags2d <- get_l2("d")
  lags2o <- get_l2("o")
  lags2w <- get_l2("w")
  return(c(lags1[declared_lag1], lags2d, lags2o, lags2w))
}


#' @keywords
derive_dow_rowsums <- function(
    OW,
    DW,
    lag_keys,
    pair_index = NULL,
    n_o = nrow(OW),
    n_d = nrow(DW)) {

  if (is.null(lag_keys))
    return(NULL) # no lags

  assert_valid_option(lag_keys, c("d","o","w"))

  l <- function(.k) .k %in% lag_keys
  if (is.null(pair_index) || (length(pair_index) == n_d*n_o)) {
    # cartesian case
    iOW <- rowSums(OW)
    iDW <- rowSums(DW)

    # assume row-normalization then verify
    rw <- named_list(lag_keys, 1)
    D_nonstochastic <- any(iDW != 1)
    O_nonstochastic <- any(iOW != 1)

    if (D_nonstochastic)
      rw[["d"]] <- `dim<-`(outer(iDW, rep(1,n_o)),NULL) %T% l("d")
    if (O_nonstochastic)
      rw[["o"]] <- `dim<-`(outer(rep(1,n_d), iOW),NULL) %T% l("o")
    if (O_nonstochastic | D_nonstochastic)
      rw[["w"]] <- `dim<-`(outer(iDW,iOW),NULL) %T% l("w")

  } else {
    # non cartesian
    I_OD <- Matrix::Matrix(0L, nrow = n_d, ncol = n_o)
    I_OD[pair_index] <- 1L

    rw <- dow_lags(I_OD, OW, DW, NULL, lag_keys)
    rw <- subset_pair_index(rw, pair_index =  pair_index, as_vec = TRUE)
  }

  return(rw)
}

#' @keywords internal
dow_lags <- function(
    M,
    OW,
    DW,
    na_M,
    lag_keys) {
  l <- function(l) l %in% lag_keys
  lg <- named_list(lag_keys)
  lg[["d"]] <- spatial_lag(M,DW, na_M = na_M) %T% l("d")
  lg[["o"]] <- spatial_lag(M,OW, na_M = na_M, left = FALSE) %T% l("o")
  # if possible exploit previous lags for two sided version
  if (l("w") & l("d"))
    lg[["w"]] <- spatial_lag(lg[["d"]], OW, na_M = na_M, left = FALSE)
  if (l("w") & !l("d") & l("o"))
    lg[["w"]] <- spatial_lag(lg[["o"]], DW, na_M = na_M)
  if (l("w") & !l("d") & !l("o"))
    lg[["w"]] <- spatial_lag(spatial_lag(M, DW, na_M = na_M), OW, na_M = na_M, left = FALSE)
  return(lg)
}

#' @keywords internal
subset_pair_index <- function(lags, pair_index, as_vec = FALSE, M) {

  if (is.null(lags))
    return(NULL)

  assert(is.list(lags) & all(sapply(lags, inherits, "Matrix")))


  if (is.null(pair_index) & !as_vec)
    return(lags)

  if (is.null(pair_index) & as_vec)
    return(lapply(lags, "as.vector"))


  .lg <- lapply(lags, "[", pair_index)
  if (as_vec)
    return(.lg)

  assert_inherits(M, "Matrix")
  .lg <- lapply(.lg, function(.l) {
    M@x <- .l
    M})

  return(.lg)
  }


#' @keywords internal
orthoginolize_instruments <- function(mat) {

  inst_index <- attr_inst_status(mat)
  no_instruments <- none(inst_index)
  if (no_instruments)
    return(mat)

  vars <- mat[,!inst_index, drop = FALSE]
  inst_orth <- decorellate_matrix(mat[,inst_index, drop = FALSE],
                                  cbind(1,vars))
  inst_orth <- linear_dim_reduction(inst_orth, var_threshold = 1e-4)

  new_matr <- cbind(vars,inst_orth)
  new_inst_order <- Map("rep", c(FALSE,TRUE), c(ncol(vars),ncol(inst_orth)))
  attr_inst_status(new_matr) <- unlist(new_inst_order)
  return(new_matr)
}


# ---- helpers ----------------------------------------------------------------
#' @keywords internal
define_spatial_lag_params <- function(model) {
  names_rho <- switch(substr(model, 7, 7),
                      "9" = c("rho_d", "rho_o", "rho_w"),
                      "8" = c("rho_d", "rho_o", "rho_w"),
                      "7" = c("rho_d", "rho_o"),
                      "6" = "rho_odw",
                      "5" = "rho_od",
                      "4" = "rho_w",
                      "3" = "rho_o",
                      "2" = "rho_d",
                      "1" = NULL)

  return(names_rho)
}


#' @keywords internal
`attr_inst_status<-` <- function(x, value) {
  attr(x, "is_instrument_var") <- value
  x
}

#' @keywords internal
attr_inst_status <- function(x) {
  attr(x, "is_instrument_var")
}

