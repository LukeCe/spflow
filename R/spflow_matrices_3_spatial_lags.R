#' @keywords internal
lag_spflow_matrices <- function(
    variable_usage,
    spflow_matrices,
    spflow_indicators,
    spflow_neighborhood,
    model,
    decorrelate_instruments,
    reduce_pair_instruments) {


  dim_nbs <- c("DW" = nlevels(spflow_indicators[[1]]), "OW" = nlevels(spflow_indicators[[2]]))
  formula_names <- c("D_" = "DEST_","O_" = "ORIG_", "I_" = "INTRA_")
  node_matrices <- intersect(names(formula_names), names(spflow_matrices))

  claculate_node_lags <- function(.key) {

    X_lag <- spflow_matrices[[.key]]
    if (is.null(X_lag))
      return(NULL)

    .Wkey <- c("I_" = "OW", "D_" = "DW", "O_" = "OW")[.key]
    WX <- spflow_neighborhood[[.Wkey]]
    n_obs <- dim_nbs[.Wkey]
    if (nrow(X_lag) < n_obs) {
      obs_index <- as.integer(row.names(X_lag))
      WX <- WX[obs_index, obs_index, drop = FALSE]
    }

    lag_num <- variable_usage[[.key]]
    lag_num <- lookup(as.integer(lag_num[["num_lags"]]),row.names(lag_num))
    X_lag <- add_lagged_cols(X_lag, WX, lag_num)

    is_inst <- unlist(variable_usage[[.key]][["inst_attr"]])
    X_lag <- cbind(
      X_lag[,!is_inst, drop = FALSE],
      X_lag[, is_inst, drop = FALSE])

    attr_inst_status(X_lag) <- sort(is_inst)
    colnames(X_lag) <- paste0(formula_names[.key], colnames(X_lag))
    if (decorrelate_instruments)
      X_lag <- orthoginolize_instruments(X_lag)

    if (nrow(X_lag) == n_obs)
      return(X_lag)

    X_lag_a <- matrix(0, n_obs, ncol(X_lag))
    X_lag_a[obs_index,] <- X_lag
    colnames(X_lag_a) <- colnames(X_lag)
    attr_inst_status(X_lag_a) <- attr_inst_status(X_lag)
    return(X_lag_a)
  }
  spflow_matrices[node_matrices] <-
    lapply(lookup(node_matrices), claculate_node_lags)


  Y_indicator <- spflow_indicators2mat(spflow_indicators, do_filter = "HAS_Y")
  G_indicator <- spflow_indicators2mat(spflow_indicators, do_filter = "HAS_SIG")
  G_lags <- variable_usage[["G_"]]
  spflow_matrices[["G_"]] <-
    Reduce("c", lapply(row.names(G_lags),  function(.var) {
      double_lag_matrix(
        M = spflow_matrices[["G_"]][[.var]],
        DW = spflow_neighborhood[["DW"]],
        OW = spflow_neighborhood[["OW"]],
        name = .var,
        key = "G",
        M_indicator = Y_indicator %||% G_indicator,
        lag_order = G_lags[.var,"num_lags"],
        return_all_lags = !reduce_pair_instruments,
        lags_are_instruments = TRUE
      )}))


  spflow_matrices[["Y_"]] <-
    Reduce("c", Map(
      f = "lag_flow_matrix",
      Y = spflow_matrices[["Y_"]],
      name = names(spflow_matrices[["Y_"]]),
      MoreArgs = c(
        spflow_neighborhood,
        list("M_indicator" = Y_indicator, "model" = model))))

  return(spflow_matrices)
}

# ---- lag computations -------------------------------------------------------

#' @keywords internal
lag_flow_matrix <- function(
    Y,
    model,
    OW,
    DW,
    name = "Y",
    M_indicator = NULL) {


  if (model == "model_1")
    return(named_list(name, Y))

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

  Y_lags <- switch(substr(model, 7, 7),   # (8.15) in LeSage book
                   "9" = list(" " = Y, ".d"   = WY, ".o" = YW, ".w" = WYW),
                   "8" = list(" " = Y, ".d"   = WY, ".o" = YW, ".w" = WYW),
                   "7" = list(" " = Y, ".d"   = WY, ".o" = YW),
                   "6" = list(" " = Y, ".odw" = (WY + YW + WYW)/3),
                   "5" = list(" " = Y, ".od"  = (WY + YW)/2),
                   "4" = list(" " = Y, ".w"   = WYW),
                   "3" = list(" " = Y, ".o"   = YW),
                   "2" = list(" " = Y, ".d"   = WY),
                   "1" = list(" " = Y))
  names(Y_lags) <- strwrap(paste0(name, names(Y_lags)))
  if (!is.null(M_indicator) & length(Y_lags) > 1)
    Y_lags[-1] <- lapply(Y_lags[-1], "*", M_indicator)


  return(Y_lags)
}

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


#' @importFrom Matrix tcrossprod t
#' @keywords internal
double_lag_matrix <- function(
    M,
    DW,
    OW,
    name = "G",
    key = "G",
    M_indicator = NULL,
    symmetric_lags = FALSE,
    lag_order = 2,
    return_all_lags = FALSE,
    lags_are_instruments = TRUE) {

  if (is.null(M))
    return(NULL)

  ### order 1 lag ###
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

  if (!is.null(M_indicator))
    M <- M * M_indicator
  wM <- (DW %*% M) %T% left_lag
  wMw <- tcrossprod(wM, OW) %T% double_lag
  # skip right lags if not needed
  Mw <- t(wM) %T% (symmetric_lags & right_lag & return_all_lags)
  Mw <- Mw %||% tcrossprod(M, OW) %T% (right_lag & return_all_lags)

  if (!is.null(M_indicator)) {
    account_for_sparsity <- function(x) x * M_indicator
    wMw <- wMw %|!|% account_for_sparsity
    Mw <- Mw %|!|% account_for_sparsity

    # skip sparsity if not returned
    if (lag_order >= 1)
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

  return(c(lags_0,lags_1,lags_2))
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

# ---- instruments ------------------------------------------------------------
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

#' @keywords internal
`attr_inst_status<-` <- function(x, value) {
  attr(x, "is_instrument_var") <- value
  x
}

#' @keywords internal
attr_inst_status <- function(x) {
  attr(x, "is_instrument_var")
}

