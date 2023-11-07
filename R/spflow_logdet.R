#' @title A function to compute the Jacobean term in the spatial econometric interaction model
#' @description These are internal functions called within the estimation procedure
#' @details
#' The Jacobean term of the model corresponds to the log-determinant of the
#' spatial filter matrix, which is costly to compute.
#' To reduce the computational burden of the MLE or MCMC estimators the package
#' uses a power-series approximation of this term.
#' This approximation was first proposed by \insertCite{Martin1992;textual}{spflow},
#' has been adapted to interaction models by \insertCite{LeSage2008;textual}{spflow} and
#' \insertCite{Dargel2023;textual}{spflow} extend the approximation to the
#' non-cartesian and rectangular cases, where the OD-matrix can be sparse and where the
#' list of origins may be different from the list of destinations.
#'
#' By using this approximation we can avoid to directly calculate the determinant term.
#' Moreover, it is possible to factor out the autoregression parameters from
#' all remaining terms, which means that we do not need to repeat the most
#' costly computations.
#'
#' @author Lukas Dargel
#' @return A function that takes the autoregression parameters as an argument and returns the log-determinant value
#' @keywords internal
#' @references \insertAllCited{}
derive_logdet_calculator <- function(
  OW,
  DW,
  n_o,
  n_d,
  model,
  approx_order,
  is_cartesian,
  M_indicator) {

  if (is.null(M_indicator)) {

    approx_logdet <- derive_approxldet_cartesian(
      OW = OW,
      DW = DW,
      n_o = n_o,
      n_d = n_d,
      model = model,
      approx_order = approx_order)
    return(approx_logdet)
  }

  if (approx_order == 2) {
    approx_logdet <- derive_approxldet_noncartesian2(
      OW = OW,
      DW = DW,
      M_indicator = M_indicator,
      n_o = n_o,
      n_d = n_d,
      model = model)
    return(approx_logdet)
  }


  W_flow <- expand_spflow_neighborhood(
    OW = OW,
    DW = DW,
    n_o = n_o,
    n_d = n_d,
    M_indicator = M_indicator,
    model = model)

  approx_logdet <- derive_approxldet_noncartesian(
    Wd = W_flow[["Wd"]],
    Wo = W_flow[["Wo"]],
    Ww = W_flow[["Ww"]],
    approx_order = approx_order,
    model = model)
  return(approx_logdet)
}



#' @rdname derive_logdet_calculator
#' @usage derive_approxldet_cartesian(OW, DW, n_o, n_d, model, approx_order)
#' @keywords internal
derive_approxldet_cartesian <- function(
    OW = NULL,
    DW = NULL,
    n_o,
    n_d,
    model,
    approx_order) {

  if (approx_order == 2)
    return(derive_approxldet_cartesian2(OW = OW, DW = DW, n_o = n_o, n_d = n_d, model = model))


  # the first trace is always zero
  powers_2p <- seq(2, approx_order)
  tseq <- function(W) trace_sequence(W, approx_order)[-1]

  if (model == "model_2")
      tWx_traces <- n_o * tseq(DW)

  if (model == "model_3")
    tWx_traces <- n_d * tseq(OW)

  if (model == "model_4")
    tWx_traces <- tseq(DW) * tseq(OW)

  if (model %in% paste0("model_", c(2,3,4))) {
    tWx_traces <-  tWx_traces / powers_2p
    logdet_calculator_234 <- function(rho) {
      rho_xt <- rho^powers_2p
      logdet_val <- -as.numeric(sum(rho_xt * tWx_traces))
      return(logdet_val)
    }
    return(logdet_calculator_234)
  }

  if (model == "model_8") {
    tWd_traces <- n_o * tseq(DW) / powers_2p
    tWo_traces <- n_d * tseq(OW) / powers_2p

    logdet_calculator_8 <- function(rho) {
      rho_dt <- rho[1]^powers_2p
      rho_ot <- rho[2]^powers_2p

      logdet_val_d <- -as.numeric(sum(rho_dt * tWd_traces))
      logdet_val_o <- -as.numeric(sum(rho_ot * tWo_traces))
      return(logdet_val_d + logdet_val_o)
    }
    return(logdet_calculator_8)
  }

  # for models 5,6,7 and 9 we use a multinomial expansion
  # to compute the trace values
  tracevals <- tracevals2params_cartesian(
    OW = OW,
    DW = DW,
    n_o = n_o,
    n_d = n_d,
    model = model,
    approx_order = approx_order)
  logdet_calculator_5679 <- tracevals2approxldet(tracevals)
  return(logdet_calculator_5679)
}


#' @rdname derive_logdet_calculator
#' @usage derive_approxldet_cartesian2(OW, DW, n_o, n_d, model) # order = 2
#' @keywords internal
derive_approxldet_cartesian2 <- function(OW = NULL, DW = NULL, n_o, n_d, model) {

  model_num <- substr(model,7,7)

  trace_dd <- if (model_num %in% c(2,5:9)) sum(DW * t(DW)) * n_o

  trace_oo <- if (model_num %in% c(3,5:9)) sum(OW * t(OW)) * n_d

  trace_ww <- if (model_num %in% c(3,5,6,8,9)) sum(OW * t(OW)) * sum(DW * t(DW))

  tracevals <- c(trace_dd,trace_oo,trace_ww) / 2
  logdet_calculator_123456789 <- function(rho) {
    logdet_val <- as.numeric(sum(rho^2 * tracevals))
    return(-logdet_val)
  }
  return(logdet_calculator_123456789)
}

#' @rdname derive_logdet_calculator
#' @keywords internal
derive_approxldet_noncartesian <- function(
    Wd,
    Wo,
    Ww,
    approx_order,
    model) {


  model_num <- substr(model, 7, 7)
  model_with_single_weight_matrix <- model_num %in% c(2,3,4,5,6)

  if (model_with_single_weight_matrix) {
    # the first trace is always zero
    powers_2p <- seq(2, approx_order)
    tseq <- function(W) (trace_sequence(W, approx_order)[-1] / powers_2p)

    if (model == "model_2")
      tWx_traces <- tseq(Wd)

    if (model == "model_3")
      tWx_traces <- tseq(Wo)

    if (model == "model_4")
      tWx_traces <- tseq(Ww)

    if (model == "model_5")
      tWx_traces <- tseq((Wd + Wo) / 2)

    if (model == "model_6")
      tWx_traces <- tseq((Wd + Wo + Ww) / 3)

    logdet_calculator_23456 <- function(rho) {
      rho_xt <- sum(rho)^powers_2p
      logdet_val <- -as.numeric(sum(rho_xt * tWx_traces))
      return(logdet_val)
    }

    return(logdet_calculator_23456)
  }

  assert(approx_order <= 4, warn = TRUE,
         "For the non-cartesian models with multiple autoregression parameters
         we only use the log-determinant approximation of order four or lower.
         The estimation proceeds with order four.")

  trace_values <- tracevals2params_noncartesian(
    Wd = Wd, Wo = Wo, Ww = Ww,
    model =  model, approx_order = approx_order)
  logdet_calculator_789 <- tracevals2approxldet(trace_values)
  return(logdet_calculator_789)
}

#' @rdname derive_logdet_calculator
#' @usage derive_approxldet_noncartesian2(OW, DW, M_indicator, n_o, n_d, model) # order = 2
#' @keywords internal
derive_approxldet_noncartesian2 <- function(OW = NULL, DW = NULL, M_indicator, n_o, n_d, model) {

  model_num <- substr(model,7,7)

  trace_dd <- if (model_num %in% c(2,5:9))
    sum(M_indicator * ((DW * t(DW)) %*% M_indicator))

  trace_oo <- if (model_num %in% c(3,5:9))
    sum(M_indicator * (M_indicator %*% ((OW * t(OW)))))

  trace_ww <- if (model_num %in% c(3,5,6,8,9))
    sum(M_indicator * ((DW * t(DW)) %*% M_indicator %*% ((OW * t(OW)))))

  tracevals <- c(trace_dd,trace_oo,trace_ww) / 2
  logdet_calculator_123456789 <- function(rho) {
    logdet_val <- as.numeric(sum(rho^2 * tracevals))
    return(-logdet_val)
  }
  return(logdet_calculator_123456789)
}

#' @rdname derive_logdet_calculator
#' @keywords internal
tracevals2params_cartesian <- function(OW, DW, n_o, n_d, model, approx_order) {

  # model 5,6,7 and 9 require a multinomial expansions
  # the parameters in 5 and 6 are those of 7 and 9
  # (just with equality constraint)
  model_params <- switch(model, "model_5" = "model_7", "model_6" = "model_9", model)
  model_params <- define_spatial_lag_params(model_params)
  power_lookup <- multinom_table(approx_order, model_params)

  OW_values <- DW_values <- lookup(1, seq(0, approx_order))
  OW_values[-1] <- trace_sequence(OW, approx_order)
  DW_values[-1] <- trace_sequence(DW, approx_order)
  OW_powers <- power_lookup$rho_o
  DW_powers <- power_lookup$rho_d
  if ("rho_w" %in%  model_params) {
    OW_powers <- OW_powers + power_lookup$rho_w
    DW_powers <- DW_powers + power_lookup$rho_w
  }

  # each row is one term in the multinomial sum
  tracevals <- data.frame(
    OW_val = OW_values[as.character(OW_powers)],
    DW_val = DW_values[as.character(DW_powers)],
    n_o = n_o ^ (power_lookup$rho_d == power_lookup$POWER_ORDER),
    n_d = n_d ^ (power_lookup$rho_o == power_lookup$POWER_ORDER),
    coef = power_lookup$COEF_MULTINOM,
    t_inv = 1/power_lookup$POWER_ORDER)
  tracevals <- data.frame("TRACE_VAL" = Reduce("*", tracevals))

  if (model %in% c("model_7", "model_9")) {
    tracevals <- cbind(tracevals, power_lookup[model_params])
    tracevals <- tracevals[tracevals$TRACE_VAL != 0,, drop = FALSE]
    return(tracevals)
  }

  # for model 5 and 6 all parameter are constrained to be equal
  # and we can aggregate the trace values
  power_lookupSum <- power_lookup["POWER_ORDER"]
  colnames(power_lookupSum) <- define_spatial_lag_params(model)

  num_params <- length(model_params)
  tracevals$TRACE_VAL <- tracevals$TRACE_VAL / num_params^power_lookupSum[[1]]
  tracevals <- aggregate(tracevals, power_lookupSum, FUN = "sum")
  tracevals <- tracevals[tracevals$TRACE_VAL != 0,, drop = FALSE]
  return(tracevals[,c(2,1)])
}


#' @rdname derive_logdet_calculator
#' @keywords  internal
tracevals2params_noncartesian <- function(Wd, Wo, Ww, model, approx_order) {

  tl <- trace_template_noncartesian()

  if (model == "model_7") {

    tl <- tl[!grepl("w", tl$TRACE_KEY),,drop = FALSE]
    if (approx_order == 2) {
      tl$TRACE_VAL[tl$TRACE_KEY == "dd"] <- mprod_trace(Wd)
      tl$TRACE_VAL[tl$TRACE_KEY == "oo"] <- mprod_trace(Wo)
    }

    if (approx_order >= 3) {
      Wdd <- Wd %*% Wd
      Woo <- Wo %*% Wo
      tl$TRACE_VAL[tl$TRACE_KEY == "dd"] <- sum(diag(Wdd))
      tl$TRACE_VAL[tl$TRACE_KEY == "oo"] <- sum(diag(Woo))
      tl$TRACE_VAL[tl$TRACE_KEY == "ddd"] <- mprod_trace(Wdd, Wd)
      tl$TRACE_VAL[tl$TRACE_KEY == "ooo"] <- mprod_trace(Woo, Wo)
    }

    if (approx_order >= 4) {
      Wdo <- Wd %*% Wo
      tl$TRACE_VAL[tl$TRACE_KEY == "dddd"] <- mprod_trace(Wdd)
      tl$TRACE_VAL[tl$TRACE_KEY == "oooo"] <- mprod_trace(Woo)
      tl$TRACE_VAL[tl$TRACE_KEY == "dodo"] <- mprod_trace(Wdo)
      tl$TRACE_VAL[tl$TRACE_KEY == "ddoo"] <- mprod_trace(Wdd, Woo)


    }
  }

  if (model %in% c("model_8", "model_9")) {

    if (approx_order == 2) {
      tl$TRACE_VAL[tl$TRACE_KEY == "dd"] <- mprod_trace(Wd)
      tl$TRACE_VAL[tl$TRACE_KEY == "oo"] <- mprod_trace(Wo)
      tl$TRACE_VAL[tl$TRACE_KEY == "ww"] <- mprod_trace(Ww)
    }

    if (approx_order >= 3) {
      Wdd <- Wd %*% Wd
      Woo <- Wo %*% Wo
      Www <- Ww %*% Ww
      Wdo <- Wd %*% Wo

      tl$TRACE_VAL[tl$TRACE_KEY == "dd"] <- sum(diag(Wdd))
      tl$TRACE_VAL[tl$TRACE_KEY == "oo"] <- sum(diag(Woo))
      tl$TRACE_VAL[tl$TRACE_KEY == "ww"] <- sum(diag(Www))


      tl$TRACE_VAL[tl$TRACE_KEY == "ddd"] <- mprod_trace(Wdd, Wd)
      tl$TRACE_VAL[tl$TRACE_KEY == "ooo"] <- mprod_trace(Woo, Wo)
      tl$TRACE_VAL[tl$TRACE_KEY == "www"] <- mprod_trace(Www, Ww)
      tl$TRACE_VAL[tl$TRACE_KEY == "dww"] <- mprod_trace(Www, Wd)
      tl$TRACE_VAL[tl$TRACE_KEY == "oww"] <- mprod_trace(Www, Wo)
      tl$TRACE_VAL[tl$TRACE_KEY == "dow"] <- mprod_trace(Wdo, Ww)
    }

    if (approx_order >= 4) {
      tl$TRACE_VAL[tl$TRACE_KEY == "dddd"] <- mprod_trace(Wdd)
      tl$TRACE_VAL[tl$TRACE_KEY == "oooo"] <- mprod_trace(Woo)
      tl$TRACE_VAL[tl$TRACE_KEY == "wwww"] <- mprod_trace(Www)
      tl$TRACE_VAL[tl$TRACE_KEY == "dodo"] <- mprod_trace(Wdo)
      tl$TRACE_VAL[tl$TRACE_KEY == "ddoo"] <- mprod_trace(Wdd, Woo)
      tl$TRACE_VAL[tl$TRACE_KEY == "ddww"] <- mprod_trace(Www, Wdd)
      tl$TRACE_VAL[tl$TRACE_KEY == "ooww"] <- mprod_trace(Www, Woo)


      Wdw <- Wd %*% Ww
      Wow <- Wo %*% Ww
      Wod <- Wo %*% Wd
      tl$TRACE_VAL[tl$TRACE_KEY == "dwdw"] <- mprod_trace(Wdw)
      tl$TRACE_VAL[tl$TRACE_KEY == "owow"] <- mprod_trace(Wow)
      tl$TRACE_VAL[tl$TRACE_KEY == "ddow"] <- mprod_trace(Wow, Wdd)
      tl$TRACE_VAL[tl$TRACE_KEY == "doow"] <- mprod_trace(Wow, Wdo)
      tl$TRACE_VAL[tl$TRACE_KEY == "dodw"] <- mprod_trace(Wdw, Wdo)
      tl$TRACE_VAL[tl$TRACE_KEY == "dowo"] <- mprod_trace(Wow, Wod)
      tl$TRACE_VAL[tl$TRACE_KEY == "doww"] <- mprod_trace(Www, Wdo)
      tl$TRACE_VAL[tl$TRACE_KEY == "dwow"] <- mprod_trace(Wow, Wdw)
      tl$TRACE_VAL[tl$TRACE_KEY == "dwwo"] <- mprod_trace(Www, Wod)
      tl$TRACE_VAL[tl$TRACE_KEY == "dwww"] <- mprod_trace(Www, Wdw)
      tl$TRACE_VAL[tl$TRACE_KEY == "owww"] <- mprod_trace(Www, Wow)
    }
  }

  tl <- tl[tl$TRACE_ORDER <= approx_order,,drop = FALSE]
  tl$TRACE_VAL <- (tl$TRACE_VAL * tl$TRACE_COEF) / tl$TRACE_ORDER
  tl$TRACE_COEF <- NULL
  tl$TRACE_ORDER <- NULL


  param_powers <- data.frame(
    "rho_d" = count_pattern(tl$TRACE_KEY, "d"),
    "rho_o" = count_pattern(tl$TRACE_KEY, "o"),
    "rho_w" = count_pattern(tl$TRACE_KEY, "w"))

  if (model == "model_8") {
    # constraint on parameter rho_w = - rho_o * rho_d
    param_powers$rho_d <- param_powers$rho_d + param_powers$rho_w
    param_powers$rho_o <- param_powers$rho_o + param_powers$rho_w
    tl$TRACE_VAL <- tl$TRACE_VAL * (-1)^param_powers$rho_w
    param_powers$rho_w <- NULL
  }

  if (model == "model_7")
    param_powers$rho_w <- NULL

  tl$TRACE_KEY <- NULL
  tl <- aggregate(tl, param_powers, FUN = "sum")
  return(tl)

}

#' @rdname derive_logdet_calculator
#' @keywords internal
trace_template_noncartesian <- function() {

  order_2 <- data.frame(
    "TRACE_ORDER" = 2,
    "TRACE_KEY" = c("dd","oo","ww"),
    "TRACE_COEF" = 1)

  order_3_tracekey2coef <- c(
    "ddd" = 1,
    "ooo" = 1,
    "www" = 1,
    "dww" = 3,
    "oww" = 3,
    "dow" = 6)

  order_3 <- data.frame(
    "TRACE_ORDER" = 3,
    "TRACE_KEY" =  names(order_3_tracekey2coef),
    "TRACE_COEF" = as.integer(order_3_tracekey2coef))

  order_4_tracekey2coef <- c(
    "dddd" = 1,
    "oooo" = 1,
    "wwww" = 1,
    "dodo" = 2,
    "ddoo" = 4,
    "ddww" = 4,
    "dwdw" = 2,
    "ooww" = 4,
    "owow" = 2,
    "ddow" = 8,
    "dodw" = 4,
    "doow" = 8,
    "dowo" = 4,
    "doww" = 4,
    "dwow" = 4,
    "dwwo" = 4,
    "dwww" = 4,
    "owww" = 4)

  order_4 <- data.frame(
    "TRACE_ORDER" = 4,
    "TRACE_KEY" =  names(order_4_tracekey2coef),
    "TRACE_COEF" = as.numeric(order_4_tracekey2coef))

  full_template <- cbind(
    TRACE_VAL = NA,
    rbind(order_2, order_3, order_4))

  return(full_template)
}

#' @rdname derive_logdet_calculator
#' @keywords  internal
tracevals2approxldet <- function(tracevals) {

  approxldet <- function(rho) {

    powers <- tracevals
    powers[["TRACE_VAL"]] <- NULL
    param_vals <- Reduce("*", Map("^", rho, powers))
    logdet_val <- -as.numeric(sum(param_vals * tracevals$TRACE_VAL))
    return(logdet_val)
  }

  return(approxldet)
}

#' @keywords internal
#' @importFrom utils combn
multinom_table <- function(max_power, coef_names) {

  # Create a table for  multinomial coefficient and parameter powers
  nb_coefs <- length(coef_names)
  possible_powers <- seq(0, max_power)
  coef_powers <- combn(rep(possible_powers,nb_coefs),nb_coefs,simplify = FALSE)
  coef_powers <- as.data.frame(do.call("rbind",coef_powers),row.names = NULL)
  names(coef_powers) <- coef_names

  coef_powers[["POWER_ORDER"]] <- rowSums(coef_powers)
  coef_powers <- coef_powers[coef_powers$POWER_ORDER <= max_power & coef_powers$POWER_ORDER > 0,, drop = FALSE]
  coef_powers <- unique(coef_powers)
  coef_powers[["COEF_MULTINOM"]] <- multinom_coef(coef_powers[coef_names])
  row.names(coef_powers) <- NULL
  return(coef_powers)
}

#' @keywords internal
multinom_coef <- function(...) {

  #  Compute the multinomial coefficient
  #  The coefficient is computed for each row in a data.frame where the
  #  rows correspond to the power and the columns for one element
  k_args <- flatlist(list(...))
  t <- Reduce("+",k_args)

  # calculate the denominator
  chose_k_factorial <- Reduce("*", lapply(k_args , factorial))
  return(factorial(t)/chose_k_factorial)
}

