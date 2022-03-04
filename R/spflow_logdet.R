derive_log_det_approximator <- function(
  OW,
  DW,
  n_o,
  n_d,
  model,
  approx_order,
  is_within,
  is_cartesian,
  flow_indicator) {

  if (is_cartesian) {
    OW_traces <- trace_sequence(model_matrices[["OW"]], approx_order)
    DW_traces <- OW_traces
    if (!is_within)
      DW_traces <- trace_sequence(model_matrices[["DW"]], approx_order)

    approx_log_det <- approxfun_log_det_cartesian(
      OW_traces = OW_traces,
      DW_traces = DW_traces,
      n_o = n_o,
      n_d = n_d,
      model = model)
    return(approx_log_det)
  }

  if (!is_cartesian) {
    W_flow <- expand_flow_neighborhood(
      OW = OW,
      DW = DW,
      n_o = n_o,
      n_d = n_d,
      flow_indicator = flow_indicator,
      model = model)



  }


}




#' Fill the lookup table created by [multinom_lookup_template()] with values
#' @keywords internal
approxfun_log_det_cartesian <- function(
  OW_traces = NULL,
  DW_traces = NULL,
  n_o,
  n_d,
  model) {

  easy_models <- "model_" %p% c(2,3,4,8)
  if (model %in% easy_models) {
    # the first trace is always equal to zero
    max_power <- max(length(OW_traces),length(DW_traces))
    powers_2p <- seq(2, max_power)
    tDW_traces <- DW_traces[-1] / powers_2p
    tOW_traces <- OW_traces[-1] / powers_2p

  log_det_calculator <- switch (model,
    "model_2" = function(rho) {
      rho_dt <- rho^powers_2p
      -as.numeric(n_o * sum(rho_dt * tDW_traces))
      },
    "model_3" = function(rho) {
      rho_ot <- rho^powers_2p
      -as.numeric(n_d * sum(rho_ot * tOW_traces))
      },
    "model_4" = function(rho) {
      rho_wt <- rho^powers_2p
      -as.numeric(sum(rho_wt * tOW_traces * DW_traces[-1]))
      },
    "model_8" = function(rho) {
      rho_dt <- rho[1]^powers_2p
      rho_ot <- rho[2]^powers_2p
      -as.numeric(n_o * sum(rho_dt * tDW_traces) +
                  n_d * sum(rho_ot * tOW_traces))
    })


  return(log_det_calculator)
  }

  # model 5,6,7 and 9 require a multinomial expansions
  # the parameters in 5 and 6 are those of 7 and 9
  # (just with equality constraint)
  mod_params <- switch (model, "model_5" = "model_7", "model_6" = "model_9", model)
  rho_names <- define_spatial_lag_params(mod_params)

  # we have to allow powers of 0 which are associated to the value 1
  max_power <- max(length(OW_traces),length(DW_traces))
  power_lookup <- multinom_lookup_template(max_power, rho_names)
  OW_values <- DW_values <- lookup(1, seq(0, max_power))
  OW_values[-1] <- OW_traces
  DW_values[-1] <- DW_traces


  OW_powers <- power_lookup$rho_o
  DW_powers <- power_lookup$rho_d
  if (mod_params == "model_9") {
    OW_powers <- OW_powers + power_lookup$rho_w
    DW_powers <- DW_powers + power_lookup$rho_w
    }

  # each row is one term in the multinomial sum
  trace_parts <- data.frame(
    OW_val = OW_values[as.character(OW_powers)],
    DW_val = DW_values[as.character(DW_powers)],
    n_o = n_o ^ (power_lookup$rho_d == power_lookup$t),
    n_d = n_d ^ (power_lookup$rho_o == power_lookup$t),
    coef = power_lookup$c_multinom,
    t_inv = 1/power_lookup$t)

  trace_parts <- Reduce("*",trace_parts)
  rho_powers <- power_lookup[rho_names]


  log_det_calculator <- switch (model,
    "model_5" = function(rho){
      rho <- rep(rho/2, 2)
      -sum(trace_parts * Reduce("*",Map("^", rho,rho_powers)))
    },
    "model_6" = function(rho){
      rho <- rep(rho/3, 3)
      -sum(trace_parts * Reduce("*",Map("^", rho,rho_powers)))
    },
    "model_7" = function(rho){
      -sum(trace_parts * Reduce("*",Map("^", rho,rho_powers)))
    },
    "model_9" = function(rho){
      -sum(trace_parts * Reduce("*",Map("^", rho,rho_powers)))
    })
  return(log_det_calculator)
}


approxfun_log_det_noncartesian <- function(
  Wd,
  Wo,
  Ww,
  approx_oder,
  model) {



  model_num <- substr(model, 7, 7)
  model_with_single_weight_matrix <- model_num %in% c(2,3,4,5,6)

  if (model == "model_2")
    tWx_traces <- trace_sequence(Wd)

  if (model == "model_3")
    tWx_traces <- trace_sequence(Wo)

  if (model == "model_4")
    tWx_traces <- trace_sequence(Ww)

  if (model == "model_5")
    tWx_traces <- trace_sequence((Wd + Wo) / 2)

  if (model == "model_6")
    tWx_traces <- trace_sequence((Wd + Wo + Ww) / 3)

  if (model_with_single_weight_matrix) {
    # the first trace is always zero
    powers_2p <- seq(2, approx_oder)
    tWx_traces <- tWx_traces[-1] / powers_2p
    log_det_calculator <- function(rho) {
      rho_xt <- sum(rho)^powers_2p
      return(-as.numeric(sum(rho_xt * tWx_traces)))
    }
    return(log_det_calculator)
  }

  assert(approx_oder <= 4, warn = TRUE,
         "For the non-cartesian models with multiple parameters we can only
         use the log-determinant approximation of order four or lower.
         The estimation proceeds with order four.")

  stop("Implement log-det!!!")
}



#' Create a lookup table for the multinomial coefficient
#' @keywords internal
#' @importFrom utils combn
multinom_lookup_template <- function(max_power, coef_names) {

  nb_coefs <- length(coef_names)
  possible_powers <- seq(0, max_power)
  coef_orders <- combn(rep(possible_powers,nb_coefs),nb_coefs,simplify = FALSE)
  coef_orders <- as.data.frame(do.call("rbind",coef_orders),row.names = NULL)
  names(coef_orders) <- coef_names

  coef_orders$t <- rowSums(coef_orders)
  coef_orders <- coef_orders[coef_orders$t <= max_power,]
  coef_orders <- coef_orders[coef_orders$t > 0,]
  coef_orders <- unique(coef_orders)
  coef_orders$c_multinom <- multinom_coef(coef_orders[coef_names])

  return(coef_orders)
}


assert(approx_order >= 2,
       "Approximations of order less than two have no meaning!")
assert(approx_order < 5,
       "Approximations of order five or higher are not implemented!")

#' @keywords internal
trace_lookup_template <- function() {

  order_2 <- data.frame(
    "TRACE_ORDER" = 2,
    "TRACE_KEY" = c("dd","oo","ww"),
    "TRACE_COEF" = 1,
    "TRACE_VAL" = NA,
    "PARAM_KEY" = c("dd", "oo", "ww"),
    "PARAM_VAL" = NA)

  tracekey2coef <- c(
    "ddd" = 1,
    "ooo" = 1,
    "www" = 1,
    "dww" = 3,
    "oww" = 3,
    "dow" = 6)

  order_3 <- data.frame(
    "TRACE_ORDER" = 3,
    "TRACE_KEY" =  names(tracekey2coef),
    "TRACE_COEF" = 1,
    "TRACE_VAL" = NA,
    "PARAM_KEY" = sort_chars(names(tracekey2coef)),
    "PARAM_VAL" = NA)

  tracekey2coef <- c(
    "dddd" = 1,
    "oooo" = 1,
    "wwww" = 1,
    "ddoo" = 6,
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
    "TRACE_KEY" =  names(tracekey2coef),
    "TRACE_COEF" = 1,
    "TRACE_VAL" = NA,
    "PARAM_KEY" = sort_chars(names(tracekey2coef)),
    "PARAM_VAL" = NA)

  return(rbind(order_2, order_3, order_4))


}
