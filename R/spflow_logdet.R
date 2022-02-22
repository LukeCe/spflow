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

#' Fill the lookup table created by [multinom_lookup_template()] with values
#' @keywords internal
derive_log_det_calculator <- function(
  OW_traces = NULL,
  DW_traces = NULL,
  n_o,
  n_d,
  model) {


  # first treat the "easy cases"
  easy_models <- "model_" %p% c(2,3,4,8)

  if (model %in% easy_models) {
    # the first power is ignored because it is always weighted by 0
    max_power <- max(length(OW_traces),length(DW_traces))
    powers_2p <- seq(2, max_power)
    tDW_traces <- DW_traces[-1] / powers_2p
    tOW_traces <- OW_traces[-1] / powers_2p

  log_det_calculator <- switch (model,
    "model_2" = function(rho) {
      rho_p <- rho^powers_2p
      -as.numeric(n_o * sum(rho_p * tDW_traces))
      },
    "model_3" = function(rho) {
      rho_p <- rho^powers_2p
      -as.numeric(n_d * sum(rho_p * tOW_traces))
      },
    "model_4" = function(rho) {
      rho_p <- rho^powers_2p
      -as.numeric(sum(rho_p * tOW_traces * DW_traces[-1]))
      },
    "model_8" = function(rho) {
      rho1_p <- rho[1]^powers_2p
      rho2_p <- rho[2]^powers_2p
      -as.numeric(n_o * sum(rho1_p * tDW_traces) +
                  n_d * sum(rho2_p * tOW_traces))
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
