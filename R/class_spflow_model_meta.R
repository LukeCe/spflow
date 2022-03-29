# ---- Class definition -------------------------------------------------------
#' @include class_generics_and_maybes.R

#' @title Class spflow_model
#'
#' @description
#' An S4 class that contains the estimation results of spatial econometric
#' interaction models estimated by the [spflow()] function.
#'
#' There are four subclasses that are specific to the chosen estimation method
#' (OLS, MLE, Bayesian MCMC or S2SLS).
#' They contain some additional information specific to the corresponding
#' method but most behaviours and data are identical among them.
#'
#' @slot estimation_results
#'   A data.frame that contains the main [results()] of the estimation
#' @slot estimation_control
#'   A list that contains all control parameters of the estimation
#'   (see [spflow_control()])
#' @slot N
#'   A numeric that corresponds to the number of origin-destination pairs
#'   (the sample size in this model)
#' @slot sd_error
#'   A numeric representing the standard deviation of the residual
#' @slot R2_corr
#'   A numeric that serves as a goodness of fit criterion.
#'   The R2_corr is computed as squared correlation between the fitted values
#'   and the observed values of the dependent variable.
#' @slot resid
#'   A numeric vector of regression residuals
#' @slot fitted
#'   A numeric vector of fitted values computed as the in sample prediction
#'   trend signal (TS) prediction described by @Goulard2017
#' @slot spatial_filter_matrix A matrix (can be sparse) or NULL
#' @slot design_matrix A list or NULL
#' @slot fit_diagnostics A list or NULL
#'
#' @name spflow_model-class
#' @seealso [spflow()], [spflow_network_classes()]
#' @examples
#'
#' spflow_results <- spflow(y9 ~ . + G_(DISTANCE),multi_net_usa_ge)
#'
#' # General methods
#' results(spflow_results) # data.frame of main results
#' coef(spflow_results) # vector of estimated coefficients
#' fitted(spflow_results) # vector of fitted values
#' resid(spflow_results) # vector of residuals
#' nobs(spflow_results) # number of observations
#' sd_error(spflow_results) # standard deviation of the error term
#' predict(spflow_results) # computation of the in sample predictor
#'
#' # MLE methods
#' logLik(spflow_results) # value of the likelihood function
#'
#' # MLE, OLS and S2SLS methods
#' varcov(spflow_results) # variance covariance matrix of the estimators
#'
#' # MCMC methods
#' spflow_results_mcmc <- spflow(
#'   y2 ~ . + G_(DISTANCE),
#'   multi_net_usa_ge,
#'   flow_control = spflow_control(estimation_method = "mcmc",
#'                                 model = "model_2"))
#' results(spflow_results)
#' mcmc_results(spflow_results_mcmc) # parameter values during the mcmc sampling
#'
setClass("spflow_model",
         slots = c(
           estimation_results = "data.frame",
           estimation_control = "list",
           N = "numeric",
           sd_error = "numeric",
           R2_corr = "maybe_numeric",
           resid = "maybe_numeric",
           fitted = "maybe_numeric",
           spatial_filter_matrix = "maybe_any_matrix",
           design_matrix = "maybe_list",
           model_moments = "maybe_list",
           fit_diagnostics = "maybe_list"))


# ---- Methods ----------------------------------------------------------------


# ---- ... add_details --------------------------------------------------------
#' @title Internal method to add details to a [spflow_model-class()]
#'
#' @details
#' The method adds the design matrix and the coefficient names to an
#' [spflow_model-class()] object.
#' It also calculates the fitted values and the residuals as well as a
#' goodness-of-fit measure.
#'
#' @param object A [spflow_model-class()]
#' @param model_matrices A list as returned by [spflow_model_matrix()]
#' @param flow_control A list as returned by [spflow_control()]
#' @param model_moments A list as returned by [spflow_model_moments()]
#' @name add_details
#' @return An object of class spflow_model
#' @keywords  internal
setMethod(
  f = "add_details",
  signature = "spflow_model",
  function(object,
           model_matrices,
           flow_control,
           model_moments) {

    object@design_matrix <- drop_instruments(model_matrices)
    object@model_moments <- model_moments

    # add fitted values , residuals, and goodness-of-fit
    nb_rho <- spatial_model_order(flow_control$model)
    mu <- coef(object)
    rho <- mu[seq_len(nb_rho)]
    delta <- mu[!names(mu) %in% names(rho)]


    object@fitted <- predict(
      object,
      type = flow_control$fitted_value_method,
      approx_expectation = flow_control$approx_expectation,
      expectation_approx_order = flow_control$approx_expectation,
      keep_matrix_form = FALSE)

    true_vals <- as.vector(object@design_matrix$Y_[[1]])
    flow_indicators <- as.vector(object@design_matrix$flow_indicator)
    if (!is.null(flow_indicators))
      true_vals <- true_vals[flow_indicators]

    object@resid <- object@fitted - true_vals
    object@R2_corr <- cor(object@fitted, true_vals)^2
    return(object)
  })


# ---- ... coef ---------------------------------------------------------------
#' @title Extract the coefficient vector from a spatial interaction model
#' @param object A [spflow_model-class()]
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "coef",
  signature = "spflow_model",
  function(object, which = NULL) {

    results_df <- object@estimation_results
    coefs <- lookup(results_df$est,rownames(results_df))

    if (is.null(which))
      return(coefs)

    nb_rho <- spatial_model_order(object@estimation_control$model)
    res <- NULL
    if ("rho" %in% which & nb_rho > 0) {
      res <- c(res,coefs[seq_len(nb_rho)])
    }

    if ("delta" %in% which) {
      res <- c(res,coefs[seq(from = 1 + nb_rho, to = length(coefs))])
    }

    return(res)
  })

# ---- ... fitted -------------------------------------------------------------
#' @title Extract a vector of fitted values from a spatial interaction model
#' @param object A [spflow_model()]
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "fitted",
  signature = "spflow_model",
  function(object) {
    return(object@fitted)
  })


# ---- ... nobs ---------------------------------------------------------------
#' @title Access the number if observations of a spatial interaction model
#' @param object A [spflow_model()]
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "nobs",
  signature = "spflow_model",
  function(object) {
    return(object@N)
  })


# ---- ... predict ------------------------------------------------------------
#' @title Prediction methods for spatial interaction models
#' @param object A [spflow_model()]
#' @param method A character indicating which method to use for computing the
#'   predictions. Should be one of c("TS", "TC", "BP").
#' @param new_data An object containing new data (to be revised)
#' @param ... Further arguments passed to the prediction function
#' @inheritParams spflow_control
#'
#' @importFrom Matrix crossprod diag solve
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "predict",
  signature = "spflow_model",
  function(object,
           ...,
           new_data = NULL,
           type = "TS",
           approx_expectation = TRUE,
           expectation_approx_order = 10,
           keep_matrix_form = FALSE) {


    # extract coefficients and compute the signal
    model <- object@estimation_control$model
    rho <- coef(object, "rho")
    delta <- coef(object, "delta")

    # compute fitted values
    if (is.null(new_data)) {

      signal <- compute_signal(object@design_matrix, delta)

      if (model == "model_1")
        type <- "LIN"

      Y_hat <- switch(
        type,
        "LIN" = signal,
        "TC" =  {
          compute_expectation(
            signal_matrix = signal,
            DW = object@design_matrix$DW,
            OW = object@design_matrix$OW,
            rho = rho,
            model = model,
            flow_indicator = object@design_matrix$flow_indicator,
            approximate = approx_expectation,
            max_it = expectation_approx_order
          )},
        "TS" = {
          trend <- Reduce("+", Map("*", object@design_matrix$Y_[-1], rho))
          trend + signal
        },
        "BP" = {
          stop("Best Prediction for in sample not yet implemented")
          # compute_bp_insample(
          #   signal_matrix = signal,
          #   DW = DW,
          #   OW = OW,
          #   rho = rho,
          #   model = model,
          #   Y_indicator = NULL
          # )
        }
      )
    }

    # compute predictions
    if (!is.null(new_data)) {
      stop("Predictions for new data are not yet implemented!")
    }


    if (keep_matrix_form)
      return(Y_hat)

    flow_indicators <- as.vector(object@design_matrix$flow_indicator)
    if (is.null(flow_indicators))
      return(as.vector(Y_hat))

    return(as.vector(Y_hat)[flow_indicators])

  })


# ---- ... resid --------------------------------------------------------------
#' @title Extract the vector of residuals values from a [spflow_model()]
#'
#' @param object A [spflow_model()]
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "resid",
  signature = "spflow_model",
  function(object) {
    return(object@resid)
  })

# ---- ... results ------------------------------------------------------------
#' @section Main results:
#' The main results are accessed with the `results()` method.
#' They are given in the form of a data frame with the following columns;
#'
#' * `est`: value of the estimated parameter
#' * `sd`: value of the standard deviation of the parameter
#' * `t.test`: value of the t-statistic under the two-sided hypothesis that
#'  the parameter value is 0.
#' * `p.val`: the p-value associated to the t-test
#' * `quant_025`: for Bayesian estimation the lower bound of 95% interval
#' * `quant_975`: for Bayesian estimation the upper bound of 95% interval
#'
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "results",
  signature = "spflow_model",
  function(object) {
    return(object@estimation_results)
  })

# ---- ... results <- ---------------------------------------------------------
#' @title Internal method for overwriting the results
#' @noRd
#' @keywords internal
setReplaceMethod(
  f = "results",
  signature = "spflow_model",
  function(object, value) {
    object@estimation_results <- value
    if (validObject(object))
      return(object)
  })

# ---- ... sd_error -----------------------------------------------------------
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "sd_error",
  signature = "spflow_model",
  function(object){
    return(object@sd_error)
  })


# ---- ... show ---------------------------------------------------------------
#' @keywords internal
setMethod(
  f = "show",
  signature = "spflow_model",
  function(object){

    cntrl <- object@estimation_control
    cat(print_line(50))
    cat("\nSpatial interaction model estimated by:",
        toupper(cntrl$estimation_method) ,collapse = " ")
    cat(sprintf("\nSpatial correlation structure: %s (%s)",
                cntrl$spatial_type,
                cntrl$model))
    cat("\nObservations:", nobs(object), collapse = " ")

    cat("\n\n")
    cat(print_line(50))
    cat("\nCoefficients:\n")
    print(round(results(object)[,1:4], digits = 2),
          print.gap = 2L,
          quote = FALSE)

    cat("\n")
    cat(print_line(50))
    cat("\nR2_corr:", object@R2_corr, collapse = " ")


    invisible(object)
  })

# ---- Constructors -----------------------------------------------------------
#' @title Internal function to construct a [spflow_model-class()]
#'
#' @param estimation_results A data.frame of estimation [results()]
#' @param flow_control A list of control parameters
#' @param N A numeric indicating the number of observations
#' @param sd_error A numeric which reports the
#' @param R2_corr A numeric which reports a pseudo R^2 measure
#' @param resid A numeric vector of regression residuals
#' @param fitted A numeric vector of fitted values
#' @param spatial_filter_matrix A matrix which represents the spatial filter
#' @param design_matrix The design matrix/matrices of the model
#' @param ...
#'   Further arguments passed to more specific classes in accordance to the
#'   estimation method
#'
#' @importFrom methods slot<- slot
#' @keywords internal
spflow_model <- function(
    ...,
    estimation_results,
    flow_control,
    N,
    sd_error,
    R2_corr = NULL,
    resid = NULL,
    fitted = NULL,
    spatial_filter_matrix = NULL,
    design_matrix = NULL,
    model_moments = NULL,
    fit_diagnostics = NULL) {

  est <- flow_control$estimation_method
  model_class <- paste0("spflow_model_", est)

  # fill generic arguments
  model_results <-
    new(model_class,
        estimation_results = estimation_results,
        estimation_control = flow_control,
        N = N,
        sd_error = sd_error,
        R2_corr = R2_corr,
        resid = resid,
        fitted = fitted,
        spatial_filter_matrix = spatial_filter_matrix,
        design_matrix = design_matrix,
        fit_diagnostics = fit_diagnostics)

  # model specific arguments
  dot_args <- list(...)
  slot_ids <- names(dot_args)
  for (s in seq_along(dot_args)) {
    slot(model_results,slot_ids[s]) <- dot_args[[s]]
  }


  return(model_results)

}

# ---- Helper functions -------------------------------------------------------


#' @keywords internal
derive_param_space_validator <- function(
    OW_eigen_range,
    DW_eigen_range,
    model,
    est) {

  is_spatial <- flow_control[["estimation_method"]] != "ols"
  OW_eigen_range <- DW_eigen_range <- NULL

  # if (is_spatial) {
  #   real_eigen_range <- function(W) {
  #     if (is.null(W))
  #       return(NULL)
  #
  #     max_abs <- RSpectra::eigs(W,1,"LM")$values
  #
  #     if (Re(max_abs) > 0) {
  #       max_re <- max_abs
  #       min_re <- RSpectra::eigs(W,1,"SR")$values
  #     }
  #
  #     if (Re(max_abs) < 0) {
  #       min_re <- max_abs
  #       max_re <- RSpectra::eigs(W,1,"LR")$values
  #     }
  #
  #     return(c(min_re, max_re))
  #   }
  #
  #   OW_eigen_range <- DW_eigen_range <-
  #     real_eigen_range(model_matrices[["OW"]])
  #
  #   if (!flow_control[["mat_within"]])
  #     DW_eigen_range <- real_eigen_range(model_matrices[["DW"]])
  #
  # }

  DWmax <- DW_eigen_range[1]
  DWmin <- DW_eigen_range[2]
  OWmax <- OW_eigen_range[1]
  OWmin <- OW_eigen_range[2]


  # TODO finish the parameter space...
  # ...  check that at least one of rho_o and rho_d is not complex
  mod_params <-
    switch (model, "model_5" = "model_7", "model_6" = "model_9", model)
  rho_names <- define_spatial_lag_params(mod_params)
  WF_eigen_part <- rbind(
    "max_max" = c("rho_d" = DWmax, "rho_o" = OWmax, "rho_w" =  OWmax*DWmax),
    "max_min" = c("rho_d" = DWmax, "rho_o" = OWmin, "rho_w" =  OWmin*DWmax),
    "min_max" = c("rho_d" = DWmin, "rho_o" = OWmax, "rho_w" =  OWmax*DWmin),
    "min_min" = c("rho_d" = DWmin, "rho_o" = OWmin, "rho_w" =  OWmin*DWmin)
  )[,rho_names]


  validate_fun <- function(rho){
    rho_zero[rho_names] <- rho
    WF_eigen_range <- range(rowSums(WF_eigen_part[,names] %*% diag(rho)))
    WF_eigen_range[2] < 1 & (WF_eigen_range[1] > -1 | est == "s2sls")
  }

  return(validate_fun)
}

#' @keywords internal
create_results <- function(...) {

  r <- list(...)
  if (is.null(r[["est"]]))
    stop("Argument est for estimates is required!")

  if (is.null(r[["sd"]]))
    stop("Argument sd for standard deviation is required!")

  if (is.null(r[["t.stat"]]))
    r <- c(r, list("t.stat" = r[["est"]] / r[["sd"]]))

  if (is.null(r[["p.val"]]))
    r <- c(r, list("p.val" = pt(1 - abs(r[["t.stat"]]), 1)))

  if (is.null(r[["quant_025"]]))
    r <- c(r, list("quant_025" = qnorm(.025, r[["est"]], r[["sd"]])))

  if (is.null(r[["quant_975"]]))
    r <- c(r, list("quant_975" = qnorm(.975, r[["est"]], r[["sd"]])))

  results <- data.frame(r)
  return(results)
}



