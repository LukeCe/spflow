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
#' @slot design_matrix A matrix (can be sparse) or NULL
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
           model_moments = "maybe_list"))

# ---- Methods ----------------------------------------------------------------

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
           model_moments) { # ---- add_details --------------------------------

    object@design_matrix <- drop_instruments(model_matrices)
    object@model_moments <- model_moments

    # add significance tests
    results_df <- results(object)
    results_df$"t.stat" <- results_df$est / results_df$sd
    results_df$"p.value" <- 1 - pt(q = abs(results_df$est / results_df$sd),
                                   df =  1)
    results(object) <- results_df

    # add fitted values , residuals, and goodness-of-fit
    nb_rho <- spatial_model_order(flow_control$model)
    mu <- coef(object)
    rho <- mu[seq_len(nb_rho)]
    delta <- mu[!names(mu) %in% names(rho)]

    fit_trend <- nb_rho
    if (nb_rho > 0) {
      fit_trend <- mapply(FUN = "*", model_matrices$Y[-1],rho,SIMPLIFY = FALSE)
      fit_trend <- as.vector(Reduce("+", x = fit_trend ))
    }

    fit_signal <- compute_signal(model_matrices = object@design_matrix,
                                 delta = delta)

    object@fitted <- fit_trend + fit_signal
    y_vec <- as.vector(object@design_matrix$Y_[[1]])
    object@resid <- y_vec - fitted(object)
    object@R2_corr <- cor(fitted(object),y_vec)^2

    return(object)
  })


#' @title Extract the coefficient vector from a spatial interaction model
#' @param object A [spflow_model-class()]
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "coef",
  signature = "spflow_model",
  function(object) { # ---- coef ----------------------------------------------
    results_df <- object@estimation_results
    return(lookup(results_df$est,rownames(results_df)))
  })


#' @title Extract a vector of fitted values from a spatial interaction model
#' @param object A [spflow_model()]
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "fitted",
  signature = "spflow_model",
  function(object) { # ---- fitted --------------------------------------------
    return(object@fitted)
  })

#' @title Access the number if observations of a spatial interaction model
#' @param object A [spflow_model()]
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "nobs",
  signature = "spflow_model",
  function(object) { # ---- nobs ----------------------------------------------
    return(object@N)
  })


#' @title Prediction methods for spatial interaction models
#' @param object A [spflow_model()]
#' @param type A character declaring the type of prediction (for now only "BP")
#' @param ... Further arguments passed to the prediction function
#'
#' @importFrom Matrix crossprod diag solve
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "predict",
  signature = "spflow_model",
  function(object, ..., type = "BP") { # ---- predict -------------------------


    # TODO describe the prediction better

    # information on the model case
    model <- object@estimation_control$model
    nb_rho <- spatial_model_order(model)
    # TODO design early exit for non-spatial: model_1
    if (model == "model_1") {
      assert(FALSE,warn = TRUE,
             "Predictions are not yet implemented for the non-spatial model")
    }

    ## 3 cases for predictions
    # fitted values -> existing data
    # prediction -> change of data for existing spatial units
    # extrapolation -> additional spatial units
    args <- list(...)

    fitted_values_case <-
      is.null(args$new_OX) & is.null(args$new_DX) & is.null(args$new_G)
    if (fitted_values_case) {

      # define the spatial filter of the model
      OW <- object@design_matrix$OW
      DW <- object@design_matrix$DW
      flow_dim <- dim(object@design_matrix$Y_[[1]])
      n_d <- flow_dim[1]
      n_o <- flow_dim[2]
      rho <- coef(object)[seq_len(nb_rho)]
      A <- expand_flow_neighborhood(DW = DW, OW = OW,
                                    n_o = n_o, n_d = n_d, model = model)
      A <- spatial_filter(A, rho)

      # create the signal part of the fitted values
      delta <- coef(object)[-seq_len(nb_rho)]
      signal <- compute_signal(object@design_matrix, delta)
      fit_signal <- solve(A,signal)

      # TODO generalize flow extraction to handle vector and matrix formulation
      # create the noise part of the fitted values
      observed_flows <- as.vector(object@design_matrix$Y_[[1]])
      noise <- observed_flows - fit_signal
      precision_mat <- crossprod(A) / (object@sd_error^2)
      diag_Q <- diag(precision_mat)
      Q_ndiag <- precision_mat - diag(diag_Q)
      fit_noise <- diag((1/diag_Q)) %*% (Q_ndiag %*% noise)

      return(as.vector(fit_signal - fit_noise))
      }

    predition_case <-
      is.null(args$new_OW) & is.null(args$new_DW)

    if (predition_case) {
      stop("Preditction are not yet implemented")
    }

    # extrapolation case...
    stop("Extrapolations are not yet implemented")


  })


#' @title Extract the vector of residuals values from a [spflow_model()]
#'
#' @param object A [spflow_model()]
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "resid",
  signature = "spflow_model",
  function(object) { # ---- resid ---------------------------------------------
    return(object@resid)
  })

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
  function(object){ # ---- results --------------------------------------------
    return(object@estimation_results)
  })

#' @title Internal method for overwriting the results
#' @noRd
#' @keywords internal
setReplaceMethod(
  f = "results",
  signature = "spflow_model",
  function(object, value) { # ---- results <- ---------------------------------
    object@estimation_results <- value
    if (validObject(object))
      return(object)
  })

#' @title
#'   Reshaped version of the estimation results that can be used to compare
#'   results in simulation studies.
#'
#' @rdname results_flat
#' @keywords internal
setMethod(
  f = "results_flat",
  signature = "spflow_model",
  function(object,
           res_info = c("est","sd"),
           cntrol_info = c("estimation_method")){ # ---- results_flat ---------

    res <- results(object)
    flat_results <- lapply(res_info, function(.col) {
        tmp <- suffix_columns(t(res[.col]), "_" %p% .col)
        data.frame(tmp, row.names = NULL,check.names = FALSE)
    })


    flat_controls <-
      cbind(as.data.frame(object@estimation_control[cntrol_info]),
            "sigma_est" = sd_error(object))

    return(cbind(flat_controls,flat_results))
  })

#' @rdname spflow_model-class
#' @export
setMethod(
  f = "sd_error",
  signature = "spflow_model",
  function(object){ # ---- sd_error -------------------------------------------
    return(object@sd_error)
  })

#' @keywords internal
setMethod(
  f = "show",
  signature = "spflow_model",
  function(object){ # ---- show -----------------------------------------------

    cntrl <- object@estimation_control
    cat(print_line(50))
    cat("\nSpatial interaction model estimated by:",
        toupper(cntrl$estimation_method) ,collapse = " ")
    cat("\nAutocorrelation structure:",
        cntrl$model, paste0("(",cntrl$model_type,")"),
        collapse = " ")
    cat("\nObservations:", nobs(object), collapse = " ")

    cat("\n\n")
    cat(print_line(50))
    cat("\nCoefficients:\n")
    print(round(results(object), digits = 2),
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
#' @param estimation_control A list of control parameters
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
  estimation_control,
  N,
  sd_error,
  R2_corr = NULL,
  resid = NULL,
  fitted = NULL,
  spatial_filter_matrix = NULL,
  design_matrix = NULL,
  model_moments = NULL) {

  est <- estimation_control$estimation_method
  model_class <- "spflow_model_" %p% est

  # fill generic arguments
  model_results <-
    new(model_class,
        estimation_results = estimation_results,
        estimation_control = estimation_control,
        N = N,
        sd_error = sd_error,
        R2_corr = R2_corr,
        resid = resid,
        fitted = fitted,
        spatial_filter_matrix = spatial_filter_matrix,
        design_matrix = design_matrix)

  # model specific arguments
  dot_args <- list(...)
  slot_ids <- names(dot_args)
  for (s in seq_along(dot_args)) {
    slot(model_results,slot_ids[s]) <- dot_args[[s]]
  }


  return(model_results)

}

# ---- Helper functions -------------------------------------------------------

#' @importFrom Matrix Diagonal
#' @keywords internal
compute_signal <- function(model_matrices, delta) {

  # index the coefficient vectors according to the model segments
  sub_index <- list(
    "const" = model_matrices$constants$global %||% 0,
    "const_intra" = 1 - is.null(model_matrices$constants$intra),
    "D_" = seq_len(ncol(model_matrices$D_) %||% 0) %||% 0,
    "O_" = seq_len(ncol(model_matrices$O_) %||% 0) %||% 0,
    "I_" = seq_len(ncol(model_matrices$I_) %||% 0) %||% 0,
    "G_" = seq_len(length(model_matrices$G)) %||% 0)
  sub_index <- sequentialize_index(sub_index)

  ## Calculate the components of the signal.
  # Missing components are set to zero and do not affect the final sum.
  # Number of destinations is required because of recycling.
  vector_or_null <- function(part_id) {
    .v <- model_matrices[[part_id]]
    .v %|!|% as.vector(.v %*% delta[sub_index[[part_id]]])
  }
  n_d <- nrow(model_matrices$Y_[[1]])

  # constant
  const <- delta[sub_index$const]

  # intra constant
  const_intra <- model_matrices$constants$intra %|!|%
    Diagonal(delta[sub_index$const_intra], n = n_d)
  const_intra <- const_intra %||% 0

  # destination part - (vector is recycled)
  dest <- vector_or_null("D_")
  dest <- dest %||% 0

  # origin part - vector is not recycled correctly ...
  orig <- vector_or_null("O_")
  orig <- orig %|!|% matrix(rep(orig,n_d),nrow = n_d,byrow = TRUE)
  orig <- orig %||% 0

  # intra part - only for diagonal elements
  intra <- vector_or_null("I_")
  intra <- intra %|!|% Diagonal(intra,n = length(intra))
  intra <- intra %||% 0

  # G part - (origin-destination pair attributes)
  g_term <- 0
  if (length(model_matrices$G_) != 0 ) {
    g_term <- Map("*", model_matrices$G_, delta[sub_index$G_])
    g_term <- as.matrix(Reduce("+", g_term))
  }

  signal <- as.vector(const + const_intra + dest + orig + intra + g_term)
  return(signal)

}

#' @keywords internal
derive_param_space_validator <- function(
  OW_eigen_range,
  DW_eigen_range,
  model,
  est) {


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

