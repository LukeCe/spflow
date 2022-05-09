#' @include class_generics_and_maybes.R class_sp_network_multi.R

# ---- Class definition -------------------------------------------------------
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
#' @slot estimation_diagnostics
#'   A list of further indicators about the estimation
#' @slot spflow_matrices A list or NULL
#'
#' @name spflow_model-class
#' @seealso [spflow()], [spflow_network_classes()]
#' @examples
#'
#' spflow_results <- spflow(y9 ~ . + G_(DISTANCE), multi_net_usa_ge)
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
setClass("spflow_model", slots = c(
  estimation_results = "data.frame",
  estimation_control = "list",
  estimation_diagnostics = "list",
  spflow_data = "maybe_list",
  spflow_neighborhood = "maybe_list",
  spflow_formula = "maybe_formula",
  spflow_moments = "maybe_list",
  spflow_matrices = "maybe_list",
  spflow_indicators = "maybe_data.frame"))


setClass("spflow_model_ols", contains = "spflow_model")
setClass("spflow_model_mle", contains = "spflow_model")
setClass("spflow_model_s2sls", contains = "spflow_model")
setClass("spflow_model_mcmc", contains = "spflow_model")
setClassUnion("spflow_model_varcov",
              c("spflow_model_ols",
                "spflow_model_mle",
                "spflow_model_s2sls"))
setClassUnion("spflow_model_spatial",
              c("spflow_model_mcmc",
                "spflow_model_mle",
                "spflow_model_s2sls"))

# ---- Methods ----------------------------------------------------------------
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


# ---- ... coord --------------------------------------------------------------
#' @keywords internal
setMethod(
  f = "coord",
  signature = "spflow_model",
  function(object) {

    if (is.null(object@spflow_data))
      return(NULL)

    cc <- lapply(compact(object@spflow_data[c("orig", "dest")]), function(.d) {
      cc <- .d[,get_keycols(.d)]
      if (!isTRUE(ncol(cc) == 3))
        return(NULL)
      row.names(cc) <- cc[[1]]
      cc[[1]] <- NULL
      colnames(cc) <- c("COORD_X","COORD_Y")
      cc
      })
    unique(Reduce("rbind",cc))
  })

# ---- ... fitted -------------------------------------------------------------
#' @title Extract a vector of fitted values from a spatial interaction model
#' @param object A [spflow_model()]
#' @param type
#'  A character indicating the format of the returned values:
#'  -  "V" leads to an atomic vector
#'  -  "M" leads to a OD matrix where missing data is replaced by zeros
#'  -  "OD" leads to a data.frame with columns being the the values
#'      and the id's of the destinations and the origins
#'
#' @rdname spflow_model-class
#' @name fitted
#' @export
setMethod(
  f = "fitted",
  signature = "spflow_model",
  function(object, type = "V") {
    do_k <- object@spflow_indicators
    spflow_indicators2format(do_k[,c(names(do_k)[1:2],"FITTED")], type, do_k[["HAS_Y"]])
  })

# ---- ... flow_map -----------------------------------------------------------
#' @title Plot the map of flows
#' @name flow_map
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "flow_map",
  signature = "spflow_model",
  function(object,
           ...,
           flow_type = "resid",
           add_title = TRUE) {

    assert_is_single_x(flow_type, "character")

    type_options <- c(
      "resid" = "Residuals",
      "fitted" = "Fitted values",
      "actual" = "True values")
    assert_valid_case(flow_type, names(type_options))

    do_flows <- match.fun(flow_type)(object, "OD")
    args <- list(
      "y" = abs(do_flows[[3]]),
      "index_o" = do_flows[[2]],
      "index_d" = do_flows[[1]])
    args <- c(args, list(...))


    if (is.null(args[["coords_s"]])) {
      args[["coords_s"]] <- coord(object)
    }

    do.call("map_flows", args)
    if (add_title)
      title(type_options[flow_type])
  })


# ---- ... flow_moran_plots  --------------------------------------------------
#' @title Plot the map of flows
#' @name flow_moran_plots
#' @rdname spflow_model-class
setMethod(
  f = "flow_moran_plots",
  signature = "spflow_model",
  function(object, model, DW, OW, add_lines = TRUE) {

    if (missing(model)) model <- object@estimation_control[["model"]]
    if (missing(DW)) DW <- object@spflow_neighborhood[["DW"]]
    if (missing(OW)) OW <- object@spflow_neighborhood[["OW"]]

    assert(model != "model_1", "The Moran plot is for spatial models!")

    # recompute the moments pretending the errors are the flows
    # with flows becoming exogenous variables

    M_indicator <- spflow_indicators2mat(object@spflow_indicators)
    E_ <- lag_flow_matrix(
      Y = resid(object, "M"),
      model = model,
      OW = OW,
      DW = DW,
      name = "RESID",
      M_indicator = M_indicator)

    E_ <- lapply(E_, vec_format_d_o, do_keys = object@spflow_indicators, type = "V")
    E_1 <- cbind(1,E_[[1]])
    for (i in seq_len(length(E_) - 1)) {
      ii <- sub("RESID.",replacement = "", names(E_)[i + 1])


      title_expr <- bquote(paste("Moran scatterplot of residuals (",  W[.(ii)], " - lag)"))
      plot(y = E_[[i + 1]], x = E_[[1]],
           main = title_expr,
           xlab = expression(residual),
           ylab = bquote(W[.(ii)] %.% "resdiual"))
      if (add_lines)
        abline(lm.fit(x = E_1 , y = E_[[i + 1]]), col = "red") ; abline(0,0)
    }
  })

# ---- ... logLik -------------------------------------------------------------
#' @param object A [spflow_model-class()]
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "logLik",
  signature = "spflow_model_mle",
  function(object) return(object@estimation_diagnostics[["ll"]]))

# ---- ... mcmc_results ------------------------------------------------------
#' @param object A [spflow_model-class()]
#' @rdname spflow_model-class
setMethod(
  f = "mcmc_results",
  signature = "spflow_model_mcmc",
  function(object) return(object@estimation_diagnostics[["mcmc_results"]]))

# ---- ... nobs ---------------------------------------------------------------
#' @title Access the number of observations inside a [spflow_model]
#' @param object A [spflow_model()]
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "nobs",
  signature = "spflow_model",
  function(object, which = "fit") {
    assert_valid_case(which, c("fit", "cart", "pred", "pair", "orig", "dest"))
    return(object@estimation_diagnostics[[paste0("N_", which)]])
  })


# ---- ... pair_corr ----------------------------------------------------------
#' @param type
#'   A character, that should indicate one of `c("fir", "empric")`
#'   - "fit" will use the moments that have been used during the estimation
#'   - "empric" will recompute these moments ignoring the weights
#' @param add_residuals
#'   A logical, indicating whether the model residuals should be added to the
#'   correlation matrix
#' @param model
#'  A character that should indicate one of `paste0("model_", 1:9)`.
#'  The option specifies different correlation structures that are detailed in
#'  the help page of [spflow_control()]
#'
#' @rdname pair_corr
#' @export
setMethod(
  f = "pair_corr",
  signature = "spflow_model",
  function(object, type = "fit", add_fitted = TRUE, add_resid = TRUE, model) {

    type_options <- c("fit", "empiric")
    assert_valid_case(type, type_options)

    new_mat <- object@spflow_matrices
    new_mat[["CONST"]][["(Intercept)"]] <- 1
    new_mom <- object@spflow_moments
    keep_moments <- type == "fit" || is.null(object@estimation_control[["weight_variable"]])
    new_mat[["weights"]] <- NULL
    if (!keep_moments) {
      new_mom <- compute_spflow_moments(
        model_matrices = new_mat,
        flow_control = object@estimation_control,
        ignore_na = TRUE)
    }

    if (!add_resid & !add_fitted)
      return(new_mom$TCORR)

    if (missing(model))
      model <- object@estimation_control[["model"]]

    # recompute the moments pretending the errors are the flows
    # with flows becoming exogenous variables
    new_mat[["G_"]] <- c(new_mat[["G_"]], new_mat[["Y_"]])
    new_mat[["Y_"]] <- NULL


    if (add_fitted) {
      new_mat[["Y_"]] <- list(fitted(object, "M"))
      names(new_mat[["Y_"]]) <- paste0(names(object@spflow_matrices[["Y_"]])[1], ".fit")
    }

    M_indicator <- spflow_indicators2mat(object@spflow_indicators)
    if (add_resid) {
      new_mat[["Y_"]] <- c(
        new_mat[["Y_"]],
        lag_flow_matrix(
          Y = resid(object, "M"),
          model = model,
          OW = object@spflow_neighborhood[["OW"]],
          DW = object@spflow_neighborhood[["DW"]],
          name = "RESID",
          M_indicator = M_indicator
        ))
    }


    JE <- lapply(new_mat[["Y_"]], "spflow_moment_cov", new_mat)
    JE <- Reduce("cbind", JE, matrix(nrow = length(JE[[1]]), ncol = 0))
    colnames(JE) <- names(new_mat[["Y_"]])

    N <- new_mom[["N"]]
    UU <- new_mom[["UU"]]
    UY <- new_mom[["UY"]]
    TSS <- new_mom[["TSS"]]

    UU <- rbind(cbind(UU,UY), cbind(t(UY), TSS))
    UY <- JE
    TSS <- crossproduct_mat_list(new_mat[["Y_"]])

    # covariance matrix
    TCORR <- rbind(cbind(UU,UY), cbind(t(UY), TSS))
    TCORR <- TCORR - (outer(TCORR[1,], TCORR[1,])/N)
    TCORR <- TCORR / outer(sqrt(diag(TCORR)), sqrt(diag(TCORR)))
    diag(TCORR[-1,-1]) <- 1
    return(TCORR)
    })


# ---- ... plot ---------------------------------------------------------------
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "plot",
  signature = "spflow_model",
  function(x, ...) {

    qqnorm(y = resid(x), main = "Normal QQ-Plot of Residuals")
    qqline(resid(x), col = "red")

    fitted_x <- fitted(x, "V")
    resid_x <- resid(x, "V")
    plot(x = fitted_x, xlab = "Fitted",
         y = resid_x,  ylab = "Residual",
         main = "Residual vs Fitted")
    abline(lm.fit(cbind(1,fitted_x), resid_x), col = "red") ; abline(a = 0, b = 0)

    actual_x <- actual(x, "V")
    plot(x = fitted_x, xlab = "Fitted",
         y = actual_x, ylab = "Actual",
         main = "Actual vs Fitted")
    abline(lm.fit(cbind(1,fitted_x), actual_x), col = "red") ; abline(a = 0, b = 1)


    coords_s <- list(...)[["coords_s"]] %||% coord(x)
    if (!is.null(coords_s)) {
      x_or_25_percent <- min(50,nobs(x) / 4)
      keep_x_at_most <- (nobs(x) - x_or_25_percent)  / nobs(x)
      flow_map(x, coords_s = coords_s, flow_type = "fitted", filter_lowest = keep_x_at_most, legend = "bottomright")
      flow_map(x, coords_s= coords_s, flow_type = "resid", filter_lowest = keep_x_at_most, legend = "bottomright")
    }

    if (!inherits(x, "spflow_model_ols"))
      flow_moran_plots(x)

    if (inherits(x, "spflow_model_mcmc"))
      plot(mcmc_results(a),density = FALSE, ask = FALSE)

    corr_map(pair_corr(x))
    title(x = "Pairwise correlations")
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
           in_sample = is.null(new_data),
           method = "TC",
           approx_expectation = TRUE,
           expectation_approx_order = 10,
           return_type = "OD") {


    # extract coefficients and compute the signal
    model <- object@estimation_control$model
    rho <- coef(object, "rho")
    delta <- coef(object, "delta")

    if (!is.null(new_data))
      stop("Data update not yet implmented!")

    spflow_indicators <- object@spflow_indicators
    filter_case <- spflow_indicators[["HAS_SIG"]] %||% TRUE
    if (in_sample)
      filter_case <- filter_case & (spflow_indicators[["HAS_Y"]] %||% TRUE)
    spflow_indicators <- spflow_indicators[filter_case,,drop = FALSE]
    spflow_matrices <- object@spflow_matrices

    signal <- compute_signal(
      delta = delta,
      spflow_matrices = spflow_matrices,
      spflow_indicators = spflow_indicators,
      keep_matrix_form = TRUE)

    if (model == "model_1")
      return_type <- "LIN"

    M_indicator <- spflow_indicators2mat(spflow_indicators)
    Y_hat <- switch(
        method,
        "LIN" = signal,
        "TC" =  {
          compute_expectation(
            signal_matrix = signal,
            DW = object@spflow_neighborhood$DW,
            OW = object@spflow_neighborhood$OW,
            rho = rho,
            model = model,
            M_indicator = M_indicator,
            approximate = approx_expectation,
            max_it = expectation_approx_order
          )},
        "TS" = {
          trend <- Reduce("+", Map("*", object@spflow_matrices$Y_[-1], rho))
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

  return(vec_format_d_o(
    mat = Y_hat,
    do_keys =  spflow_indicators,
    type =  return_type,
    name =  "PREDICTION"))
  })

# ---- ... preds --------------------------------------------------------------
#' #' @title Extract the vector of residuals values from a [spflow_model()]
#' #'
#' #' @param object A [spflow_model()]
#' #' @inheritParams fitted
#' #' @rdname spflow_model-class
#' #' @export
#' setMethod(
#'   f = "preds",
#'   signature = "spflow_model",
#'   function(object, type = "V") {
#'     return(vec_format_d_o(
#'       mat = object@resid,
#'       do_keys = object@spflow_matrices$do_keys,
#'       type = type,
#'       name = "RESID"))})


# ---- ... resid --------------------------------------------------------------
#' @title Extract the vector of residuals values from a [spflow_model()]
#'
#' @param object A [spflow_model()]
#' @inheritParams fitted
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "resid",
  signature = "spflow_model",
  function(object, type = "V") {
    do_k <- object@spflow_indicators
    do_k[["RESID"]] <- do_k[["FITTED"]] - do_k[["ACTUAL"]]
    spflow_indicators2format(do_k[,c(names(do_k)[1:2],"RESID")], type, do_k[["HAS_Y"]])
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
    return(object@estimation_diagnostics[["sd_error"]])
    })


# ---- ... show ---------------------------------------------------------------
#' @noRd
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

    cat("\n\n")
    cat(print_line(50))
    cat("\nCoefficients:\n")
    print(round(results(object)[,1:4], digits = 2),
          print.gap = 2L,
          quote = FALSE)

    cat("\n")
    cat(print_line(50))
    cat("\nR2_corr:", object@estimation_diagnostics[["R2_corr"]], collapse = " ")
    cat("\nObservations:", nobs(object), collapse = " ")

    pspace <- "Model coherence:"
    pspace_res <- object@estimation_diagnostics[[pspace]]
    if (!is.null(pspace_res))
      cat(sprintf("\n%s %s", pspace, pspace_res))

    invisible(object)
  })



# ---- ... varcov -------------------------------------------------------------
#' @param object A [spflow_model-class()]
#' @rdname spflow_model-class
setMethod(
  f = "varcov",
  signature = "spflow_model_varcov",
  function(object) return(object@estimation_diagnostics[["varcov"]]))


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
#' @param spflow_matrices The design matrix/matrices of the model
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
    estimation_diagnostics,
    spflow_indicators = NULL,
    spflow_neighborhood = NULL,
    spflow_data = NULL,
    spflow_matrices = NULL,
    spflow_moments = NULL) {

  model_class <- paste0("spflow_model_", estimation_control[["estimation_method"]])
  model_results <- new(
    model_class,
    estimation_results = estimation_results,
    estimation_control = estimation_control,
    estimation_diagnostics = estimation_diagnostics,
    spflow_indicators = spflow_indicators,
    spflow_data = spflow_data,
    spflow_matrices = spflow_matrices,
    spflow_moments = spflow_moments)
  return(model_results)
}

# ---- Helper functions -------------------------------------------------------
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

#' @keywords internal
vec_format_d_o <- function(mat, do_keys, type = "M", name = "FITTED") {

  if (type == "M")
    return(mat)

  is_cartesian <- nrow(do_keys) == length(mat)
  if (is_cartesian)
    vec <- as.vector(mat)

  if (!is_cartesian)
    vec <- mat[as.integer(rownames(do_keys))]

  if (type == "V")
    return(vec)

  do_keys[[name]] <- vec
  if (type == "OD")
    return(do_keys)

  stop('Agument type musst be equal to "V", "M", or "OD"!')
}

#' @keywords internal
actual <- function(object, type = "V"){
  do_k <- object@spflow_indicators
  spflow_indicators2format(do_k[,c(names(do_k)[1:2],"ACTUAL")], type, do_k[["HAS_Y"]])
}


