#' @include class_generics_and_maybes.R class_spflow_network_multi.R

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
#' method but most behaviors and data are identical among them.
#'
#' @slot estimation_results
#'   A data.frame that contains the main [results()] of the estimation
#' @slot estimation_control
#'   A list that contains all control parameters of the estimation
#'   (see [spflow_control()])
#' @slot estimation_diagnostics
#'   A list of further indicators about the estimation
#' @slot spflow_matrices A list or NULL
#' @slot spflow_data
#'   A list containing the data.frames for origins, destinations, and od-pairs
#' @slot spflow_formula
#'   The formula used to fit the model
#' @slot spflow_indicators
#'   A data.frame containing the indicators of od-pairs
#' @slot spflow_moments
#'   A list of moment matrices used for estimating the model
#' @slot spflow_neighborhood
#'   The neighborhood matrices for origins and destinations
#'
#' @name spflow_model-class
#' @aliases spflow_model_mcmc, spflow_model_mle, spflow_model_s2sls, spflow_model_ols
#' @seealso [spflow()], [spflow_network_classes()]
#' @export
#' @examples
#'
#' spflow_results <- spflow(y9 ~ . + P_(DISTANCE), multi_net_usa_ge)
#'
#' # General methods
#' results(spflow_results) # data.frame of main results
#' coef(spflow_results) # vector of estimated coefficients
#' fitted(spflow_results) # vector of fitted values
#' resid(spflow_results) # vector of residuals
#' nobs(spflow_results) # number of observations
#' sd_error(spflow_results) # standard deviation of the error term
#' predict(spflow_results) # computation of the in sample predictor
#' plot(spflow_results) # some plots for assessing the model
#'
#' # MLE methods
#' logLik(spflow_results) # value of the likelihood function
#'
#' # MLE, OLS and S2SLS methods
#' varcov(spflow_results) # variance covariance matrix of the estimators
#'
#' # MCMC methods
#' spflow_results_mcmc <- spflow(
#'   y2 ~ . + P_(DISTANCE),
#'   multi_net_usa_ge,
#'   estimation_control = spflow_control(estimation_method = "mcmc",
#'                                 model = "model_2"))
#' results(spflow_results)
#' mcmc_results(spflow_results_mcmc) # parameter values during the mcmc sampling
#'
setClass("spflow_model", slots = c(
  estimation_results = "data.frame",
  estimation_control = "list",
  estimation_diagnostics = "list",
  spflow_networks = "maybe_spflow_network_multi",
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

    if (is.null(object@spflow_networks))
      return(NULL)
    get_od_coords(object@spflow_networks)
  })

# ---- ... fitted -------------------------------------------------------------
#' @title Extract a vector of fitted values from a spatial interaction model
#' @param object A [spflow_model-class()]
#' @param return_type
#'  A character indicating the format of the returned values:
#'  -  "V" leads to an atomic vector
#'  -  "M" leads to a OD matrix where missing data is replaced by zeros
#'  -  "OD" leads to a data.frame with columns being the the values
#'      and the id's of the destinations and the origins
#'
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "fitted",
  signature = "spflow_model",
  function(object, return_type = "V") {
    do_k <- object@spflow_indicators
    spflow_indicators2format(do_k[,c(names(do_k)[1:2],"FITTED")], return_type, do_k[["IN_SAMPLE"]])
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
#' @rdname spflow_model-class
setMethod(
  f = "mcmc_results",
  signature = "spflow_model_mcmc",
  function(object) return(object@estimation_diagnostics[["mcmc_results"]]))

# ---- ... nobs ---------------------------------------------------------------
#' @title Access the number of observations inside a [spflow_model]
#' @param object A [spflow_model()]
#' @param which
#'   A character vector indicating the subset of observations to consider
#'   should be one of `c("fit", "cart", "pop", "pair", "orig", "dest")`.
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "nobs",
  signature = "spflow_model",
  function(object, which = "sample") {
    assert_valid_case(which, c("sample", "cart", "pop", "pair", "orig", "dest"))
    return(object@estimation_diagnostics[[paste0("N_", which)]])
  })


# ---- ... neighborhood -------------------------------------------------------
#' @title Access the origin or destination neighborhood of a spflow_model
#' @param object A [spflow_model()]
#' @param which
#'   A character vector: "OW" for origin- and "DW" for destination neighborhood
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "neighborhood",
  signature = "spflow_model",
  function(object, which) {

    assert_valid_option(which, c("OW", "DW"))
    if (is.null(object@spflow_networks))
      return(NULL)

    od_id <- id(object@spflow_networks@pairs[[1]])
    od_id <- od_id[ifelse(which == "OW", "orig", "dest")]
    neighborhood(object@spflow_networks, od_id)
  })


# ---- ... pair_corr ----------------------------------------------------------
#' @param type
#'   A character, that should indicate one of `c("fit", "empiric")`
#'   - "fit" will use the moments that have been used during the estimation
#'   - "empric" will recompute these moments ignoring the weights
#' @param add_resid,add_fitted
#'   A logical, indicating whether the model residuals and fitted value
#'   should be added to the correlation matrix
#' @param model
#'  A character that should indicate one of `paste0("model_", 1:9)`.
#'  The option specifies different correlation structures that are detailed in
#'  the help page of [spflow_control()]
#'
#' @rdname pair_corr
#' @export
#' @examples
#'
#'  # Used with a model...
#'  gravity_ge <- spflow(
#'    y1 ~ . + P_(DISTANCE),
#'    multi_net_usa_ge,
#'    "ge_ge",
#'    spflow_control(model = "model_1"))
#'
#'  corr_mat <- pair_corr(gravity_ge)
#'  corr_map(corr_mat)
#'
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
      # for empiric version we drop the weights
      N <-  object@spflow_indicators
      N <- if (is.null(N[["IN_SAMPLE"]])) nrow(N) else sum(N[["IN_SAMPLE"]])
      new_mom <- compute_spflow_moments(
        spflow_matrices = new_mat,
        n_o = new_mom[["n_o"]],
        n_d = new_mom[["n_d"]],
        N = N,
        na_rm = TRUE)
    }

    if (!add_resid & !add_fitted)
      return(new_mom$TCORR)

    if (missing(model))
      model <- object@estimation_control[["model"]]

    # recompute the moments pretending the errors are the flows
    # with flows becoming exogenous variables
    new_mat[["P_"]] <- c(new_mat[["P_"]], new_mat[["Y_"]])
    new_mat[["Y_"]] <- NULL


    if (add_fitted) {
      new_mat[["Y_"]] <- list(fitted(object, "M"))
      names(new_mat[["Y_"]]) <- paste0("FITTED_", names(object@spflow_matrices[["Y_"]])[1])
    }

    M_indicator <- spflow_indicators2mat(object@spflow_indicators)
    if (add_resid) {
      new_mat[["Y_"]] <- c(
        new_mat[["Y_"]],
        lag_flow_matrix(
          Y = resid(object, "M"),
          model = model,
          OW = neighborhood(object, "OW"),
          DW = neighborhood(object, "DW"),
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
#' @importFrom graphics abline image.default par title
#' @importFrom stats aggregate complete.cases lm.fit qnorm qqline qqnorm
#' @param x A [spflow_model-class]
#' @param ... Arguments passed on to other plotting functions
#' @param y not used
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
         asp = 1,
         main = "Actual vs Fitted")
    abline(lm.fit(cbind(1,fitted_x), actual_x), col = "red") ; abline(a = 0, b = 1)


    coords_s <- list(...)[["coords_s"]] %||% coord(x)
    if (!is.null(coords_s)) {
      x_or_25_percent <- min(50,nobs(x) / 4)
      keep_x_at_most <- (nobs(x) - x_or_25_percent)  / nobs(x)
      spflow_map(x, coords_s = coords_s, flow_type = "fitted", filter_lowest = keep_x_at_most, legend = "bottomright")
      spflow_map(x, coords_s= coords_s, flow_type = "resid", filter_lowest = keep_x_at_most, legend = "bottomright")
    }

    if (!inherits(x, "spflow_model_ols"))
      spflow_moran_plots(x)

    if (inherits(x, "spflow_model_mcmc"))
      plot(mcmc_results(x),density = FALSE, ask = FALSE)

    corr_map(pair_corr(x))
    })


# ---- ... predict ------------------------------------------------------------
#' @title Prediction methods for spatial interaction models
#' @param object A [spflow_model()]
#' @param method A character indicating which method to use for computing the
#'   predictions. Should be one of c("TS", "TC", "BP").
#' @param new_data An object containing new data (to be revised)
#' @param approx_expectation
#'   A logical, if `TRUE` the expected value of the dependent variable is
#'   approximated by a Taylor series. For spatial models this can lead to
#'   significant performance gains.
#' @param expectation_approx_order
#'   A numeric, defining the order of the Taylor series approximation.
#' @param return_type A character indicating which format the return values should
#'   have: "V" for vector, "M" for matrix, "OD" for data.frame
#'   predictions. Should be one of c("TS", "TC", "BP").
#' @param add_new_signal
#'   A logical, if `TRUE` the new signal is added to the as a column to
#'   the results. This only works when the return type is "OD".
#' @param ... Further arguments passed to the prediction function
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
           method = "BPA",
           approx_expectation = TRUE,
           expectation_approx_order = 10,
           return_type = "OD",
           add_new_signal = FALSE) {


    insample_methods <- c("TS","BPI","TCI")
    fullpop_methods <- c("BPA","TC","BP")
    assert_valid_option(method, c(insample_methods, fullpop_methods))
    assert(is.null(new_data) | method %in% fullpop_methods,
           "In sample methods are invalid when new data is supplied!")


    # prepare the data and coefficients....
    model <- object@estimation_control$model
    rho <- coef(object, "rho")
    delta <- coef(object, "delta")

    if (is.null(new_data)) {
      spflow_indicators <- object@spflow_indicators
      spflow_matrices <- object@spflow_matrices
    }

    if (!is.null(new_data)) {
      stop("Data update not yet implmented!")

    }

    # for in-sample methods the population is set equal to the sample
    filter_case <- ifelse(method %in% insample_methods, "IN_SAMPLE", "IN_POP")
    filter_case <- spflow_indicators[[filter_case]] %||% TRUE
    population_indicators <- spflow_indicators[filter_case,,drop = FALSE]
    M_indicator <- spflow_indicators2mat(population_indicators)

    # the signal (= Z %*% delta) is always required
    signal <- compute_signal(
      delta = delta,
      spflow_matrices = spflow_matrices,
      spflow_indicators = population_indicators,
      keep_matrix_form = TRUE)
    n_o <- ncol(signal)
    n_d <- nrow(signal)

    if (model == "model_1")
      method <- "LM"

    if (method %in% c("TC","TCI","BP","BPA")) {
      Y_TC <- compute_expectation(
        signal_matrix = signal,
        DW = neighborhood(object, "DW"),
        OW = neighborhood(object, "OW"),
        rho = rho,
        model = model,
        M_indicator = M_indicator,
        approximate = approx_expectation,
        max_it = expectation_approx_order)
    }

    # compute the prediction according to the chosen method
    if (method == "LM")
      Y_hat <- signal

    if (method == "TS") {
      trend <- Reduce("+", Map("*", object@spflow_matrices$Y_[-1], rho))
      Y_hat <- trend + signal
    }

    if (method == "BPI") {

      res_ts <- Reduce("+", Map("*", object@spflow_matrices$Y_, c(1, -rho)))
      res_ts <- res_ts - signal
      res_ts <- filter_flow_matrix(
        Y = res_ts,
        model = model,
        DW = neighborhood(object, "DW") %|!|% t,
        OW = neighborhood(object, "OW") %|!|% t,
        M_indicator = M_indicator,
        rho = rho)

      diag_AA <- compute_diag_precision_mat(
        DW = neighborhood(object, "DW"),
        OW = neighborhood(object, "OW"),
        rho = rho,
        n_o = n_o,
        n_d = n_d,
        M_indicator = M_indicator)

      Y_hat <- object@spflow_matrices$Y_[[1]] - (res_ts/diag_AA)
    }

    if (method %in% c("TC","TCI"))
      Y_hat <- Y_TC

    if (method %in% c("BPA","BP")) {

      # the methods use an additive  correction terms on the Y_TC predictor...
      corig_tc <- object@spflow_matrices$Y_[[1]] - Y_TC
      if (!is.null(M_indicator))
        corig_tc <- corig_tc * M_indicator


      corig_tc <- filter_flow_matrix(
        Y = corig_tc,
        model = model,
        DW = neighborhood(object, "DW") %|!|% t,
        OW = neighborhood(object, "OW") %|!|% t,
        M_indicator = M_indicator,
        rho = rho)
      corig_tc <- filter_flow_matrix(
        Y = corig_tc,
        model = model,
        DW = neighborhood(object, "DW"),
        OW = neighborhood(object, "OW"),
        M_indicator = M_indicator,
        rho = rho)

      if (method == "BPA") {
        diag_AA <- compute_diag_precision_mat(
          DW = neighborhood(object, "DW"),
          OW = neighborhood(object, "OW"),
          rho = rho,
          n_o = n_o,
          n_d = n_d,
          M_indicator = M_indicator)
        Y_hat <- Y_TC - (corig_tc/diag_AA)
      }

      if (method == "BP") {
        A <- spatial_filter(
          weight_matrices = expand_spflow_neighborhood(
            DW = neighborhood(object, "DW"),
            OW = neighborhood(object, "OW"),
            M_indicator = M_indicator,
            model = model),
          autoreg_parameters = rho)

        if (is.null(M_indicator))
          Y_hat <- matrix(as.vector(Y_TC) - solve(crossprod(A), as.vector(corig_tc)),n_d, n_o)

        if (!is.null(M_indicator)) {
          pop_index <- spflow_indicators2pairindex(population_indicators)
          Y_hat <- as.vector(Y_TC[pop_index]) - solve(crossprod(A), as.vector(corig_tc[pop_index]))
          Y_hat <- spflow_indicators2mat(population_indicators, do_values = Y_hat)
        }
      }
    }

    result <- spflow_mat2format(
      mat = Y_hat,
      do_keys =  population_indicators,
      return_type =  return_type,
      name =  "PREDICTION")

    if (return_type == "OD") {
      result2 <- cbind(spflow_indicators, PREDICTION = NA_real_)
      result2[filter_case, "PREDICTION"] <- result[,"PREDICTION"]

      if (add_new_signal) {
        result2 <- cbind(result2, NEW_SIGNAL = NA_real_)
        result2[filter_case, "NEW_SIGNAL"] <- signal[spflow_indicators2pairindex(population_indicators)]
      }

      return(result2)
    }
    return(result)
  })


# ---- ... predict_effect -----------------------------------------------------
#' @title Prediction methods for spatial interaction models
#' @param object A [spflow_model()]
#' @param method A character indicating which method to use for computing the
#'   predictions. Should be one of c("TS", "TC", "BP").
#' @param new_data An object containing new data (to be revised)
#' @param old_signal A matrix that can be supplied to specify the reference
#'   value for the signal.
#'   If not given the signal contained in the model is used.
#' @param approx_expectation
#'   A logical, if `TRUE` the expected value of the dependent variable is
#'   approximated by a Taylor series. For spatial models this can lead to
#'   significant performance gains.
#' @param expectation_approx_order
#'   A numeric, defining the order of the Taylor series approximation.
#' @param return_type A character indicating which format the return values should
#'   have: "V" for vector, "M" for matrix, "OD" for data.frame
#'   predictions. Should be one of c("TS", "TC", "BP").
#'   predictions
#' @param ... Further arguments passed to the prediction function
#'
#' @importFrom Matrix crossprod diag solve
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "predict_effect",
  signature = "spflow_model",
  function(object,
           ...,
           new_data,
           old_signal = NULL,
           approx_expectation = TRUE,
           expectation_approx_order = 10,
           return_type = "OD") {

    # prepare the data and coefficients....
    model <- object@estimation_control$model
    rho <- coef(object, "rho")
    delta <- coef(object, "delta")

    new_networks <- update_dat(object@spflow_networks, new_data)
    new_matrices <- derive_spflow_matrices(
      id_spflow_pairs = names(new_networks@pairs)[1],
      spflow_networks = new_networks,
      spflow_formula = object@spflow_formula,
      spflow_control = object@estimation_control,
      na_rm = object@estimation_control[["na_rm"]])
    new_signal <- compute_signal(
      delta = delta,
      spflow_matrices = new_matrices,
      spflow_indicators = new_matrices[["spflow_indicators"]],
      keep_matrix_form = TRUE)

    if (is.null(old_signal))
      old_signal <- spflow_indicators2mat(object@spflow_indicators,do_filter = "IN_POP",do_values = "SIGNAL")
    assert(all(dim(new_signal) == dim(old_signal)),
           "The old_signal must be a matrix with dimensions %s x %s!", nrow(new_signal), ncol(new_signal))

    diff_effect <- compute_expectation(
      signal_matrix = new_signal - old_signal,
      DW = neighborhood(object, "DW"),
      OW = neighborhood(object, "OW"),
      rho = rho,
      model = model,
      M_indicator = spflow_indicators2mat(new_matrices[["spflow_indicators"]]),
      approximate = approx_expectation,
      max_it = expectation_approx_order)

    result <- spflow_mat2format(
      mat = diff_effect,
      do_keys =  new_matrices[["spflow_indicators"]],
      return_type =  return_type,
      name =  "DIFF_EFFECT")
    return(result)
  })

# ---- ... resid --------------------------------------------------------------
#' @title Extract the vector of residuals values from a [spflow_model()]
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "resid",
  signature = "spflow_model",
  function(object, return_type = "V") {
    do_k <- object@spflow_indicators
    do_k[["RESID"]] <- do_k[["FITTED"]] - do_k[["ACTUAL"]]
    spflow_indicators2format(do_k[,c(names(do_k)[1:2],"RESID")], return_type, do_k[["IN_SAMPLE"]])
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


# ---- ... spflow_map ---------------------------------------------------------
#' @param object
#'   A [spflow_model-class()]
#' @param flow_type
#'   A character indicating what to values to show.
#'   Should be one of c("resid", "fitted", "actual").
#' @param add_title
#'   A logical, if `TRUE` the flow_type is added as title.
#' @param ... further arguments passed to [map_flows()]
#' @rdname spflow_map
#' @seealso [map_flows()]
#' @export
#' @examples
#'
#'  # Used with a model...
#'  gravity_ge <- spflow(
#'    y1 ~ . + P_(DISTANCE),
#'    multi_net_usa_ge,
#'    "ge_ge",
#'    spflow_control(model = "model_1"))
#'  spflow_map(gravity_ge)
#'  spflow_map(gravity_ge, flow_type = "fitted")
#'  spflow_map(gravity_ge, flow_type = "actual")
#'
setMethod(
  f = "spflow_map",
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


# ---- ... spflow_moran_plots  ------------------------------------------------
#' @param DW,OW
#'   A matrix to replace the neighborhood of the destinations (DW) and origins (OW).
#'   Defaults to the one supplied to the model.
#' @param add_lines
#'   A logical, if `TRUE` regression lines are added to the Moran scatter plots.
#' @rdname spflow_moran_plots
#' @examples
#'
#'  # Used with a model...
#'  # To check the if there is spatial correlation in the residuals
#'  gravity_ge <- spflow(
#'    y9 ~ . + P_(DISTANCE),
#'    multi_net_usa_ge,
#'    "ge_ge",
#'    spflow_control(model = "model_1"))
#'  spflow_moran_plots(gravity_ge)
#'
setMethod(
  f = "spflow_moran_plots",
  signature = "spflow_model",
  function(object, model = "model_9", DW, OW, add_lines = TRUE) {

    if (missing(DW)) DW <- neighborhood(object, "DW")
    if (missing(OW)) OW <- neighborhood(object, "OW")

    assert(inherits(DW, "maybe_Matrix"), "The DW argument musst be of class Matrix or NULL!")
    assert(inherits(OW, "maybe_Matrix"), "The OW argument musst be of class Matrix or NULL!")
    assert(!all(is.null(DW), is.null(OW)), "At least one neighborhood matrix musst be given!")

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

    E_ <- lapply(E_, spflow_mat2format, do_keys = object@spflow_indicators, return_type = "V")
    E_1 <- cbind(1,E_[[1]])
    for (i in seq_len(length(E_) - 1)) {
      ii <- sub("RESID.",replacement = "", names(E_)[i + 1])


      title_expr <- bquote(paste("Moran scatterplot of residuals (",  W[.(ii)], " - lag)"))
      plot(y = E_[[i + 1]], x = E_[[1]],
           main = title_expr,
           xlab = expression(residual),
           ylab = bquote(W[.(ii)] %.% "resdiual"),
           asp = 1)
      if (add_lines)
        abline(lm.fit(x = E_1 , y = E_[[i + 1]]), col = "red") ; abline(0,0)
    }
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
#' @param estimation_control A list of control parameters
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
    spflow_networks = NULL,
    spflow_indicators = NULL,
    spflow_matrices = NULL,
    spflow_moments = NULL) {

  model_class <- paste0("spflow_model_", estimation_control[["estimation_method"]])
  model_results <- new(
    model_class,
    estimation_results = estimation_results,
    estimation_control = estimation_control,
    estimation_diagnostics = estimation_diagnostics,
    spflow_networks = spflow_networks,
    spflow_indicators = spflow_indicators,
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
actual <- function(object, return_type = "V"){
  do_k <- object@spflow_indicators
  spflow_indicators2format(do_k[,c(names(do_k)[1:2],"ACTUAL")], return_type, do_k[["IN_SAMPLE"]])
}


