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
#' @slot spflow_formula
#'   A formula
#' @slot spflow_networks
#'   A [spflow_network_multi-class()]
#' @slot spflow_matrices
#'   A list or NULL
#' @slot spflow_formula
#'   The formula used to fit the model
#' @slot spflow_indicators
#'   A data.frame containing the indicators of od-pairs
#' @slot spflow_moments
#'   A list of moment matrices used for estimating the model
#' @slot spflow_nbfunctions
#'   A list that may contain a function to calculate the log-determinant term
#'   and one to validate the parameter space for the spatial interaction model.
#'
#' @name spflow_model-class
#' @author Lukas Dargek
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
#' results(spflow_results_mcmc)
#' plot(mcmc_results(spflow_results_mcmc)) # parameter values during the mcmc sampling
setClass("spflow_model", slots = c(
  estimation_results = "data.frame",
  estimation_control = "list",
  estimation_diagnostics = "list",
  spflow_formula = "formula",
  spflow_networks = "maybe_spflow_network_multi",
  spflow_moments = "maybe_list",
  spflow_matrices = "maybe_list",
  spflow_indicators = "maybe_data.frame",
  spflow_nbfunctions = "maybe_list"))


setClass("spflow_model_ols", contains = "spflow_model")
setClass("spflow_model_mle", contains = "spflow_model")
setClass("spflow_model_s2sls", contains = "spflow_model")
setClass("spflow_model_mcmc", contains = "spflow_model")
setClassUnion("spflow_model_ll",
              c("spflow_model_ols",
                "spflow_model_mle"))
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
#' @rdname spflow_model-class
#' @param param_subset
#'  A character indicating the subset of model parameters to be returned "rho"
#'  relates to the autoregression parameters and "delta" to those of the
#'  exogenous variables.
#' @export
setMethod(
  f = "coef",
  signature = "spflow_model",
  function(object, param_subset = NULL) {

    results_df <- object@estimation_results
    coefs <- lookup(results_df$est,rownames(results_df))

    if (is.null(param_subset))
      return(coefs)

    nb_rho <- spatial_model_order(object@estimation_control$model)
    res <- NULL
    if ("rho" %in% param_subset & nb_rho > 0) {
      res <- c(res,coefs[seq_len(nb_rho)])
    }

    if ("delta" %in% param_subset) {
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
  signature = "spflow_model",
  function(object) return(object@estimation_diagnostics[["ll"]]))

# ---- ... mcmc_results -------------------------------------------------------
#' @rdname spflow_model-class
setMethod(
  f = "mcmc_results",
  signature = "spflow_model_mcmc",
  function(object) return(object@estimation_diagnostics[["mcmc_results"]]))

# ---- ... nobs ---------------------------------------------------------------
#' @title Access the number of observations inside a [spflow_model-class()
#' @param object A [spflow_model-class()]
#' @param which
#'   A character vector indicating the subset of observations to consider
#'   should be one of `c("fit", "cart", "pop", "pair", "orig", "dest")`.
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "nobs",
  signature = "spflow_model",
  function(object, which = "sample") {
    assert_valid_option(which, c("sample", "cart", "pop", "pair", "orig", "dest"))
    return(object@estimation_diagnostics[[paste0("N_", which)]])
  })


# ---- ... neighborhood -------------------------------------------------------
#' @title Access the origin or destination neighborhood of a spflow_model
#' @param object A spflow_model
#' @param which_nb
#'   A character vector: "OW" for origin- and "DW" for destination neighborhood
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "neighborhood",
  signature = "spflow_model",
  function(object, which_nb) {

    assert_valid_option(which_nb, c("OW", "DW"))
    if (is.null(object@spflow_networks))
      return(NULL)

    od_id <- id(object@spflow_networks@pairs[[1]])
    od_id <- od_id[ifelse(which_nb == "OW", "orig", "dest")]
    neighborhood(object@spflow_networks, od_id)
  })


# ---- ... pair_cor ----------------------------------------------------------
#' @param add_resid,add_fitted
#'   Logicals, indicating whether the model residuals and fitted value
#'   should be added to the correlation matrix
#' @param exploit_fit
#'   A logical, if `TRUE` the correlation that is generated as a byproduct of
#'   fitting the model is returned.
#'   Otherwise it is recreated from the input data, without considering the
#'   weights.
#' @inheritParams spflow_control
#' @rdname pair_cor
#' @export
#' @examples
#' # Used with a model...
#' gravity_ge <- spflow(
#'   y1 ~ . + P_(DISTANCE),
#'   multi_net_usa_ge,
#'   "ge_ge",
#'   spflow_control(model = "model_1"))
#'
#' cor_mat <- pair_cor(gravity_ge)
#' cor_image(cor_mat)
#'
setMethod(
  f = "pair_cor",
  signature = "spflow_model",
  function(object, add_fitted = TRUE, add_resid = TRUE, model, exploit_fit = TRUE) {

    assert_is_single_x(add_fitted, "logical")
    assert_is_single_x(add_resid, "logical")
    assert_is_single_x(exploit_fit, "logical")

    new_mat <- object@spflow_matrices
    new_mat[["CONST"]][["(Intercept)"]] <- 1
    new_mom <- object@spflow_moments
    keep_moments <- exploit_fit || is.null(object@estimation_control[["weight_variable"]])
    new_mat[["weights"]] <- NULL
    if (!keep_moments) {
      # for empiric version we drop the weights
      N <-  object@spflow_indicators
      N <- if (is.null(N[["IN_SAMPLE"]])) nrow(N) else sum(N[["IN_SAMPLE"]])
      new_mom <- derive_spflow_moments(
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

    cor_image(pair_cor(x))
    })


# ---- ... predict ------------------------------------------------------------
#' @title Prediction methods for the [spflow_model-class()]
#'
#' @description
#' The methods `predict()` and `predict_effect()` compute spatial predictions.
#' The former will return the predicted values of the dependent variables and
#' the later computes the change in its levels given the input data changes.
#'
#' @details
#' The prediction methods used here have been developed or analyzed by \insertCite{Goulard2017;textual}{spflow}.
#' \insertCite{Dargel2022;textual}{spflow} describe how they can be adapted to the case of interaction models.
#'
#' @param object A [spflow_model-class()]
#' @param method A character indicating which method to use for computing the
#'   predictions. Should be one of c("TS", "TC", "BP").
#' @param old_signal
#'   A matrix that can be supplied to specify the reference value for the signal.
#'   If not given the signal contained in the model is used.
#' @param approx_expectation
#'   A logical, if `TRUE` the expected value of the dependent variable is
#'   approximated by a Taylor series. For spatial models this can lead to
#'   significant performance gains.
#' @param expectation_approx_order
#'   A numeric, defining the order of the Taylor series approximation.
#' @param add_new_signal
#'   A logical, if `TRUE` the new signal is added to the as a column to
#'   the results. This only works when the return type is "OD".
#'
#' @inheritParams spflow_network_multi-class
#' @inheritParams spflow_model-class
#' @references \insertAllCited{}
#' @importFrom Matrix crossprod diag solve
#' @aliases predict,spflow_model-method predict_effect,spflow_model-method
#' @rdname predict
#' @return Predicted values in the format specified by the argument return_type.
#' @export
setMethod(
  f = "predict",
  signature = "spflow_model",
  function(object,
           new_dat = NULL,
           method = "BPA",
           approx_expectation = TRUE,
           expectation_approx_order = 10,
           return_type = "OD",
           add_new_signal = FALSE) {


    insample_methods <- c("TS","BPI","TCI")
    fullpop_methods <- c("BPA","TC","BP")
    assert_valid_option(method, c(insample_methods, fullpop_methods))
    assert(is.null(new_dat) | method %in% fullpop_methods,
           "In sample methods are invalid when new data is supplied!")


    # prepare the data and coefficients....
    model <- object@estimation_control$model
    rho <- coef(object, "rho")
    delta <- coef(object, "delta")

    if (is.null(new_dat)) {
      spflow_indicators <- object@spflow_indicators
      spflow_matrices <- object@spflow_matrices
    }

    if (!is.null(new_dat)) {
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
#' @name predict_effect
#' @rdname predict
#' @export
setMethod(
  f = "predict_effect",
  signature = "spflow_model",
  function(object,
           new_dat,
           old_signal = NULL,
           approx_expectation = TRUE,
           expectation_approx_order = 10,
           return_type = "OD") {

    # prepare the data and coefficients....
    model <- object@estimation_control$model
    rho <- coef(object, "rho")
    delta <- coef(object, "delta")

    new_networks <- update_dat(object@spflow_networks, new_dat)
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
#' @title Extract the vector of residuals values from a [spflow_model-class()]
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
  function(object,
           as_column = FALSE,
           sig_levels = c("***" = .001, "**" = 0.01, "*" = 0.05, "'" = 0.1),
           global_vars = c( "model_coherence", "N_sample", "R2_corr", "ll"),
           digits = 3,
           add_dispersion = FALSE
           ){

    if (!as_column)
      return(object@estimation_results)


    rd <- function(x) round(x, digits)
    res <- object@estimation_results
    res_col <- data.frame("T" = "param", "M" = rd(res$est), row.names = rownames(res))

    if (!is.null(sig_levels)) {
      assert(is.numeric(sig_levels) &&
               all(sig_levels >= 0) && all(sig_levels <= 1) &&
               !is.null(names(sig_levels)),
             "The argument sig_levels must be a named numeric
              with values between 0 and 1!")

      c_breaks <- c(0,as.numeric(sig_levels),1)
      c_labels <- c(names(sig_levels),"")
      stars <- cut(pmin(res[["p.val"]], .9999999),c_breaks,c_labels,include.lowest = TRUE)
      res_col[["M"]] <- paste0(res_col[["M"]], stars)
    }

    if (add_dispersion)
      res_col[["M"]] <- sprintf("%s\n(%s)", res_col[["M"]], rd(res$sd))

    if (!is.null(global_vars)) {
      assert_is(global_vars, "character")
      global_vars <- c(
        object@estimation_control[global_vars],
        object@estimation_diagnostics[global_vars])
      global_vars <- Filter(function(x) length(x) == 1, global_vars)
      global_vars <- lapply(global_vars, function(x) as.character(ifelse(is.numeric(x), rd(x), x)))
      global_vars <- data.frame("T" = "global", "M" = unlist(global_vars))
      res_col <- rbind(res_col, global_vars)
    }

    return(res_col)
  })


# ---- ... results (for mcmc) -------------------------------------------------
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "results",
  signature = "spflow_model_mcmc",
  function(object,
           as_column = FALSE,
           sig_levels = c("***" = .001, "**" = 0.01, "*" = 0.05, "'" = 0.1),
           global_vars = c( "model_coherence", "N_sample", "R2_corr"),
           digits = 3,
           add_dispersion = FALSE
  ){

    if (!as_column)
      return(object@estimation_results)

    rd <- function(x) round(x, digits)
    res <- object@estimation_results
    res_col <- data.frame("T" = "param", "M" = rd(res$est), row.names = rownames(res))

    if (length(sig_levels) > 0) {
      assert(is.numeric(sig_levels) &&
               all(sig_levels >= 0) && all(sig_levels <= 1) &&
               !is.null(names(sig_levels)),
             "The argument sig_levels must be a named numeric
              with values between 0 and 1!")

      mcmc_res <- seq(object@estimation_control$mcmc_burn_in)
      mcmc_res <- mcmc_results(object)[-mcmc_res,]
      mcmc_res <- mcmc_res[,-ncol(mcmc_res)]

      has_sig_level <- function(x, lv) outside(0, quantile(x, c(lv/2, 1-lv/2)))
      mcmc_sig_stars <- structure(rep("", nrow(res_col)), names = rownames(res_col))
      sig_levels <- sort(sig_levels,decreasing = TRUE)
      for (i in seq_along(sig_levels)) {
        mcmc_sig <- apply(mcmc_res, 2, has_sig_level, sig_levels[i])
        mcmc_sig_stars[mcmc_sig] <- names(sig_levels[i])
      }
      res_col[["M"]] <- paste0(res_col[["M"]], mcmc_sig_stars)
    }

    if (add_dispersion)
      res_col[["M"]] <- sprintf("%s\n[%s;%s]", res_col[["M"]], rd(res$quant_025), rd(res$quant_975))

    if (!is.null(global_vars)) {
      assert_is(global_vars, "character")
      global_vars <- c(
        object@estimation_control[global_vars],
        object@estimation_diagnostics[global_vars])
      global_vars <- Filter(function(x) length(x) == 1, global_vars)
      global_vars <- lapply(global_vars, function(x) as.character(ifelse(is.numeric(x), rd(x), x)))
      global_vars <- data.frame("T" = "global", "M" = unlist(global_vars))
      res_col <- rbind(res_col, global_vars)
    }

    return(res_col)
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

# ---- ... results_flat -------------------------------------------------------
#' @rdname spflow_model-class
#' @param coef_info A character indicating column names in the results
#' @param main_info A character indicating named elements in the estimation_control or estimation_diagnostics
#' @export
setMethod(
  f = "results_flat",
  signature = "spflow_model",
  function(object,
           coef_info = c("est","sd"),
           main_info = c("estimation_method", "model_coherence", "R2_corr", "ll","sd_error")){

    res <- results(object)
    coef_info <- Filter(function(x) x %in% names(res),coef_info)
    flat_param <- lapply(coef_info, function(.col) {
      tmp <- suffix_columns(t(res[.col]), paste0("_",.col))
      data.frame(tmp, row.names = NULL,check.names = FALSE)
    })

    main_info <- c(object@estimation_control[main_info], object@estimation_diagnostics[main_info])
    main_info <- Filter(function(x) length(x) == 1, main_info)
    flat_results <- cbind(as.data.frame(main_info), flat_param)
    return(flat_results)
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
    cat(sprintf("\nDependent variable: %s",
                names(object@spflow_matrices$Y_)[1]))


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

    pspace_res <- object@estimation_diagnostics[["model_coherence"]]
    if (!is.null(pspace_res))
      cat(sprintf("\nModel coherence: %s", pspace_res))

    invisible(object)
  })


# ---- ... spflow_map ---------------------------------------------------------
#' @param flow_type
#'   A character indicating what to values to show.
#'   Should be one of c("resid", "fitted", "actual").
#' @param add_title
#'   A logical, if `TRUE` the flow_type is added as title.
#' @rdname spflow_map
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
    assert_valid_option(flow_type, names(type_options))

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
#' @inheritParams spflow_control
#' @rdname spflow_moran_plots
#' @export
#' @examples
#'
#'  # Used with a spflow_model...
#'  # Check the if there is spatial correlation in the residuals
#'  gravity_ge <- spflow(
#'    y9 ~ . + P_(DISTANCE),
#'    multi_net_usa_ge,
#'    "ge_ge",
#'    spflow_control(model = "model_1"))
#'
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
#' @rdname spflow_model-class
#' @export
setMethod(
  f = "varcov",
  signature = "spflow_model_varcov",
  function(object) return(object@estimation_diagnostics[["varcov"]]))


# ---- Constructors -----------------------------------------------------------
#' @title Internal function to construct a [spflow_model-class()]
#'
#' @param ...
#'   Further arguments passed to more specific classes in accordance to the
#'   estimation method
#'
#' @importFrom methods slot<- slot
#' @keywords internal
#' @noRd
spflow_model <- function(
    ...,
    estimation_results,
    estimation_control,
    estimation_diagnostics,
    spflow_networks = NULL,
    spflow_indicators = NULL,
    spflow_matrices = NULL,
    spflow_moments = NULL,
    spflow_nbfunctions = NULL) {

  model_class <- paste0("spflow_model_", estimation_control[["estimation_method"]])
  model_results <- new(
    model_class,
    estimation_results = estimation_results,
    estimation_control = estimation_control,
    estimation_diagnostics = estimation_diagnostics,
    spflow_networks = spflow_networks,
    spflow_indicators = spflow_indicators,
    spflow_matrices = spflow_matrices,
    spflow_moments = spflow_moments,
    spflow_nbfunctions = spflow_nbfunctions)
  return(model_results)
}

# ---- Helper functions -------------------------------------------------------
#' @keywords internal
create_results <- function(..., df = 1) {

  r <- list(...)
  if (is.null(r[["est"]]))
    stop("Argument est for estimates is required!")

  if (is.null(r[["sd"]]))
    stop("Argument sd for standard deviation is required!")

  if (is.null(r[["t.stat"]]))
    r <- c(r, list("t.stat" = r[["est"]] / r[["sd"]]))

  if (is.null(r[["p.val"]]))
    r <- c(r, list("p.val" = 2 * pt(abs(r[["t.stat"]]), max(df,1e-10), lower.tail = FALSE)))

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

#' @keywords internal
outside <- function(x, bounds) {
    bounds <- range(bounds)
  x > bounds[2] | x < bounds[1]
}


#' @keywords internal
compare_results <- function(model_list, global_vars = c("model_coherence", "R2_corr", "ll", "N_sample")) {

  res <- lapply(model_list, results, as_column = TRUE, global_vars = global_vars)

  pnames <- lapply(res, function(x) rownames(x[x[["T"]] == "param",]))
  pnames <- sort(unique(unlist(pnames)))
  rhos <- intersect(paste0("rho_", c("d","o","w","od","odw")),pnames)
  pnames <- c(rhos , setdiff(pnames, rhos))

  gnames <- lapply(res, function(x) rownames(x[x[["T"]] == "global",]))
  gnames <- unique(unlist(gnames))
  pnames <- c(pnames, gnames)

  mnames <- sprintf("Fit (%s)", seq_along(res))
  if (!is.null(names(res)))
    mnames <- names(res)

  res_mat <- matrix(
    "", ncol = length(mnames), nrow = length(pnames),
    dimnames = list(pnames, mnames))
  for (i in seq_along(res))
    res_mat[rownames(res[[i]]), i] <- res[[i]][["M"]]

  return(as.data.frame(res_mat))
}
