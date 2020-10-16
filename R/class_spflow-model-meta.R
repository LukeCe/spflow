#' @title
#' A S4 class that represent the results of a spatial interaction model
#'
#' @slot estimation_results data.frame.
#' @slot estimation_control list.
#' @slot N numeric.
#' @slot sd_error numeric.
#' @slot R2_corr numeric.
#' @slot resid maybe_numeric.
#' @slot fitted maybe_numeric.
#' @slot spatial_filter_matrix maybe_matrix.
#' @slot design_matrix
#'
#' @name spflow_model_meta
setClass("spflow_model_meta",
         slots = c(
           estimation_results = "data.frame",
           estimation_control = "list",
           N = "numeric",
           sd_error = "numeric",
           R2_corr = "maybe_numeric",
           resid = "maybe_numeric",
           fitted = "maybe_numeric",
           spatial_filter_matrix = "maybe_matrix",
           design_matrix = "ANY"))

# ---- Methods ----------------------------------------------------------------
#' @param model_matrices A list as returned by [spflow_model_matrix()]
#' @param flow_control A list as returned by [spflow_control()]
#'
#' @rdname add_details
#' @keywords  internal
setMethod(
  f = "add_details",
  signature = "spflow_model_meta",
  function(object, model_matrices, flow_control) { # ---- add_details ---------------------------------------

    object@design_matrix <- drop_instruments(model_matrices)

    # add coef names
    coef_names <- parameter_names(
      model_matrices = object@design_matrix,
      model_formulation = flow_control$formulation,
      model = flow_control$model)
    results(object) <- data.frame(results(object),
                                  ".names" = coef_names,
                                  row.names = ".names")

    # add fitted values , residuals, and goodness-of-fit
    nb_rho <- spatial_model_order(flow_control$model)
    mu <- coef(object)
    rho <- mu[seq_len(nb_rho)]
    delta <- mu[!names(mu) %in% names(rho)]

    fit_trend <- nb_rho
    if (nb_rho > 0) {
      fit_trend <-
        mapply(FUN = "*", model_matrices$Y[-1],rho,SIMPLIFY = FALSE) %>%
        Reduce("+", x = . ) %>%
        as.vector()
    }

    fit_signal <- compute_signal(model_matrices = object@design_matrix,
                                 delta = delta)

    object@fitted <- fit_trend + fit_signal
    object@resid <- as.vector(object@design_matrix$Y) - fitted(object)
    object@R2_corr <- stats::cor(fitted(object),
                                 as.vector(object@design_matrix$Y))^2

    return(object)
  })


#' @title Extract the coefficient vector from a spatial interaction model
#' @param object A [spflow_model()]
#' @export
setMethod(
  f = "coef",
  signature = "spflow_model_meta",
  function(object) { # ---- coef ----------------------------------------------
    results_df <- object@estimation_results
    return(lookup(results_df$est,rownames(results_df)))
  })


#' @title Extract a vector of fitted values from a spatial interaction model
#' @param object A [spflow_model()]
#' @export
setMethod(
  f = "fitted",
  signature = "spflow_model_meta",
  function(object) { # ---- fitted --------------------------------------------
    return(object@fitted)
  })

#' @title Access the number if observations of a spatial interaction model
#' @param object A [spflow_model()]
#' @export
setMethod(
  f = "nobs",
  signature = "spflow_model_meta",
  function(object) { # ---- nobs ----------------------------------------------
    return(object@N)
  })


#' @title Prediction methods for spatial interaction models
#'
#' @param object A [spflow_model()]
#' @param type A character declaring the type of prediction (for now only "BP")
#' @param ... Further arguments passed to the prediction function
#'
#' @rdname predict
#' @export
setMethod(
  f = "predict",
  signature = "spflow_model_meta",
  function(object, ..., type = "BP") { # ---- predict -------------------------------------------


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
      n_d <- nrow(DW) %||% (nobs(object) / nrow(OW))
      n_o <- nrow(OW) %||% (nobs(object) / nrow(DW))
      rho <- coef(object)[seq_len(nb_rho)]
      A <- expand_flow_neighborhood(DW = DW, OW = OW,
                                    n_o = n_o, n_d = n_d, model = model) %>%
        spatial_filter(., rho)

      # create the signal part of the fitted values
      delta <- coef(object)[-seq_len(nb_rho)]
      signal <- compute_signal(object@design_matrix, delta)
      fit_signal <- solve(A,signal)

      # TODO generelize flow extraction to handle vector and matrix
      # create the noise part of the fitted values
      observed_flows <- as.vector(object@design_matrix$Y)
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


#' @title Extract the vector of residuals values from a spatial interaction model
#'
#' @param object A [spflow_model()]
#' @export
setMethod(
  f = "resid",
  signature = "spflow_model_meta",
  function(object) { # ---- resid ---------------------------------------------
    return(object@resid)
  })

#' @rdname results
setMethod(
  f = "results",
  signature = "spflow_model_meta",
  function(object){ # ---- results --------------------------------------------
    return(object@estimation_results)
  })

#' @rdname results
#' @keywords internal
setReplaceMethod(
  f = "results",
  signature = "spflow_model_meta",
  function(object, value) { # ---- results <- ---------------------------------
    object@estimation_results <- value
    if (validObject(object))
      return(object)
  })

#' @rdname sd_error
#' @export
setMethod(
  f = "sd_error",
  signature = "spflow_model_meta",
  function(object){ # ---- sd_error -------------------------------------------
    return(object@sd_error)
  })

setMethod(
  f = "show",
  signature = "spflow_model_meta",
  function(object){ # ---- show -----------------------------------------------

    cntrl <- object@estimation_control
    cat(print_line(50))
    cat("\nSpatial interaction model estimated by:",
        cntrl$estimation_method ,collapse = " ")
    cat("\nAuto-correlation structure:",
        cntrl$model, ifelse(cntrl$use_sdm, "(SDM)", "(SLM)"),
        collapse = " ")
    cat("\nObservations:", nobs(object), collapse = " ")

    cat("\n\n")
    cat(print_line(50))
    cat("\nCoefficients:\n")
    print(round(results(object), digits = 2),
          print.gap = 2L,
          quote = FALSE)

    cat("\n")
    invisible(object)
  })

# ---- Constructors -----------------------------------------------------------

#' @title
#' Construct an S4 spflow_model class.
#'
#' @param estimation_results A data.frame of estimation results
#' @param estimation_control A list of control parameters
#' @param N A numeric indicating the number of obsevations
#' @param sd_error A numeric which reports the
#' @param R2_corr A numeric which reports a pseudo R^2 measure
#' @param resid A numeric vector of regression residuals
#' @param fitted A numeric vector of fitted values
#' @param spatial_filter_matrix A matrix which represents the spatial filter
#' @param design_matrix The design matrix/matrices of the model
#' @param ... Further arguments passed to more specific classes in accordance to the estimation method
#'
#' @importFrom methods slot<- slot
#' @export
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
  design_matrix = NULL) {

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
compute_signal <- function(model_matrices, delta) {

  matrix_formulation_case <- !is.matrix(model_matrices)

  if (matrix_formulation_case) {

    # index the coefficient vectors according to the model segments
    sub_index <- list(
      "const" = 1,
      "const_intra" = 1 - is.null(model_matrices$const_intra),
      "DX" = seq_len(ncol(model_matrices$DX) %||% 0) %|0|% 0,
      "OX" = seq_len(ncol(model_matrices$OX) %||% 0) %|0|% 0,
      "IX" = seq_len(ncol(model_matrices$IX) %||% 0) %|0|% 0,
      "G" = seq_len(length(model_matrices$G)) %|0|% 0) %>%
      sequentialize_index()

    # Calculate the components of the signal
    # missing components are set to zero and do not affect the final sum
    # number of destinations is required
    vector_or_null <- function(part_id) {
      model_matrices[[part_id]] %|!|%
        as.vector(model_matrices[[part_id]] %*%
                    delta[sub_index[[part_id]]])
    }
    n_d <- nrow(model_matrices$Y)

    # constant
    const <- delta[sub_index$const]

    # intra constant
    const_intra <- model_matrices$const_intra %|!|%
      Matrix::Diagonal(delta[sub_index$const_intra], n = n_d)
    const_intra <- const_intra %||% 0

    # destination part - (vector is recycled)
    dest <- vector_or_null("DX")
    dest <- dest %||% 0

    # origin part - vector is not recycled correctly ...
    orig <- vector_or_null("OX")
    orig <- orig %|!|% matrix(rep(orig,n_d),nrow = n_d,byrow = TRUE)
    orig <- orig %||% 0

    # intra part - only for diagonal elements
    intra <- vector_or_null("IX")
    intra <- intra %|!|% Matrix::Diagonal(intra,n = length(intra))
    intra <- intra %||% 0

    # G part - (origin-destination pair attributes)
    g_term <- 0
    if (length(model_matrices$G) != 0 ) {
      g_term <-
        mapply(FUN = "*", model_matrices$G, delta[sub_index$G],
               SIMPLIFY = FALSE) %>%
        Reduce(f = "+", x = .) %>%
        as.matrix()
    }

    signal <- as.vector(const + const_intra + dest + orig + intra + g_term)
    return(signal)
  }

  # vector formulation...
  stop("not yet implemented")

}
