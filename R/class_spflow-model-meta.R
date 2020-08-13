#' @include class_virtual.R

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
#' @return
#' @name spflow_model_meta
#' @export
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

# ---- generics & methods -----------------------------------------------------

#' @export
setMethod(
  f = "coef",
  signature = "spflow_model_meta",
  function(object) { # ---- coef ----------------------------------------------
    results_df <- object@estimation_results
    return(lookup(results_df$est,rownames(results_df)))
  })


#' @export
setMethod(
  f = "fitted",
  signature = "spflow_model_meta",
  function(object) { # ---- fitted --------------------------------------------
    return(object@fitted)
  })

#' @export
setMethod(
  f = "nobs",
  signature = "spflow_model_meta",
  function(object) { # ---- nobs ----------------------------------------------
    return(object@N)
  })

#' @export
setMethod(
  f = "resid",
  signature = "spflow_model_meta",
  function(object) { # ---- resid ---------------------------------------------
    return(object@resid)
  })

#' @title Access results of an estimation
#' @param object spflow_model_meta
#' @name results
#' @export
setGenericVerif("results",  function(object, ...){ # ---- results -------------
  standardGeneric("results")})

#' @param object spflow_model_meta
#' @name results
#' @export
setGenericVerif("results<-",  function(object, ...){ # ---- results <- --------
  standardGeneric("results<-")})

#' @export
#' @rdname results
setMethod(
  f = "results",
  signature = "spflow_model_meta",
  function(object){ # ---- results --------------------------------------------
    return(object@estimation_results)
  })

#' @export
#' @rdname id
setReplaceMethod(
  f = "results",
  signature = "spflow_model_meta",
  function(object, value) { # ---- results <- ---------------------------------
    object@estimation_results <- value
    if (validObject(object))
      return(object)
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

# ---- constructors -----------------------------------------------------------

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
#' @return
#' @export
spflow_model_s4 <- function(
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


