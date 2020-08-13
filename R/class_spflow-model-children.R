#' @include class_spflow-model-meta.R

# ---- MLE class --------------------------------------------------------------

#' @slot ll numeric.
#' @slot AIC numeric.
#' @slot BIC numeric.
#' @slot varcov matrix.
#'
#' @return
#' @rdname spflow_model_meta
#' @export
setClass("spflow_model_mle",
         slots = c(
           ll = "maybe_numeric",
           AIC = "maybe_numeric",
           BIC = "maybe_numeric",
           varcov = "maybe_matrix"),
         contains = "spflow_model_meta")


# ---- S2SLS class ------------------------------------------------------------

#' @slot varcov The variance covariance matrix of the estimated paramerters
#' @return
#'
#' @rdname spflow_model_meta
#' @export
setClass("spflow_model_s2sls",
         slots = c(
           varcov = "maybe_matrix"),
         contains = "spflow_model_meta")

# ---- MCMC class -------------------------------------------------------------


#' @slot mcmc_results A data.frame containg the estimated parameters for each iteration of the sampler
#' @return
#'
#' @rdname spflow_model_meta
#' @export
setClass("spflow_model_mcmc",
         slots = c(
           mcmc_results = "maybe_matrix"),
         contains = "spflow_model_meta")

# ---- Virtual classes --------------------------------------------------------
setClassUnion("spflow_model_mle_s2sls",
              c("spflow_model_mle","spflow_model_s2sls"))

# ---- generics & methods -----------------------------------------------------
#' @export
setMethod(
  f = "logLik",
  signature = "spflow_model_mle",
  function(object) { # ---- logLik --------------------------------------------
    return(object@ll)
  })

#' @title Access variance covriance matrix of the estimators
#' @param object spflow_model_mle or spflow_model_s2sls
#' @name varcov
#' @export
setGenericVerif("varcov",  function(object){ # ---- varcov ---------------
  standardGeneric("varcov")})

#' @export
#' @rdname varcov
setMethod(
  f = "varcov",
  signature = "spflow_model_mle_s2sls",
  function(object){ # ---- varcov ---------------------------------------------
    return(object@varcov)
  })




