#' @include class_spflow-model-meta.R

# ---- OLS class --------------------------------------------------------------
#' @slot ll numeric.
#' @slot AIC numeric.
#' @slot BIC numeric.
#' @slot varcov matrix.
#'
#' @return A [spflow_model()] object
#' @rdname spflow_model_meta
#' @export
setClass("spflow_model_ols",
         slots = c(
           ll = "maybe_numeric",
           AIC = "maybe_numeric",
           BIC = "maybe_numeric",
           varcov = "maybe_matrix"),
         contains = "spflow_model_meta")

# ---- MLE class --------------------------------------------------------------

#' @slot ll numeric.
#' @slot AIC numeric.
#' @slot BIC numeric.
#' @slot varcov matrix.
#'
#' @return A [spflow_model()] object
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

#' @slot varcov The variance covariance matrix of the estimated parameters
#' @return
#'
#' @rdname spflow_model_meta
#' @export
setClass("spflow_model_s2sls",
         slots = c(
           varcov = "maybe_matrix"),
         contains = "spflow_model_meta")

# ---- MCMC class -------------------------------------------------------------


#' @slot mcmc_results A data.frame containing the estimated parameters for each iteration of the sampler
#' @return
#'
#' @rdname spflow_model_meta
#' @export
setClass("spflow_model_mcmc",
         slots = c(
           mcmc_results = "maybe_matrix"),
         contains = "spflow_model_meta")

# ---- Virtual classes --------------------------------------------------------
setClassUnion("spflow_model_mle_s2sls_ols",
              c("spflow_model_ols",
                "spflow_model_mle",
                "spflow_model_s2sls"))

# ---- generics & methods -----------------------------------------------------

#' @title
#' Access the value of the log likelihood function of a [spflow_model()]
#' that is estimated by MLE
#'
#' @param object A [spflow_model()] object
#'
#' @export
setMethod(
  f = "logLik",
  signature = "spflow_model_mle",
  function(object) { # ---- logLik --------------------------------------------
    return(object@ll)
  })

#' @export
#' @rdname varcov
setMethod(
  f = "varcov",
  signature = "spflow_model_mle_s2sls_ols",
  function(object){ # ---- varcov ---------------------------------------------
    return(object@varcov)
  })




