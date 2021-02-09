#' @include class_spflow-model-meta.R

# ---- OLS class --------------------------------------------------------------

#' @slot varcov A matrix or NULL
#'
#' @rdname spflow_model-class
#' @export
setClass("spflow_model_ols",
         slots = c(
           varcov = "maybe_matrix"),
         contains = "spflow_model")

# ---- MLE class --------------------------------------------------------------

#' @slot ll A numeric or NULL
#' @slot AIC A numeric or NULL
#' @slot BIC A numeric or NULL
#'
#' @rdname spflow_model-class
#' @export
setClass("spflow_model_mle",
         slots = c(
           ll = "maybe_numeric",
           AIC = "maybe_numeric",
           BIC = "maybe_numeric",
           varcov = "maybe_matrix"),
         contains = "spflow_model")


# ---- S2SLS class ------------------------------------------------------------

#' @rdname spflow_model-class
#' @export
setClass("spflow_model_s2sls",
         slots = c(
           varcov = "maybe_matrix"),
         contains = "spflow_model")

# ---- MCMC class -------------------------------------------------------------


#' @slot mcmc_results
#' A data.frame containing the estimated parameters for each iteration of the
#' MCMC sampling procedure
#'
#' @rdname spflow_model-class
#' @export
setClass("spflow_model_mcmc",
         slots = c(
           mcmc_results = "maybe_matrix"),
         contains = "spflow_model")

# ---- Virtual classes --------------------------------------------------------
setClassUnion("spflow_model_mle_s2sls_ols",
              c("spflow_model_ols",
                "spflow_model_mle",
                "spflow_model_s2sls"))

# ---- generics & methods -----------------------------------------------------

#' @title Access the value of the log-likelihood function
#' @param object A [spflow_model-class()]
#' @rdname spflow_model_mle
setMethod(
  f = "logLik",
  signature = "spflow_model_mle",
  function(object) { # ---- logLik --------------------------------------------
    return(object@ll)
  })

#' @title Access the value of the log-likelihood function for the MLE
#' @param object A [spflow_model-class()]
#' @rdname spflow_model-class
setMethod(
  f = "varcov",
  signature = "spflow_model_mle_s2sls_ols",
  function(object){ # ---- varcov ---------------------------------------------
    return(object@varcov)
  })




