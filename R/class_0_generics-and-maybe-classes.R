#' @include utils_classes.R spflow-package.R

# This script adds new generics and virtual classes that are used inside the
# package it should be run before all other classes which is ensured by the
# class_[abc] naming convention for scripts that define classes.


# ---- Maybe Classes ----------------------------------------------------------
# allow NULL slots during instantiation of classes
setClassUnion("maybe_data.frame", c("NULL", "data.frame"))
setClassUnion("maybe_Matrix"    , c("NULL", "Matrix"))
setClassUnion("maybe_matrix"    , c("NULL", "matrix"))
setClassUnion("maybe_any_matrix", c("NULL", "matrix", "Matrix"))
setClassUnion("maybe_list"      , c("NULL", "list"))
setClassUnion("maybe_numeric"   , c("NULL", "numeric"))
setOldClass("mcmc")
setClassUnion("maybe_mcmc"      , c("NULL", "mcmc"))


# ---- New Generics -----------------------------------------------------------

#' @name add_details
#' @rdname add_details
#' @keywords internal
setGenericVerif("add_details",function(object, ...){ # ---- add_details -------
  standardGeneric("add_details")})


#' @title Generic for accessing the data inside
#'   [spflow network classes][sp_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @param object A [spflow network classes][sp_network_classes()]
#' @name dat
#' @aliases dat-set
#' @seealso [sp_network_classes()]
#' @export
setGenericVerif("dat",  function(object, ...){ # ---- dat ---------------------
  standardGeneric("dat")})

#' @param value A data.frame to replace existing data
#' @rdname dat
#' @name dat<-
#' @export
setGenericVerif("dat<-",  function(object, ..., value){ # ---- dat <- ---------
  standardGeneric("dat<-")})

#' @title Generic for accessing the ids of
#'   [spflow network classes][sp_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @title Access the id of [spflow network classes][sp_network_classes()]
#' @description For details see the documentation of the corresponding classes.
#' @param object A [spflow network class][sp_network_classes()]
#' @name id
#' @aliases id-set
#' @seealso [sp_network_classes()]
#' @export
setGenericVerif("id", function(object, ...){ # ---- id ------------------------
  standardGeneric("id")})

#' @param value A character replacing the existing id
#' @rdname id
#' @name id<-
#' @export
setGenericVerif("id<-",function(object,...,value){ # ---- id <- ---------------
  standardGeneric("id<-")})

#' @title Access the sampling results of a [spflow_model_mcmc-class()]
#' @description For details see the documentation of the corresponding class.
#' @name mcmc_results
#' @param object A [spflow_model_mcmc-class()]
#' @seealso [spflow_model_mcmc-class()]
#' @export
setGenericVerif("mcmc_results", function(object){ # ---- mcmc_results ---------
  standardGeneric("mcmc_results")})

#' @title Generic for accessing the neighborhood matrix inside
#'   [spflow network classes][sp_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @param object One of [sp_network_nodes-class()], [sp_multi_network-class()]
#' @name neighborhood
#' @seealso [sp_network_nodes-class()] [sp_multi_network-class()]
#'
#' @export
setGenericVerif("neighborhood", function(object, ...){ # ---- neighborhood ----
  standardGeneric("neighborhood")})

#' @rdname neighborhood
#' @param value A neighborhood matrix to replace the existing one
#' @name neighborhood<-
#' @export
setGenericVerif("neighborhood<-",
                function(object, value){ # ---- neighborhood <- ---------------
  standardGeneric("neighborhood<-")})

#' @title Generic for accessing the node count of
#'   [spflow network classes][sp_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @param object One of [sp_network_nodes-class()], [sp_network_pair-class()]
#' @rdname nnodes
#' @name nnodes
#' @seealso [sp_network_nodes-class()] [sp_network_pair-class()]
#' @export
setGenericVerif("nnodes",function(object, ...){ # ---- nnodes -----------------
  standardGeneric("nnodes")})

#' @title Generic for accessing the node pair count of a
#'   [sp_network_pair-class()]
#' @description For details on the method see the documentation of the
#'   corresponding class.
#' @param object A [sp_network_pair-class()]
#' @name npairs
#' @seealso [sp_network_pair-class()]
#' @export
setGenericVerif("npairs",function(object){ # ---- npairs ----------------------
  standardGeneric("npairs")})

#' @title Generic for merging information on origins and destination to node
#'   pairs inside [spflow network classes][sp_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @rdname pair_merge
#' @name pair_merge
#' @param object A [sp_multi_network-class()]
#' @seealso [sp_multi_network-class()]
#' @export
setGenericVerif("pair_merge", function(object, ...){ # ---- pair_merge --------
  standardGeneric("pair_merge")})


#' @title Generic for accessing a [sp_network_pair-class()] or a
#'   [sp_network_nodes-class()] inside a [sp_multi_network-class()]
#' @description For details on the method see the documentation of the
#'   corresponding class.
#' @param object A [sp_multi_network-class()]
#' @seealso [sp_multi_network-class()]
#' @name pull_member
#' @export
setGenericVerif("pull_member",
                function(object, ...){ # ---- pull_member ---------------------
                  standardGeneric("pull_member")})

#' @title Generic for accessing the results of a [spflow_model-class()].
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @name results
#' @description For details see the documentation of the corresponding class.
#' @param object A [spflow_model-class()]
#' @seealso [spflow_model-class()]
#' @export
setGenericVerif("results",  function(object){ # ---- results -------------
  standardGeneric("results")})

#' @title Replacement method for internal purposes
#' @param object A [spflow_model()]
#' @param value A data.frame of results
#' @name results<-
#' @keywords internal
setGenericVerif("results<-",  function(object, value){ # ---- results <- ------
  standardGeneric("results<-")})

#' @title Access results of a [spflow_model()] as a one row data.frame
#' @param object spflow_model_meta
#' @name results_flat
#' @rdname results_flat
#' @keywords internal
setGenericVerif("results_flat",
                function(object, ...){ # ---- results_flat --------------------
  standardGeneric("results_flat")})

#' @title Generic for accessing the standard deviation of the residual inside
#'   a [spflow_model-class()].
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @title Extract the standard deviation from a [spflow_model-class()]
#' @description For details see the documentation of the corresponding class.
#' @param object A [spflow_model-class()]
#' @seealso [spflow_model-class()]
#' @name sd_error
setGenericVerif("sd_error", function(object){ # ---- sd_error -----------------
  standardGeneric("sd_error")})


#' @title Generic for accessing the variance-covariance matrix of the
#'   parameters inside a [spflow_model-class()].
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @param object A [spflow_model-class()]
#' @name varcov
#' @seealso [spflow_model-class()]
#' @export
setGenericVerif("varcov",  function(object){ # ---- varcov --------------------
  standardGeneric("varcov")})
