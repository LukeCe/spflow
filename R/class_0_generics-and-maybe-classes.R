#' @include utils_classes.R spflow-package.R

# This script adds new generics and virtual classes that are used inside the
# package it should be run before all other classes which is ensured by the
# class_[abc] naming convention for scripts that define classes.


# ---- Maybe Classes ----------------------------------------------------------
# allow NULL slots during instantiation of classes
setOldClass("mcmc")
setClassUnion("maybe_data.frame", c("NULL", "data.frame"))
setClassUnion("maybe_Matrix"    , c("NULL", "Matrix"))
setClassUnion("maybe_matrix"    , c("NULL", "matrix"))
setClassUnion("maybe_any_matrix", c("NULL", "matrix", "Matrix"))
setClassUnion("maybe_list"      , c("NULL", "list"))
setClassUnion("maybe_numeric"   , c("NULL", "numeric"))
setClassUnion("maybe_mcmc"      , c("NULL", "mcmc"))


# ---- New Generics -----------------------------------------------------------

#' @name add_details
#' @rdname add_details
#' @keywords internal
setGeneric(
  name = "add_details", # ---- ... add_details --------------------------------
  def = function(object, ...) standardGeneric("add_details"))



#' @title Generic for accessing the data inside
#'   [spflow network classes][sp_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @param object A [spflow network classes][sp_network_classes()]
#' @name dat
#' @aliases dat-set
#' @seealso [sp_network_classes()]
#' @export
setGeneric(
  name = "dat", # ---- ... dat ------------------------------------------------
  def = function(object, ...) standardGeneric("dat"))


#' @param value A data.frame to replace existing data
#' @rdname dat
#' @name dat<-
#' @export
setGeneric(
  name = "dat<-", # ---- ... dat<- --------------------------------------------
  def = function(object, ...) standardGeneric("dat<-"))


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
setGeneric(
  name = "id", # ---- ... id --------------------------------------------------
  def = function(object, ...) standardGeneric("id"))


#' @param value A character replacing the existing id
#' @rdname id
#' @name id<-
#' @export
setGeneric(
  name = "id<-", # ---- ... id<- ----------------------------------------------
  def = function(object, ...) standardGeneric("id<-"))


#' @title Access the sampling results of a [spflow_model_mcmc-class()]
#' @description For details see the documentation of the corresponding class.
#' @name mcmc_results
#' @param object A [spflow_model_mcmc-class()]
#' @seealso [spflow_model_mcmc-class()]
#' @export
setGeneric(
  name = "mcmc_results", # ---- ... mcmc_results ------------------------------
  def = function(object, ...) standardGeneric("mcmc_results"))


#' @title Generic for accessing the neighborhood matrix inside
#'   [spflow network classes][sp_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @param object One of [sp_network_nodes-class()], [sp_multi_network-class()]
#' @name neighborhood
#' @seealso [sp_network_nodes-class()] [sp_multi_network-class()]
#'
#' @export
setGeneric(
  name = "neighborhood", # ---- ... neighborhood ------------------------------
  def = function(object, ...) standardGeneric("neighborhood"))


#' @rdname neighborhood
#' @param value A neighborhood matrix to replace the existing one
#' @name neighborhood<-
#' @export
setGeneric(
  name = "neighborhood<-", # ---- ... neighborhood<- --------------------------
  def = function(object, ...) standardGeneric("neighborhood<-"))


#' @title Generic for accessing the node count of
#'   [spflow network classes][sp_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @param object One of [sp_network_nodes-class()], [sp_network_pair-class()]
#' @rdname nnodes
#' @name nnodes
#' @seealso [sp_network_nodes-class()] [sp_network_pair-class()]
#' @export
setGeneric(
  name = "nnodes", # ---- ... nnodes ------------------------------------------
  def = function(object, ...) standardGeneric("nnodes"))


#' @title Generic for accessing the node pair count of a
#'   [sp_network_pair-class()]
#' @description For details on the method see the documentation of the
#'   corresponding class.
#' @param object A [sp_network_pair-class()]
#' @name npairs
#' @seealso [sp_network_pair-class()]
#' @export
setGeneric(
  name = "npairs", # ---- ... npairs ------------------------------------------
  def = function(object, ...) standardGeneric("npairs"))


#' @title Generic for merging information on origins and destination to node
#'   pairs inside [spflow network classes][sp_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @rdname pair_merge
#' @name pair_merge
#' @param object A [sp_multi_network-class()]
#' @seealso [sp_multi_network-class()]
#' @export
setGeneric(
  name = "pair_merge", # ---- ... pair_merge ----------------------------------
  def = function(object, ...) standardGeneric("pair_merge"))



#' @title Generic for accessing a [sp_network_pair-class()] or a
#'   [sp_network_nodes-class()] inside a [sp_multi_network-class()]
#' @description For details on the method see the documentation of the
#'   corresponding class.
#' @param object A [sp_multi_network-class()]
#' @seealso [sp_multi_network-class()]
#' @name pull_member
#' @export
setGeneric(
  name = "pull_member", # ---- ... pull_member --------------------------------
  def = function(object, ...) standardGeneric("pull_member"))


#' @title Generic for accessing the results of a [spflow_model-class()].
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @name results
#' @description For details see the documentation of the corresponding class.
#' @param object A [spflow_model-class()]
#' @seealso [spflow_model-class()]
#' @export
setGeneric(
  name = "results", # ---- ... results ----------------------------------------
  def = function(object, ...) standardGeneric("results"))


#' @title Replacement method for internal purposes
#' @param object A [spflow_model()]
#' @param value A data.frame of results
#' @name results<-
#' @keywords internal
setGeneric(
  name = "results<-", # ---- ... results<- ------------------------------------
  def = function(object, ...) standardGeneric("results<-"))


#' @title Access results of a [spflow_model()] as a one row data.frame
#' @param object spflow_model_meta
#' @name results_flat
#' @rdname results_flat
#' @keywords internal
setGeneric(
  name = "results_flat", # ---- ... results_flat ------------------------------
  def = function(object, ...) standardGeneric("results_flat"))


#' @title Generic for accessing the standard deviation of the residual inside
#'   a [spflow_model-class()].
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @title Extract the standard deviation from a [spflow_model-class()]
#' @description For details see the documentation of the corresponding class.
#' @param object A [spflow_model-class()]
#' @seealso [spflow_model-class()]
#' @name sd_error
setGeneric(
  name = "sd_error", # ---- ... sd_error --------------------------------------
  def = function(object, ...) standardGeneric("sd_error"))



#' @title Generic for accessing the variance-covariance matrix of the
#'   parameters inside a [spflow_model-class()].
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @param object A [spflow_model-class()]
#' @name varcov
#' @seealso [spflow_model-class()]
#' @export
setGeneric(
  name = "varcov", # ---- ... varcov ------------------------------------------
  def = function(object, ...) standardGeneric("varcov"))

