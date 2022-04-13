#' @include utils_classes.R spflow_package.R

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

# ---- ... add_details --------------------------------------------------------
#' @name add_details
#' @rdname add_details
#' @keywords internal
setGeneric(
  name = "add_details",
  def = function(object, ...) standardGeneric("add_details"))

# ---- ... complete_pairs -----------------------------------------------------
#' @name complete_pairs
#' @rdname sp_multi_network-class
#' @export
setGeneric(
  name = "complete_pairs",
  def = function(object, ...) standardGeneric("complete_pairs"))

# ---- ... dat ----------------------------------------------------------------
#' @title Generic for accessing the data inside
#'   [spflow network classes][spflow_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @param object An object belonging to the [spflow network classes][spflow_network_classes()]
#' @param ... Arguments to be passed to methods
#' @return A the data.frame describing the set of nodes or node-pairs
#' @name dat
#' @aliases dat-set
#' @seealso [spflow_network_classes()]
#' @export
setGeneric(
  name = "dat",
  def = function(object, ...) standardGeneric("dat"))


# ---- ... dat<- --------------------------------------------------------------
#' @param value A data.frame to replace existing data
#' @rdname dat
#' @name dat<-
#' @export
setGeneric(
  name = "dat<-",
  def = function(object, ... , value) standardGeneric("dat<-"))


# ---- ... id -----------------------------------------------------------------
#' @title Generic for accessing the ids of
#'   [spflow network classes][spflow_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @title Access the id of [spflow network classes][spflow_network_classes()]
#' @description For details see the documentation of the corresponding classes.
#' @param object A [spflow network class][spflow_network_classes()]
#' @param ... Arguments to be passed to methods
#' @return A character, corresponding to the id
#' @name id
#' @aliases id-set
#' @seealso [spflow_network_classes()]
#' @export
setGeneric(
  name = "id",
  def = function(object, ...) standardGeneric("id"))


# ---- ... id<- ---------------------------------------------------------------
#' @param value A character replacing the existing id
#' @rdname id
#' @name id<-
#' @export
setGeneric(
  name = "id<-",
  def = function(object, ..., value) standardGeneric("id<-"))


# ---- ... flow_map -----------------------------------------------------------
#' @title Graphical representation of flows
#' @param value A character replacing the existing id
#' @rdname flow_map
#' @name flow_map
#' @export
setGeneric(
  name = "flow_map",
  def = function(object, ...) standardGeneric("flow_map"))

# ---- ... flow_moran_plots ---------------------------------------------------
#' @title Graphical representation of flows
#' @param value A character replacing the existing id
#' @rdname flow_moran_plots
#' @name flow_moran_plots
#' @export
setGeneric(
  name = "flow_moran_plots",
  def = function(object, ...) standardGeneric("flow_moran_plots"))


# ---- ... mcmc_results -------------------------------------------------------
#' @title Access the sampling results of a [spflow_model_mcmc-class()]
#' @description For details see the documentation of the corresponding class.
#' @name mcmc_results
#' @param object A [spflow_model_mcmc-class()]
#' @return An mcmc object, containing the draws of the sampled parameters
#' @seealso [spflow_model_mcmc-class()]
#' @export
setGeneric(
  name = "mcmc_results",
  def = function(object) standardGeneric("mcmc_results"))


# ---- ... neighborhood -------------------------------------------------------
#' @title Generic for accessing the neighborhood matrix inside
#'   [spflow network classes][spflow_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @param object One of [sp_network_nodes-class()], [sp_multi_network-class()]
#' @param ... Arguments to be passed to methods
#' @return
#'   A matrix (optionally sparse), representing the neighborhood links
#'   between the nodes
#' @name neighborhood
#' @seealso [sp_network_nodes-class()] [sp_multi_network-class()]
#'
#' @export
setGeneric(
  name = "neighborhood",
  def = function(object, ...) standardGeneric("neighborhood"))


# ---- ... neighborhood<- -----------------------------------------------------
#' @rdname neighborhood
#' @param value A neighborhood matrix to replace the existing one
#' @name neighborhood<-
#' @export
setGeneric(
  name = "neighborhood<-",
  def = function(object, value) standardGeneric("neighborhood<-"))


# ---- ... nnodes -------------------------------------------------------------
#' @title Generic for accessing the node count of
#'   [spflow network classes][spflow_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @param object One of [sp_network_nodes-class()], [sp_network_pair-class()]
#' @param ... Arguments to be passed to methods
#' @return A numeric, corresponding to the number of nodes
#' @rdname nnodes
#' @name nnodes
#' @seealso [sp_network_nodes-class()] [sp_network_pair-class()]
#' @export
setGeneric(
  name = "nnodes",
  def = function(object, ...) standardGeneric("nnodes"))


# ---- ... npairs -------------------------------------------------------------
#' @title Generic for accessing the node pairs count of a
#'   [sp_network_pair-class()]
#' @description For details on the method see the documentation of the
#'   corresponding class.
#' @param object A [sp_network_pair-class()]
#' @return A numeric, corresponding to the number of node-pairs
#' @name npairs
#' @seealso [sp_network_pair-class()]
#' @export
setGeneric(
  name = "npairs",
  def = function(object, ...) standardGeneric("npairs"))


# ---- ... pair_corr ----------------------------------------------------------
#' @title Correlation matrices for OD data
#'
#' @description
#' The method computes person correlations for all variables available for the
#' the origins, destinations, and OD-pairs.
#' The OD-pairs information can be either come from a
#' [sp_multi_network][sp_multi_network-class()] or a
#' [spflow_models][spflow_model-class()].
#'
#' @rdname pair_corr
#' @name pair_corr
#' @seealso [sp_multi_network-class()], [spflow_model-class()]
#' @export
#' @return A matrix of pairwise person correlations between all variables
setGeneric(
  name = "pair_corr",
  def = function(object, ...) standardGeneric("pair_corr"))


# ---- ... pair_merge ---------------------------------------------------------
#' @title Generic for merging information on origins and destination to node
#'   pairs inside [spflow network classes][spflow_network_classes()]
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @rdname pair_merge
#' @name pair_merge
#' @param object A [sp_multi_network-class()]
#' @param ... Arguments to be passed to methods
#' @seealso [sp_multi_network-class()]
#' @export
setGeneric(
  name = "pair_merge",
  def = function(object, ...) standardGeneric("pair_merge"))



#' @title Generic for accessing a [sp_network_pair-class()] or a
#'   [sp_network_nodes-class()] inside a [sp_multi_network-class()]
#' @description For details on the method see the documentation of the
#'   corresponding class.
#' @param object A [sp_multi_network-class()]
#' @param ... Arguments to be passed to methods
#' @return  A [sp_network_pair-class()] or a [sp_network_nodes-class()]
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
#' @return A data.frame, summarizing the results of the estimation
#' @seealso [spflow_model-class()]
#' @export
setGeneric(
  name = "results", # ---- ... results ----------------------------------------
  def = function(object) standardGeneric("results"))


#' @title Replacement method for internal purposes
#' @param object A [spflow_model()]
#' @param value A data.frame of results
#' @name results<-
#' @keywords internal
setGeneric(
  name = "results<-", # ---- ... results<- ------------------------------------
  def = function(object, value) standardGeneric("results<-"))


#' @title Access results of a [spflow_model()] as a one row data.frame
#' @param object spflow_model_meta
#' @param ... Arguments to be passed to methods
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
#' @return
#'   A numeric, representing the estimated standard deviation of the error term
#' @seealso [spflow_model-class()]
#' @name sd_error
setGeneric(
  name = "sd_error", # ---- ... sd_error --------------------------------------
  def = function(object) standardGeneric("sd_error"))



#' @title Generic for accessing the variance-covariance matrix of the
#'   parameters inside a [spflow_model-class()].
#' @description For details on the methods see the documentation of the
#'   corresponding classes.
#' @param object A [spflow_model-class()]
#' @return The variance-covariance matrix of the fitted model parameters
#' @name varcov
#' @seealso [spflow_model-class()]
#' @export
setGeneric(
  name = "varcov", # ---- ... varcov ------------------------------------------
  def = function(object) standardGeneric("varcov"))

