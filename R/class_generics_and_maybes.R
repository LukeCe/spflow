#' @include spflow_package.R utils.R

# ---- Maybe Classes ----------------------------------------------------------
# allow NULL slots during instantiation of classes
setOldClass("mcmc")
setClassUnion("maybe_data.frame", c("NULL", "data.frame"))
setClassUnion("maybe_list"      , c("NULL", "list"))
setClassUnion("maybe_formula"   , c("NULL", "formula"))
setClassUnion("maybe_Matrix"    , c("NULL", "Matrix"))
setClassUnion("maybe_matrix"    , c("NULL", "matrix"))
setClassUnion("maybe_any_matrix", c("NULL", "matrix", "Matrix"))
setClassUnion("maybe_mcmc"      , c("NULL", "mcmc"))
setClassUnion("maybe_numeric"   , c("NULL", "numeric"))

# ---- New Generics -----------------------------------------------------------
#' @title Generic functions for the [spflow_network_classes()]
#'   and the [spflow_model-class()]
#' @description
#'   Most of these generic functions link to simple accessing or
#'   replacement methods, and do not have their own documentation page.
#'   The associated methods are documented within the corresponding classes.
#'
#'   Generics with more complex methods have their dedicated documentation pages.
#'
#'   Also note that some generic functions are currently not exported and meant
#'   to be used by package developers only.
#'
#' @param object A [spflow_model-class()] or [spflow_network_classes()]
#' @param ... Arguments passed on to methods
#' @param value An appropriate replacement value
#' @name spflow_generics
#' @seealso
#'   - Documentation of classes: [spflow_model-class()], [spflow_network_classes()]
#'   - Other generics and methods: [pair_cor()], [pair_merge()], [predict()], [predict_effect()], [spflow_moran_plots()], [spflow_map()]
NULL

# ---- ... complete_pairs -----------------------------------------------------
#' @rdname spflow_generics
#' @name complete_pairs
#' @export
setGeneric(
  name = "complete_pairs",
  def = function(object, ...) standardGeneric("complete_pairs"))

# ---- ... coord --------------------------------------------------------------
#' @rdname spflow_generics
#' @name coord
#' @keywords internal
#' @usage coord(object, ...) # internal
setGeneric(
  name = "coord",
  def = function(object, ...) standardGeneric("coord"))

# ---- ... dat ----------------------------------------------------------------
#' @rdname spflow_generics
#' @name dat
#' @export
setGeneric(
  name = "dat",
  def = function(object, ...) standardGeneric("dat"))


# ---- ... dat<- --------------------------------------------------------------
#' @rdname spflow_generics
#' @name dat<-
#' @aliases dat-set
#' @export
setGeneric(
  name = "dat<-",
  def = function(object, ... , value) standardGeneric("dat<-"))


# ---- ... id -----------------------------------------------------------------
#' @rdname spflow_generics
#' @name id
#' @export
setGeneric(
  name = "id",
  def = function(object, ...) standardGeneric("id"))


# ---- ... id<- ---------------------------------------------------------------
#' @rdname spflow_generics
#' @name id<-
#' @aliases id-set
#' @export
setGeneric(
  name = "id<-",
  def = function(object, ..., value) standardGeneric("id<-"))

# ---- ... impacts_matrix -----------------------------------------------------
#' @rdname spflow_generics
#' @name impacts_matrix
#' @export
setGeneric(
  name = "impacts_matrix",
  def = function(object, ...) standardGeneric("impacts_matrix"))


# ---- ... mcmc_results -------------------------------------------------------
#' @rdname spflow_generics
#' @name mcmc_results
#' @export
setGeneric(
  name = "mcmc_results",
  def = function(object) standardGeneric("mcmc_results"))


# ---- ... neighborhood -------------------------------------------------------
#' @rdname spflow_generics
#' @name neighborhood
#' @export
setGeneric(
  name = "neighborhood",
  def = function(object, ...) standardGeneric("neighborhood"))


# ---- ... neighborhood<- -----------------------------------------------------
#' @rdname spflow_generics
#' @name neighborhood<-
#' @aliases neighborhood-set
#' @export
setGeneric(
  name = "neighborhood<-",
  def = function(object, value) standardGeneric("neighborhood<-"))


# ---- ... nnodes -------------------------------------------------------------
#' @rdname spflow_generics
#' @name nnodes
#' @export
setGeneric(
  name = "nnodes",
  def = function(object, ...) standardGeneric("nnodes"))


# ---- ... npairs -------------------------------------------------------------
#' @rdname spflow_generics
#' @name npairs
#' @export
setGeneric(
  name = "npairs",
  def = function(object, ...) standardGeneric("npairs"))


# ---- ... pair_cor ----------------------------------------------------------
#' @title Correlation matrices for OD data
#'
#' @description
#' The method computes person correlations for all variables available for the
#' the origins, destinations, and OD-pairs.
#' The OD-pairs information can be either come from a
#' [spflow_network_multi][spflow_network_multi-class()] or a
#' [spflow_models][spflow_model-class()].
#'
#' @param object A [spflow_model-class()] or a [spflow_network_multi-class()]
#' @param ... Arguments to be passed to methods
#' @seealso [spflow_network_multi-class()], [spflow_model-class()]
#' @return A matrix of pairwise correlations between all variables
#' @author Lukas Dargel
#' @export
setGeneric(
  name = "pair_cor",
  def = function(object, ...) standardGeneric("pair_cor"))


# ---- ... pair_merge ---------------------------------------------------------
#' @title Create a long form data.frame of origin-destination pairs
#'
#' @description
#' The method merges all available information on origins and destinations to
#' the data.frame describing the OD-pairs.
#' @name pair_merge
#' @param object A [spflow_network_multi-class()]
#' @param ... Arguments to be passed to methods
#' @seealso [spflow_network_multi-class()]
#' @author Lukas Dargel
#' @export
setGeneric(
  name = "pair_merge",
  def = function(object, ...) standardGeneric("pair_merge"))


# ---- ... predict_effect -----------------------------------------------------
#' @rdname predict
#' @name predict_effect
#' @export
setGeneric(
  name = "predict_effect",
  def = function(object, ...) standardGeneric("predict_effect"))

# ---- ... pull_member --------------------------------------------------------
#' @rdname spflow_generics
#' @name pull_member
#' @export
setGeneric(
  name = "pull_member",
  def = function(object, ...) standardGeneric("pull_member"))


# ---- ... results ------------------------------------------------------------
#' @rdname spflow_generics
#' @name results
#' @export
setGeneric(
  name = "results",
  def = function(object, ...) standardGeneric("results"))


# ---- ... results<- ----------------------------------------------------------
#' @rdname spflow_generics
#' @name results<-
#' @keywords internal
#' @usage results(object) <- value # internal
setGeneric(
  name = "results<-",
  def = function(object, value) standardGeneric("results<-"))


# ---- ... results_flat -------------------------------------------------------
#' @rdname spflow_generics
#' @name results_flat
#' @usage results_flat(object, ...)
#' @export
setGeneric(
  name = "results_flat",
  def = function(object, ...) standardGeneric("results_flat"))


# ---- ... sd_error -----------------------------------------------------------
#' @rdname spflow_generics
#' @name sd_error
#' @export
setGeneric(
  name = "sd_error",
  def = function(object) standardGeneric("sd_error"))

# ---- ... spflow_map ---------------------------------------------------------
#' @title Geographic representation of flows
#' @description
#' This generic is used as an interface to [map_flows()], where all information
#' available in the object is extracted and provided as arguments.
#'
#' @name spflow_map
#' @param object A [spflow_network_multi-class()] or a [spflow_model-class()]
#' @param ... arguments passed to methods and `map_flows()`
#' @author Lukas Dargel
#' @export
setGeneric(
  name = "spflow_map",
  def = function(object, ...) standardGeneric("spflow_map"))

# ---- ... spflow_moran_plots -------------------------------------------------
#' @title Moran scatter plots of interaction data
#'
#' @description
#' Generate up to three Moran scatter plots, related to origin-, destination-,
#' and origin-to-destination-dependence.
#'
#' @rdname spflow_moran_plots
#' @param object A [spflow_network_multi-class()] or a [spflow_model-class()]
#' @param ... arguments passed to methods
#' @export
setGeneric(
  name = "spflow_moran_plots",
  def = function(object, ...) standardGeneric("spflow_moran_plots"))

# ---- ... update_dat ---------------------------------------------------------
#' @rdname spflow_generics
#' @name update_dat
#' @export
setGeneric(
  name = "update_dat",
  def = function(object, ...) standardGeneric("update_dat"))

# ---- ... varcov -------------------------------------------------------------
#' @rdname spflow_generics
#' @name varcov
#' @export
setGeneric(
  name = "varcov",
  def = function(object) standardGeneric("varcov"))

