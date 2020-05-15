#' @include utils.R class_virtual.R

#' An S4 class to represent information origin-destination pairs (od-pairs) composed of two nodes.
#'
#' Each origin destination pair is composed of two nodes (see [sp_network()]).
#' All origins belong to the same (origin-) network and all destination belong to
#' the same (destination-) network.
#' It is possible to chose the same network for origins and destinations, which
#' enables to represent origin-destination pairs within the same network.
#'
#' @slot origin_network_id A character that serves as identifier for the origin network
#' @slot origin_node_count A numeric that represents the number of nodes in the origin network
#' @slot destination_network_id A character that serves as identifier for the destination network
#' @slot network_pair_id A character identifying the pair of networks
#' @slot node_pair_data A data.table containing information on origin-destination pairs
#' @slot node_pair_count A numeric indicating the number of origin-destination pairs
#' @slot destination_node_count A numeric that represents the number of nodes in the destination network
#'
#' @family sp_network_pair sp_multi_network
#' @importClassesFrom Matrix Matrix
#' @export
setClass("sp_network_pair",
         slots = c(origin_network_id      = "character",
                   origin_node_count      = "maybe_numeric",
                   destination_network_id = "character",
                   destination_node_count = "maybe_numeric",
                   network_pair_id        = "character",
                   node_pair_data         = "maybe_data.table",
                   node_pair_count        = "maybe_numeric"))

# validity --------------------------------------------------------------------
setValidity("sp_network_pair", function(object) {

  consitent_od_dim <-
    c(nrow(object@node_pair_data),
      object@origin_node_count * object@destination_node_count,
      object@node_pair_count) %>%
    has_equal_elements(.)

  if (!consitent_od_dim) {
    error_msg <- "The dimensions of node pairs are inconsistent!"
    return(error_msg)
  }

  TRUE
})

# ---- get and set ------------------------------------------------------------
setMethod(
  f = "count",
  signature = "sp_network_pair",
  function(object, what = cases) { # ---- count -------------------------------

    counts <- c(
      "origins" = object@origin_node_count,
      "destinations" = object@destination_node_count,
      "pairs" = object@node_pair_count
    )
    cases <- names(counts)
    assert_valid_case(what,cases)

    return(counts[what])
  })

setMethod(
  f = "dat",
  signature = "sp_network_pair",
  function(object) { # ---- dat -----------------------------------------------
    return(object@node_pair_data)
  })

setReplaceMethod(
  f = "dat",
  signature = "sp_network_pair",
  function(
    object,
    value,
    ...) {

    sp_network_pair(origin_network_id = object@origin_network_id,
                    destination_network_id = object@destination_network_id,
                    node_pair_data = value,
                    ...)
  })

setMethod(
  f = "id",
  signature = "sp_network_pair",
  function(object,what = cases) { # ---- id -----------------------------------

    ids <- c(
      "network_pair_id" = object@network_pair_id,
      "origin_network_id" = object@origin_network_id,
      "destination_network_id" = object@destination_network_id
    )
    cases <- names(ids)
    assert_valid_case(what,cases)

    return(ids[what])
  })

setReplaceMethod(
  f = "id",
  signature = "sp_network_pair",
  function(object,origin_network_id,destination_network_id) {
    object@origin_network_id      <- origin_network_id
    object@destination_network_id <- destination_network_id
    object@pair           <- origin_network_id %p% "_" %p% destination_network_id

    if (validObject(object))
      return(object)
  })

setMethod(
  f = "variable_names",
  signature = "sp_network_pair",
  function(object) { # ---- variable_names ------------------------------------
    return(names(object@node_pair_data))
  })

setReplaceMethod(
  f = "variable_names",
  signature = "sp_network_pair",
  function(object,value) {
    names(object@node_pair_data) <- value
    if (validObject(object))
      return(object)
  })


# ---- mehtods ----------------------------------------------------------------



# ---- constructors -----------------------------------------------------------
#' Create an S4 object that contains information on origin-destination pairs
#'
#' @param origin_network_id A character that serves as identifier for the origin network
#' @param destination_network_id A character that serves as identifier for the destination network
#' @param node_pair_data
#'
#' @family sp_network_pair sp_multi_network
#'
#' @return An S4 class of type
#' @export
sp_network_pair <- function(
  origin_network_id,
  destination_network_id,
  node_pair_data = NULL,
  origin_key_column = NULL,
  origin_node_count = NULL,
  destination_key_column = NULL,
  destination_node_count = NULL
) {

  network_pair <- new(
    "sp_network_pair",
    origin_network_id      = origin_network_id,
    origin_node_count      = NULL,
    destination_network_id = destination_network_id,
    destination_node_count = NULL,
    network_pair_id        =
      origin_network_id %p% "_" %p% destination_network_id,
    node_pair_data         = try_coercion(node_pair_data, "data.table"),
    node_pair_count        = NULL)

  # early return with empty counts when no data was provided
  if (is.null(network_pair@node_pair_data)
      && validObject(network_pair)) {
    return(network_pair)
  }

  ### Solve the ids and counts
  # case when key variables are given ...
  data_has_origin_key      <-
    !is.null(origin_key_column) & !is.null(node_pair_data)
  data_has_destination_key <-
    !is.null(destination_key_column) & !is.null(node_pair_data)

  if (data_has_origin_key) {
    data.table::setnames(network_pair@node_pair_data,
                         old = origin_key_column,
                         new = "orig_id")
    network_pair@node_pair_data[, orig_id := factor_in_order(orig_id)]
    origin_node_count <- nlevels(network_pair@node_pair_data$orig_id)
  }

  if (data_has_destination_key) {
    data.table::setnames(network_pair@node_pair_data,
                         old = destination_key_column,
                         "dest_id")

    network_pair@node_pair_data[
      , dest_id := factor_in_order(dest_id)]

    destination_node_count <-
      nlevels(network_pair@node_pair_data$dest_id)
  }

  # case when key variables not given ...
  data_needs_destination_key <-
    is.null(destination_key_column) & !is.null(node_pair_data)
  data_needs_origin_key <-
    is.null(origin_key_column) & !is.null(node_pair_data)

  # infer number of origins and destinations
  node_pair_count <-
    nrow(node_pair_data)
  origin_node_count <-
    origin_node_count %||% (node_pair_count / destination_node_count)
  destination_node_count <-
    destination_node_count %||% (node_pair_count / origin_node_count)
  node_pair_count <-
    node_pair_count %||% (destination_node_count * origin_node_count)

  # check if enough information was provided
  all_counts_defined <-
    c(origin_node_count, destination_node_count, node_pair_count) %>%
    length(.) %>%
    identical(.,3L)

  assert(all_counts_defined,
         "The provided information does not suffice to identify all " %p%
         "origin-and destination nodes in the network pair data!\n" %p%
         "Consider providing the arguments [*_key_column] arguments ")

  if (data_needs_origin_key) {
    origin_node_key <-
      factor_in_order(origin_network_id %p%
                        "_" %p%
                        seq_len(origin_node_count)
                      ) %>%
      rep(.,each = destination_node_count)

    network_pair@node_pair_data[, orig_id := origin_node_key]
  }

  if (data_needs_destination_key) {
    destination_node_key <-
      factor_in_order((destination_network_id %p%
                         "_" %p%
                         seq_len(destination_node_count))) %>%
      rep(., times = origin_node_count)

    network_pair@node_pair_data[, dest_id := destination_node_key]
  }

  data.table::setkey(network_pair@node_pair_data,orig_id,dest_id)
  network_pair@origin_node_count <- origin_node_count
  network_pair@destination_node_count <- destination_node_count
  network_pair@node_pair_count <- node_pair_count

  if (validObject(network_pair)) {
    return(network_pair)
  }
}

