#' @include utils.R compact-purrr.R class_virtual.R

#' An S4 class to represent information origin-destination pairs (od-pairs) composed of two nodes.
#'
#' Each origin destination pair is composed of two nodes (see [sp_network()]).
#' All origins belong to the same (origin-) network and all destination belong to
#' the same (destination-) network.
#' It is possible to chose the same network for origins and destinations, which
#' enables to represent origin-destination pairs within the same network.
#'
#' @slot origin_id A character that serves as identifier for the origin network
#' @slot origin_count A numeric that represents the number of nodes in the origin network
#' @slot destination_id A character that serves as identifier for the destination network
#' @slot destination_count A numeric that represents the number of nodes in the destination network
#' @slot pair_data A list of matrices that contain information on node pairs (origins are in rows and destinations are in columns)
#'
#' @family sp_network_pair sp_multi_network
#' @importClassesFrom Matrix Matrix
#' @export
setClass("sp_network_pair",
         slots = c(origin_id         = "character",
                   origin_count      = "maybe_numeric",
                   destination_id    = "character",
                   destination_count = "maybe_numeric",
                   pair_id           = "character",
                   pair_data         = "maybe_list",
                   pair_count        = "maybe_numeric"))

# validity --------------------------------------------------------------------
setValidity("sp_network_pair", function(object) {

  consitent_od_dim <-
    lapply(object@pair_data, dim) %>%
    append(.,list(c(object@origin_count,
                    object@destination_count))) %>%
    has_equal_elements(.)

  if (!consitent_od_dim) {
    error_msg <- "The pair dimensions of the pair attributes imply an inconsitent number of origins and destinations!"
    return(error_msg)
  }

  TRUE
})



# get and set -----------------------------------------------------------------
setMethod(
  f = "count",
  signature = "sp_network_pair",
  definition = function(object, what = cases) { # counts ----

    counts <- c(
      "origins" = object@origin_count,
      "destinations" = object@destination_count,
      "pairs" = object@destination_count * object@origin_count
    )
    cases <- names(counts)
    assert_valid_case(what,cases)

    return(counts[what])
  })

setMethod(
  f = "data",
  signature = "sp_network_pair",
  definition = function(object) { # data ----
    return(object@pair_data)
  })

setReplaceMethod(
  f = "data",
  signature = "sp_network_pair",
  definition = function(object,value) {

    object@pair_data <- value %>%
      savely_to_list(.) %>%
      lapply(try_coercion,"Matrix")

    dim_pair_data <- dim(object@pair_data[[1]])

    object@origin_count <- dim_pair_data[1]
    object@destination_count <- dim_pair_data[2]

    if (validObject(object)) {
      return(object)
    }
  })

setMethod(
  f = "id",
  signature = "sp_network_pair",
  definition = function(object,what = cases) { # id ----

    ids <- c(
      "origins" = object@origin_id,
      "destinations" = object@destination_id,
      "pairs" = object@pair_id
    )
    cases <- names(ids)
    assert_valid_case(what,cases)

    return(ids[what])
  })

setReplaceMethod(
  f = "id",
  signature = "sp_network_pair",
  definition = function(object,origin_id,destination_id) {
    object@origin_id      <- origin_id
    object@destination_id <- destination_id
    object@pair           <- origin_id %p% "_" %p% destination_id

    if (validObject(object))
      return(object)
  })

setMethod(
  f = "variable_names",
  signature = "sp_network_pair",
  definition = function(object) { # variable_names ----
    return(names(object@pair_data))
  })

setReplaceMethod(
  f = "variable_names",
  signature = "sp_network_pair",
  definition = function(object,value) {
    names(object@pair_data) <- value
    if (validObject(object))
      return(object)
  })


# constructors ----------------------------------------------------------------
#' Create an S4 object that contains information on origin-destination pairs
#'
#' @param origin_id A character that serves as identifier for the origin network
#' @param destination_id A character that serves as identifier for the destination network
#' @param pair_data A list of matrices that contain information on node pairs (origins are in rows and destinations are in columns)
#'
#' @family sp_network_pair sp_multi_network
#'
#' @return An S4 class of type
#' @export
sp_network_pair <- function(
  origin_id,
  destination_id,
  pair_data = NULL
) {

  # solve the naming for later modelling
  # use Matrix class for efficiency
  pair_data <- pair_data %>%
    savely_to_list()  %>%
    lapply(., try_coercion, "Matrix")

  pair_variable_key <- "G"

  if (is.null(pair_data)) {
    names(pair_data) <- pair_variable_key %p% seq_along(pair_data)
  }

  names(pair_data) <- names(pair_data) %>%
    make.names() %>%
    replace_empty(pair_variable_key) %>%
    replace_NA_chr(pair_variable_key) %>%
    make.unique("")

  # solve the dimensions
  pair_data_dimensions <-
    pair_data %>%
    lapply(., dim)

  assert(has_equal_elements(pair_data_dimensions),
         ("All supplied information on pairs of nodes must be " %p%
            "matrices with the same dimensions!"))

  pair_data_dimensions <- pair_data_dimensions %>% unique() %>% unlist()

  node_pair_data <- new(
    "sp_network_pair",
    origin_id         = origin_id,
    origin_count      = pair_data_dimensions[1],
    destination_id    = destination_id,
    destination_count = pair_data_dimensions[2],
    pair_id           = origin_id %p% "_" %p% destination_id,
    pair_count        = prod(pair_data_dimensions),
    pair_data         = pair_data
  )

  return(node_pair_data)
}

