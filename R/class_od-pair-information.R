#' @include utils.R compact-purrr.R class_virtual.R

#' An S4 class to represent information origin-destination pairs (od-pairs) composed of two nodes.
#'
#' Each origin destination pair is composed of two nodes (see [node_information()]).
#' All origins belong to the same (origin-) network and all destination belong to
#' the same (destination-) network.
#' It is possible to chose the same network for origins and destinations, which
#' enables to represent od-pairs within the same network.
#'
#' @slot origin_id A character that serves as identifier for the origin network
#' @slot origin_count A numeric that represents the number of nodes in the origin network
#' @slot destination_id A character that serves as identifier for the destination network
#' @slot destination_count A numeric that represents the number of nodes in the destination network
#' @slot pair_data A list of matrices that contain information on node pairs (origins are in rows and destinations are in columns)
#'
#' @family od_pair_information network_data
#' @importClassesFrom Matrix Matrix
#' @export
setClass("od_pair_information",
         slots = c(origin_id         = "character",
                   origin_count      = "maybe_numeric",
                   destination_id    = "character",
                   destination_count = "maybe_numeric",
                   pair_data         = "maybe_list"))

# validity ----
#' @keywords internal
#' @family od_pair_information
setValidity("od_pair_information", function(object) {

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

# constructor ----
#' Create an S4 object that contains information on origin-destination pairs
#'
#' @param origin_id A character that serves as identifier for the origin network
#' @param destination_id A character that serves as identifier for the destination network
#' @param pair_data A list of matrices that contain information on node pairs (origins are in rows and destinations are in columns)
#'
#' @family od_pair_information network_data
#'
#' @return An S4 class of type
#' @export
od_pair_information <- function(
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
    "od_pair_information",
    origin_id         = origin_id,
    origin_count      = pair_data_dimensions[1],
    destination_id    = destination_id,
    destination_count = pair_data_dimensions[2],
    pair_data   = pair_data
  )

  return(node_pair_data)
}

