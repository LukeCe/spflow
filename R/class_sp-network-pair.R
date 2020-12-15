#' @title
#' An S4 class which holds information on origin-destination pairs.
#'
#' @description
#' Each origin destination pair is composed of two nodes (see [sp_network_nodes()]).
#' All origins belong to the same (origin-) network and all destination belong to
#' the same (destination-) network.
#' It is possible to chose the same network for origins and destinations, which
#' enables to represent origin-destination pairs within the same network.
#'
#' @slot orig_net_id A character that serves as identifier for the origin network
#' @slot orig_nnodes A numeric that represents the number of nodes in the origin network
#' @slot dest_net_id A character that serves as identifier for the destination network
#' @slot network_pair_id A character identifying the pair of networks
#' @slot pair_data A data.table containing information on origin-destination pairs
#' @slot npairs A numeric indicating the number of origin-destination pairs
#' @slot dest_nnodes A numeric that represents the number of nodes in the destination network
#'
#' @family sp_network
#' @importClassesFrom data.table data.table
#' @importClassesFrom Matrix Matrix
#' @export
setClass("sp_network_pair",
         slots = c(orig_net_id     = "character",
                   orig_nnodes     = "maybe_numeric",
                   dest_net_id     = "character",
                   dest_nnodes     = "maybe_numeric",
                   network_pair_id = "character",
                   pair_data       = "maybe_data.table",
                   npairs          = "maybe_numeric"))

# ---- Methods ----------------------------------------------------------------

#' @export
#' @rdname dat
setMethod(
  f = "dat",
  signature = "sp_network_pair",
  function(object) { # ---- dat -----------------------------------------------
    return(object@pair_data)
  })

#' @rdname dat
#' @param ... more arguments passed to the constructor [sp_network_pair()]
#' @keywords internal
setReplaceMethod(
  f = "dat",
  signature = "sp_network_pair",
  function(object, value) {  # ---- dat <- -------------------------------
    object@pair_data <- value
    object@npairs <- nrow(value)
    return(object)
  })

#' @param what
#'    A character to indicating from what part the id should be retrieved;
#'    should be in c("orig","dest", "pair").
#' @rdname id
#' @export
setMethod(
  f = "id",
  signature = "sp_network_pair",
  function(object,what = cases) { # ---- id -----------------------------------

    ids <- c(
      "pair" = object@network_pair_id,
      "orig" = object@orig_net_id,
      "dest" = object@dest_net_id
    )
    cases <- names(ids)
    assert_valid_case(what,cases)

    return(ids[what])
  })


#' @rdname id
#' @keywords internal
setReplaceMethod(
  f = "id",
  signature = "sp_network_pair",
  function(object,value) {  # ---- id <- --------------------------------------

    new_id_orig <- strsplit("_",value) %>% unlist() %>% head(1)
    new_id_dest <- strsplit("_",value) %>% unlist() %>% tail(1)
    new_id_pair <- new_id_orig %p% "_" %p% new_id_dest
    assert(new_id_pair == value,
           "The provided pair id is not valid.")

    object@orig_net_id      <- new_id_orig
    object@dest_net_id <- new_id_dest
    object@pair                   <- new_id_orig %p% "_" %p% new_id_dest

    if (validObject(object))
      return(object)
  })

#' @rdname npairs
#' @export
setMethod(
  f = "npairs",
  signature = "sp_network_pair",
  function(object) { # ---- npairs --------------------------------------------
    return(object@npairs)
  })

#' @param what
#'    A character to indicate what to count; should be in c("orig","dest").
#' @rdname npairs
#' @export
setMethod(
  f = "nnodes",
  signature = "sp_network_pair",
  function(object, what = cases) { # ---- nnodes ------------------------------

    count_nodes <- list(
      "orig" = object@orig_nnodes,
      "dest" = object@dest_nnodes
    )
    cases <- names(count_nodes)
    assert_valid_case(what,cases)

    return(unlist(count_nodes[what]))
  })


#' @export
setMethod(
  f = "show",
  signature = "sp_network_pair",
  function(object){ # ---- show -----------------------------------------------

    cat("Spatial network pair with id:",id(object,"pair"))
    cat("\n")
    cat(print_line(50))

    od_explain <- "\n%s network id: %s (with %s nodes)"

    cat(od_explain %>% sprintf(
      "Origin", id(object,"orig"), nnodes(object, "orig") %||% "[?]"))
    cat(od_explain %>% sprintf(
      "Destination", id(object,"dest"), nnodes(object, "dest")  %||% "[?]"))

    has_all_counts <- (c(npairs(object),nnodes(object)) %>% length()) == 3
    if (has_all_counts) {
      cat("\nNumber of pairs:", npairs(object))
      pair_explain <- "\nCompleteness of pairs: %s (%i/%i)"
      cat(pair_explain %>% sprintf(
        format_percent(npairs(object) / prod(nnodes(object))),
        npairs(object),
        prod(nnodes(object))
      ))
    }

    has_data <- !is.null(dat(object))
    if (has_data) {
      cat("\n\nData on node-pairs:\n")
      print(dat(object))
    }
    cat("\n")
    invisible(object)
  })

setValidity("sp_network_pair", function(object) { # ---- validity -------------

  # check ids
  ids <- id(object)
  pair_id <- (ids["orig"] %p% "_" %p% ids["dest"])
  if (ids["pair"] != pair_id) {
    error_msg <- "The id of the pair object is invalid!"
    return(error_msg)
  }

  # check plausibility and identifiability of the data
  if (is.null(dat(object)))
    return(TRUE)

  possible_pair_count <- prod(nnodes(object)) >= npairs(object)
  data_keys <- dat(object)[,data.table::key(dat(object))]
  unique_identification <- unique(data_keys) %>% nrow() == npairs(object)

  if (!all(possible_pair_count,unique_identification)) {
    error_msg <- "The observations cannot be identifyed!"
    return(error_msg)
  }

  # The object is valid
  return(TRUE)
})
# ---- Constructors -----------------------------------------------------------

#' Create an S4 object that contains information on origin-destination pairs
#'
#' @param orig_net_id A character that serves as identifier for the origin network
#' @param dest_net_id A character that serves as identifier for the destination network
#' @param pair_data A data.frame containing information to describe the origin-destination pairs
#' @param orig_key_column A character indicating the column containing identifiers for the origins
#' @param orig_nnodes A numeric declaring the number of origins
#' @param dest_key_column A character indicating the column containing identifiers for the destinations
#' @param dest_nnodes A numeric declaring the number of destinations
#'
#' @family sp_network
#'
#' @return An S4 class of type [sp_network_pair()]
#' @importFrom data.table setDT := key copy setnames setkeyv
#' @export
sp_network_pair <- function(
  orig_net_id,
  dest_net_id,
  pair_data = NULL,
  orig_key_column,
  dest_key_column
) {

  network_pair <- new(
    "sp_network_pair",
    orig_net_id      = orig_net_id,
    orig_nnodes      = NULL,
    dest_net_id      = dest_net_id,
    dest_nnodes      = NULL,
    network_pair_id  = orig_net_id %p% "_" %p% dest_net_id,
    pair_data        = NULL,
    npairs           = NULL)

  # early return with empty counts when no data was provided
  if (is.null(pair_data) && validObject(network_pair))
    return(network_pair)

  # when the data is provided there must be valid key columns...
  pair_data <- try_coercion(pair_data,"data.table")
  has_orig_key <- orig_key_column %in% colnames(pair_data)
  has_dest_key <- dest_key_column %in% colnames(pair_data)
  assert(has_orig_key & has_dest_key,
         "The origin and destination key columns are not found in " %p%
           "the pair data!")

  # rename and convert to factor
  key_cols <- c("ORIG_ID", "DEST_ID")
  setnames(pair_data, c(orig_key_column,dest_key_column), key_cols)

  # IDEA preserve factor levels if already provided
  pair_data[, ORIG_ID := factor_in_order(ORIG_ID)]
  pair_data[, DEST_ID := factor_in_order(DEST_ID)]

  setkeyv(pair_data,cols = key_cols)
  network_pair@pair_data <- pair_data
  network_pair@orig_nnodes <- nlevels(network_pair@pair_data$ORIG_ID)
  network_pair@dest_nnodes <- nlevels(network_pair@pair_data$DEST_ID)
  network_pair@npairs      <- nrow(pair_data)

  if (validObject(network_pair))
    return(network_pair)
}

