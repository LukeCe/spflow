#' @include class_sp-network-nodes.R class_sp-network-pair.R


#' @title
#' An S4 class that gathers information on one or multiple networks
#' [sp_network_nodes()] and origin-destination pairs [sp_network_pair()].
#'
#' @slot networks A list of [sp_network_nodes()] objects
#' @slot network_pairs A list of [sp_network_pair()] objects
#'
#' @family network_info
#' @export
setClass("sp_multi_network",
         slots = c(networks = "list",
                   network_pairs = "list"))


# ---- Methods ----------------------------------------------------------------

#' @export
#' @rdname id
setMethod(
  f = "id",
  signature = "sp_multi_network",
  function(object) { # ---- id ------------------------------------------------

    network_ids <- rapply(slot(object,"networks"), id)
    network_pair_ids <- lapply(slot(object,"network_pairs"), id)

    return(list("networks" = as.vector(network_ids),
                "network_pairs" = network_pair_ids))
  })


#' @param network_id A single character for an id of [sp_network_nodes()]
#' @param network_pair_id A single character for an id of [sp_network_pair()]
#'
#' @rdname dat
#' @export
setMethod(
  f = "dat",
  signature = "sp_multi_network",
  function(object,
           network_id = NULL,
           network_pair_id = NULL) { # ---- dat -------------------------------

    if (is.null(network_id) & is.null(network_pair_id)) {
      return(NULL)
    }

    if (is.null(network_pair_id)) {
      return(dat(object@networks[[network_id]]))
    }

    if (is.null(network_id)) {
      return(dat(object@network_pairs[[network_pair_id]]))
    }
})

#' @param network_ids A character vector of ids for contained [sp_network_nodes()] objects
#' @rdname neighborhoods
#' @export
setMethod(
  f = "neighborhoods",
  signature = "sp_multi_network",
  function(object,
           network_ids = NULL) { # ---- neighborhoods --------------------------
    return(networks(object, network_ids) %>% lapply(neighborhood))
    })

#' @inheritParams neighborhoods
#' @rdname networks
#' @export
setMethod(
  f = "networks",
  signature = "sp_multi_network",
  function(object,
           network_ids = NULL) { # ---- networks -------------------------------

    network_ids <- network_ids %||% id(object)$networks
    return(object@networks[network_ids])
  })

#' @param network_pair_ids A character vector of ids for contained [sp_network_pair()] objects
#' @rdname network_pairs
#' @export
setMethod(
  f = "network_pairs",
  signature = "sp_multi_network",
  function(object,
           network_pair_ids = NULL) { # ---- network_pairs ---------------------

    network_pair_ids <- network_pair_ids %||% names(id(object)$network_pairs)
    if (is_single_character(network_pair_ids))
      return(object@network_pairs[[network_pair_ids]])

    if (is.character(network_pair_ids))
      return(object@network_pairs[network_pair_ids])

    stop("The id must be a character!")
  })

setMethod(
  f = "show",
  signature = "sp_multi_network",
  function(object){ # ---- show -----------------------------------------------

    cat("Collection of spatial network nodes and pairs")
    cat("\n")
    cat(print_line(50))

    multi_net_ids <- id(object)
    nodes_ids <- multi_net_ids$networks
    cat("\nContains",length(nodes_ids),
        "sets of spatial network nodes.",
        "\n    With ids:",nodes_ids)

    pair_ids <- names(multi_net_ids$network_pairs)
    cat("\nContains",length(pair_ids),
        "sets of spatial network pairs",
        "\n    With ids:",pair_ids)

    od_pair_info <- multi_net_ids$network_pairs %>%
      reduce(rbind) %>%
      as.data.frame(row.names = seq_along(pair_ids))

    od_pair_info["(o info)"] <-
      lapply(od_pair_info["origin"],"%in%", nodes_ids)
    od_pair_info["(d info)"] <-
      lapply(od_pair_info["destination"],"%in%", nodes_ids)
    cat("\n\nAvailability origin-destination pair information:\n")
    print(od_pair_info[,c(1,2,4,3,5)])

    cat("\n")
    invisible(object)
  })


#' @inheritParams dat
#' @rdname pair_merge
#' @export
setMethod(
  f = "pair_merge",
  signature = "sp_multi_network",
  function(object,
           network_pair_id) { # ---- pair_merge -------------------------------

    od_ids <- id(object)[["network_pairs"]][[network_pair_id]]

    orig_data <- dat(object,network_id = od_ids["origin"])
    names(orig_data) <- "orig_" %p% names(orig_data)

    dest_data <- dat(object,network_id = od_ids["destination"])
    names(dest_data) <- "dest_" %p% names(dest_data)

    pair_data <- dat(object,network_pair_id = od_ids["pair"])

    expanded_data <- pair_data[dest_data, on = "dest_id"
                               ][orig_data, on = "orig_id"]

    return(expanded_data)
})

setValidity("sp_multi_network",
            function(object) { # ---- validity --------------------------------

              valid_pairs <-
                c(rapply(object@network_pairs, is_one_of,
                         .classes = c("sp_network_pair","NULL")),
                  rapply(object@network_pairs, validObject))

              if (!all(valid_pairs)) {
                error_msg <-
                  "All objects in the network_pairs accessor " %p%
                  "must be of class sp_network_pair!"
                return(error_msg)
              }

              valid_nodes <-
                c(rapply(object@networks, is_one_of, .classes = c("sp_network_nodes","NULL")),
                  rapply(object@networks, validObject))

              if (!all(valid_nodes)) {
                error_msg <-
                  "All objects in the networks accessor must be of class sp_network."
                return(error_msg)
              }

              network_names <- lapply(object@networks, slot, name = "network_id")
              od_names <- lapply(object@network_pairs, pull_slots,
                                 .slots = c("origin_network_id",
                                            "destination_network_id"))
              od_keys <- lapply(od_names, reduce, paste, sep = "_")

              if (!(has_distinct_elements(network_names)
                    & has_distinct_elements(od_keys))
              ) {
                error_msg <-
                  "The identification of all networks and network_pairs must be unique!"
                return(error_msg)
              }

              network_sizes <- lapply(object@networks, slot, name = "node_count")
              names(network_sizes) <- network_names

              od_names <- reduce(od_names,c)
              od_sizes <-
                lapply(object@network_pairs, pull_slots,
                       .slots = c("origin_node_count","destination_node_count")) %>%
                reduce(c) %>%
                setNames(.,od_names)

              consistent_node_numbers <-
                lapply(network_names,
                       function(.name,.net,.od) c(.net[[.name]],.od[[.name]]),
                       .net = network_sizes, .od = od_sizes) %>%
                rapply(., has_equal_elements)

              if (!all(consistent_node_numbers)) {
                error_msg <-
                  "The number of nodes in network [%s] is not consistent!" %>%
                  sprintf(.,paste(od_names[!consistent_node_numbers],sep = " and "))
                return(error_msg)
              }

              return(TRUE)
            })



# ---- Constructors -----------------------------------------------------------

#' Create an S4 class that contains [sp_network_nodes()] and [sp_network_pair()] for one or multiple networks
#'
#' @param ... objects of of type [sp_network_nodes()] and [sp_network_pair()]
#'
#' @return A S4 network data object
#' @export
sp_multi_network <- function(...) {

  input_nets <- list(...) %>% flatten() %||% list()

  assert(all(rapply(input_nets, is_one_of,
                    .classes = c("sp_network_nodes", "sp_network_pair"))),
    "All information that is not of type sp_network or sp_network_pair is discarded!",
    warn = TRUE
  )

  is_net <- rapply(input_nets, is, class2 = "sp_network_nodes")
  is_pair <- rapply(input_nets, is, class2 = "sp_network_pair")

  sp_networks <- input_nets[is_net]
  sp_network_pairs <- input_nets[is_pair]

  names(sp_networks) <- lapply(sp_networks, id)
  names(sp_network_pairs) <- lapply(sp_network_pairs, id, "pair")

  return(new("sp_multi_network",
             networks = sp_networks,
             network_pairs = sp_network_pairs))
}


