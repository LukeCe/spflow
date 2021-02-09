#' @include class_sp-network-nodes.R class_sp-network-pair.R


#' @title sp_multi_network Class
#'
#' @description
#' An S4 class that gathers information on one or multiple networks
#' [sp_network_nodes()] and origin-destination pairs [sp_network_pair()].
#'
#' @slot networks A list of [sp_network_nodes()] objects
#' @slot network_pairs A list of [sp_network_pair()] objects
# #' @slot completeness
# #'    A data.frame summarizing the identification between pairs and nodes
#'
#' @family spflow network objects
#' @export
setClass("sp_multi_network",
         slots = c(networks = "list",
                   network_pairs = "list",
                   completeness = "maybe_data.frame"))


# ---- Methods ----------------------------------------------------------------

#' @export
#' @param what
#'     A character to indicating from what part the id should be retrieved;
#'     should be in `c("orig","dest", "pair")`.
#' @rdname id
setMethod(
  f = "id",
  signature = "sp_multi_network",
  function(object, what = cases) { # ---- id ------------------------------------------------

    ids <- list("networks" = as.vector(rapply(slot(object,"networks"), id)),
                "network_pairs" = lapply(slot(object,"network_pairs"), id))
    cases <- names(ids)
    assert_valid_case(what,cases)

    if (is_single_character(what))
      return(ids[[what]])

    return(ids[what])
  })


#' @param network_id
#'   A single character that correspond to the id of a
#'   [sp_network_nodes-class()] inside the [sp_multi_network-class()]
#' @param network_pair_id
#'   A single character that correspond to the id of a
#'   [sp_network_pair-class()] inside the [sp_multi_network-class()]
#' @name dat
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
      assert_is_single_x(network_id, x = "character")
      return(dat(object@networks[[network_id]]))
    }

    if (is.null(network_id)) {
      assert_is_single_x(network_pair_id, x = "character")
      return(dat(object@network_pairs[[network_pair_id]]))
    }
})

#' @param network_ids A character vector of ids for contained
#'    [sp_network_nodes()] objects
#' @rdname pull_neighborhood
#' @keywords internal
setMethod(
  f = "pull_neighborhood",
  signature = "sp_multi_network",
  function(object,
           network_ids = NULL) { # ---- pull_neighborhood ---------------------

    if (length(network_ids) > 1)
      return(pull_nodes(object, network_ids) %>% lapply(neighborhood))


    return(pull_nodes(object, network_ids) %>% neighborhood())
    })

#' @inheritParams pull_neighborhood
#' @rdname pull_nodes
#' @export
setMethod(
  f = "pull_nodes",
  signature = "sp_multi_network",
  function(object,
           network_ids = NULL) { # ---- pull_nodes -------------------------

    network_ids <- network_ids %>% flatten() %||% id(object)$networks

    if (length(network_ids) == 1)
      return(object@networks[[network_ids]])

    return(object@networks[network_ids])
  })

#' @param network_pair_ids A character vector of ids for contained [sp_network_pair()] objects
#' @rdname pull_pairs
#' @export
setMethod(
  f = "pull_pairs",
  signature = "sp_multi_network",
  function(object,
           network_pair_ids = NULL) { # ---- pull_pairs ---------------------

    network_pair_ids <- network_pair_ids %||% names(id(object)$network_pairs)
    if (is_single_character(network_pair_ids))
      return(object@network_pairs[[network_pair_ids]])

    if (is.character(network_pair_ids))
      return(object@network_pairs[network_pair_ids])

    stop("The id must be a character!")
  })

#' @keywords internal
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
        "\n    With ids:", paste(nodes_ids, collapse = ", "))

    pair_ids <- names(multi_net_ids$network_pairs)
    cat("\nContains",length(pair_ids),
        "sets of spatial network pairs",
        "\n    With ids:", paste(pair_ids, collapse = ", "))

    od_pair_info <- multi_net_ids$network_pairs %>%
      lapply(as.list) %>%
      lapply(data.frame, stringsAsFactors = FALSE) %>%
      lreduce(rbind)


    od_pair_info["(o info)"] <-
      lapply(od_pair_info["orig"],"%in%", nodes_ids)
    od_pair_info["(d info)"] <-
      lapply(od_pair_info["dest"],"%in%", nodes_ids)
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

    assert_is_single_x(network_pair_id,"character")
    od_ids <- id(object,"network_pairs")[[network_pair_id]]

    assert(!is.null(od_ids),
           "Network pair with id " %p% network_pair_id %p% " was not found!")

    pair_data <- dat(object,network_pair_id = od_ids["pair"])
    orig_data <- dat(object,network_id = od_ids["orig"])
    dest_data <- dat(object,network_id = od_ids["dest"])

    names(orig_data) <- "ORIG_" %p% names(orig_data)
    names(dest_data) <- "DEST_" %p% names(dest_data)

    expanded_data <- pair_data[dest_data, on = c("DEST_ID")
                               ][orig_data, on = c("ORIG_ID")]

    return(expanded_data)
})

#' @importFrom data.table key
setValidity("sp_multi_network", function(object) { # ---- validity ------------

  ### check validity of pairs and nodes
  object@network_pairs %>% compact() %>% lapply("assert_is", "sp_network_pair")
  object@network_pairs %>% lapply("validObject")

  object@networks %>% compact() %>% lapply("assert_is", "sp_network_nodes")
  object@networks %>% lapply("validObject")

  ### check consistency of pairs and nodes...
  #... naming: networks
  network_names <- lapply(object@networks, slot, name = "network_id")
  pair_names <- lapply(object@network_pairs, slot, name = "network_pair_id")
  unique_names <-
    has_distinct_elements(network_names) & has_distinct_elements(pair_names)
  if (!unique_names) {
    error_msg <-
      "The identification of all networks and network_pairs must be unique!"
    return(error_msg)
  }

  #...identification: nodes
  extract_node_list <- function(net){
    id_col <- key(dat(net))
    nodes <- dat(net) %[[% id_col %>% levels()
  }
  cip <- check_node_identification_in_pairs <- function(pair){

    # node ids in the pair data
    id_cols <- key(dat(pair))
    od_node_list <- list(
      "orig" = dat(pair) %[[% id_cols[1] %>% levels(),
      "dest" = dat(pair) %[[% id_cols[2] %>% levels())

    # node ids in the node data and in the pair data: allow null
    compare_node_lists <- function(str){

      id_net <- id(pair)[str]
      node_ids_net <- net_node_lists[[id_net]]
      node_ids_pair <- od_node_list[[str]]

      consistent_node_lists <-
        length(node_ids_net) == length(node_ids_pair) &&
        all(node_ids_net == node_ids_pair)
      consistent_node_lists <- consistent_node_lists | is.null(node_ids_net)

    }

    node_identifiction <-
      lookup(c("orig","dest")) %>%
      lapply("compare_node_lists") %>% unlist()

  }
  net_node_lists <- object@networks %>% lapply("extract_node_list")
  net_vs_pair_node_lists <- object@network_pairs %>% lapply("cip")
  names(net_vs_pair_node_lists) <- pair_names

  pair_and_net_nodes_match <- net_vs_pair_node_lists %>% flatten() %>% all()
  if (!pair_and_net_nodes_match){
    first_bad_pair <- net_vs_pair_node_lists %>% lfilter("none") %[% 1

    error_msg <-
      "The %s node ids of the sp_network_pair %s " %p%
      "do not match with the corresponding nodes in the sp_network_nodes!"
    bad_o_or_d <-
      first_bad_pair %[[% 1 %>% names() %>% paste(collapse = " and ")

    return(sprintf(error_msg,bad_o_or_d,names(first_bad_pair)))
  }

  # object is valid
  return(TRUE)
})



# ---- Constructors -----------------------------------------------------------

#' Create an S4 class that contains [sp_network_nodes()] and [sp_network_pair()] for one or multiple networks
#'
#' @param ... objects of of type [sp_network_nodes()] and [sp_network_pair()]
#' @param level_node_ids A logical to activate re-levelling of the id columns.
#'    If `TRUE` the origin and destination nodes in [sp_network_pair()] are set
#'    to match the levels of the nodes in the [sp_network_nodes()].
#'
#' @return A S4 network data object
#' @family spflow network objects
#' @export
sp_multi_network <- function(..., level_node_ids = TRUE) {

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

  # TODO add completeness info to multinetwork
  od_pair_completeness <- NULL
  # nodes_ids <- id(object, "networks")
  # od_pair_completeness <-
  #   id(object, "network_pairs") %>%
  #   lapply(as.list) %>%
  #   lapply(data.frame, stringsAsFactors = FALSE) %>%
  #   lreduce(rbind)
  #
  # od_pair_completeness["(o info)"] <-
  #   lapply(od_pair_info["orig"],"%in%", nodes_ids)
  # od_pair_completeness["(d info)"] <-
  #   lapply(od_pair_info["dest"],"%in%", nodes_ids)

  return(new("sp_multi_network",
             networks = sp_networks,
             network_pairs = sp_network_pairs,
             completeness = od_pair_completeness))
}


