#' @include class_sp-network-nodes.R class_sp-network-pair.R


#' @title Class sp_multi_network
#'
#' @description
#' A S4 class that gathers information on one or multiple networks
#' [sp_network_nodes()] and origin-destination pairs [sp_network_pair()].
#' The class is constructed with the [sp_multi_network()] function.
#'
#' @slot networks A list of [sp_network_nodes-class()] objects
#' @slot network_pairs A list of [sp_network_pair-class()] objects
#'
#' @param object sp_multi_network-class
#' @family spflow network classes
#' @name sp_multi_network-class
#' @export
setClass("sp_multi_network",
         slots = c(networks = "list",
                   network_pairs = "list"))


# ---- Methods ----------------------------------------------------------------

#' @rdname sp_multi_network-class
#' @export
#' @examples
#' ## access the id of a networks or network_pairs inside a multi network
#'
#' id(multi_net_usa_ge)$networks
#' id(multi_net_usa_ge)$network_pairs
#'
setMethod(
  f = "id",
  signature = "sp_multi_network",
  function(object) { # ---- id -----------------------------------------------
    return(list(
      "networks" = names(slot(object,"networks")),
      "network_pairs" = names(slot(object,"network_pairs")))
      )
  })


#' @rdname sp_multi_network-class
#' @param .id A character indicating the id of a [sp_network_nodes-class()] or a
#'   [sp_network_pair-class()] inside the [sp_multi_network-class()].
#' @export
#' @examples
#' ## access the data of a network or a network_pair inside a multi_network
#'
#' dat(multi_net_usa_ge, "ge")    # extract data of nodes
#' dat(multi_net_usa_ge, "ge_ge") # extract data of pairs
#'
setMethod(
  f = "dat",
  signature = "sp_multi_network",
  function(object, .id) { # ---- dat ------------------------------------

    assert(.id %in% unlist(id(object)),
           "The provided id does not correspond to any network object.")
    if (valid_network_id(.id))
      from <- "networks"
    if (valid_network_pair_id(.id))
      from <- "network_pairs"
    return(dat(slot(object,from)[[.id]]))
})


#' @rdname sp_multi_network-class
setMethod(
  f = "neighborhood",
  signature = "sp_multi_network",
  function(object, .id = 1) { # ---- neighborhood ----------------------
    assert(.id %in% id(object)[["networks"]],
           "The provided id does not correspond to any sp_network_nodes.")
    sp_net <- slot(object, "networks")[[.id]]
    return(sp_net %|!|% neighborhood(sp_net))
  })

#' @rdname sp_multi_network-class
#' @export
#' @examples
#' ## access sp_network_nodes or sp_network_pair inside a sp_multi_network
#'
#' pull_member(multi_net_usa_ge,"ge")
#' pull_member(multi_net_usa_ge,"usa")
#' pull_member(multi_net_usa_ge,"ge_ge")
#'
setMethod(
  f = "pull_member",
  signature = "sp_multi_network",
  function(object, .id) { # ---- pull_member ----------------------------------
    assert(.id %in% unlist(id(object)),
           "The provided id does not correspond to any network object.")
    if (valid_network_id(.id))
      from <- "networks"
    if (valid_network_pair_id(.id))
      from <- "network_pairs"
    return(slot(object,from)[[.id]])
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

    pair_ids <- multi_net_ids$network_pairs
    cat("\nContains",length(pair_ids),
        "sets of spatial network pairs",
        "\n    With ids:", paste(pair_ids, collapse = ", "))

    if (length(pair_ids) > 0) {
      cat("\n\nAvailability of origin-destination pair information:\n")
      od_pair_info <- check_pair_completeness(object)
      od_pair_info$COMPLETENESS <- format_percent(od_pair_info$COMPLETENESS)

      print(od_pair_info)
    }


    cat("\n")
    invisible(object)
  })


#' @title Create a long form data.frame of origin-destination pairs
#'
#' @description
#' The method merges all available information on origins and destinations to
#' the data.frame describing the pairs.
#'
#' @param object
#'   A [sp_multi_network-class()]
#' @param network_pair_id
#'   A character indicating the id of a [sp_network_pair-class()]
#' @param all_pairs
#'   A logical, when set to `TRUE` the resulting data.frame contains all
#'   possible pairs of origins and destination, even if the data in the
#'   [sp_network_pair-class()] does not have them.
#' @return A single data.frame, combining all available information on origins, destinations and OD pairs
#' @rdname pair_merge
#' @export
#' @examples
#' # long form data for flows from Germany to Germany
#' pair_merge(multi_net_usa_ge,"ge_ge")
#'
#' # long form data for flows from Germany to USA
#' pair_merge(multi_net_usa_ge,"ge_usa")
setMethod(
  f = "pair_merge",
  signature = "sp_multi_network",
  function(object, network_pair_id, all_pairs = FALSE) { # ---- pair_merge ----

    od_ids <- id(object)[["network_pairs"]]
    assert(network_pair_id %in% od_ids,
           "Network pair with id " %p% network_pair_id %p% " was not found!")

    pair_data <- dat(object, network_pair_id)
    orig_data <- dat(object, id_part(network_pair_id, "orig"))
    dest_data <- dat(object, id_part(network_pair_id, "dest"))
    od_keys <- attr_key_od(pair_data)

    # check if cartesian merge is required
    if (all_pairs && (nrow(pair_data) < nrow(orig_data) * nrow(dest_data))) {
      o_keys <- orig_data[[attr_key_nodes(orig_data)]]
      d_keys <- dest_data[[attr_key_nodes(dest_data)]]
      template <-
        data.frame(rep(o_keys,length(d_keys)),
                   rep(d_keys, each = length(o_keys)))
      names(template) <- od_keys
      pair_data <- merge(template,pair_data,by = od_keys,all = TRUE)
    }

    names(dest_data) <- "DEST_" %p% names(dest_data)
    d_key <- "DEST_" %p% attr_key_nodes(dest_data)
    pair_data <- merge(
      pair_data, dest_data,
      by.x = od_keys[2],
      by.y = d_key,
      all.x = TRUE,
      sort = FALSE)

    names(orig_data) <- "ORIG_" %p% names(orig_data)
    o_key <- "ORIG_" %p% attr_key_nodes(orig_data)
    pair_data <- merge(
      pair_data, orig_data,
      by.x = od_keys[1],
      by.y = o_key,
      all.x = TRUE,
      sort = FALSE)

    col_order <- order(pair_data[[od_keys[1]]], pair_data[[od_keys[2]]])
    name_order <- unique(c(
      od_keys,
      setdiff(names(orig_data), o_key),
      setdiff(names(dest_data), d_key),
      names(pair_data)))

    return(pair_data[col_order, name_order])
})

setValidity("sp_multi_network", function(object) { # ---- validity ------------

  ### check validity of pairs and nodes
  lapply(compact(object@network_pairs), "assert_is", "sp_network_pair")
  lapply(object@network_pairs, "validObject")
  lapply(compact(object@networks), "assert_is", "sp_network_nodes")
  lapply(object@networks, "validObject")

  ### check consistency of pairs and nodes...
  # ... naming: networks
  network_names <- lapply(object@networks, slot, name = "network_id")
  pair_names <- lapply(object@network_pairs, slot, name = "network_pair_id")
  unique_names <-
    has_distinct_elements(network_names) & has_distinct_elements(pair_names)
  if (!unique_names) {
    error_msg <-
      "The identification of all networks and network_pairs must be unique!"
    return(error_msg)
  }

  # ... identification: nodes to origins and destinations
  check_pair_key_levels <- function(pair_ids){
    this_pair <- pair_ids["pair"]
    this_orig <- pair_ids["orig"]
    this_dest <- pair_ids["dest"]

    err_msg <-
      "The %ss in the network pair can not be identifyed with the nodes " %p%
      "in the %s network. Please make sure that the all ids are present " %p%
      "in the network nodes and the network pairs and check for NA values!"
    if (this_orig %in% all_ids$networks) {
      o_key <- attr_key_orig(dat(pull_member(object, this_pair)))
      o_levels <- dat(pull_member(object, this_pair))[[o_key]]
      o_key_target <- attr_key_nodes(dat(pull_member(object, this_orig)))
      o_levels_target <- dat(pull_member(object, this_pair))[[o_key_target]]

      if (!all(levels(o_levels_target) == levels(o_levels))
          || any(is.na(o_levels_target))
          || any(is.na(o_levels)))
        return(sprintf(err_msg, "origin", "origin"))
    }

    if (this_dest %in% all_ids$networks) {
      d_key <- attr_key_orig(dat(pull_member(object, this_pair)))
      d_levels <- dat(pull_member(object, this_pair))[[d_key]]
      d_key_target <- attr_key_nodes(dat(pull_member(object, this_dest)))
      d_levels_target <- dat(pull_member(object, this_pair))[[d_key_target]]

      if (!all(levels(d_levels_target) == levels(d_levels))
          || any(is.na(d_levels_target))
          || any(is.na(d_levels)))
        return(sprintf(err_msg, "destination", "destination"))
    }
  }
  all_ids <- id(object)
  all_pairs <- names(all_ids[["network_pairs"]])
  lapply(all_pairs, "check_pair_key_levels")

  # object is valid
  return(TRUE)
})



# ---- Constructors -----------------------------------------------------------

#' Create an S4 class that contains [sp_network_nodes()] and [sp_network_pair()] for one or multiple networks
#'
#' @param ... objects of type [sp_network_nodes()] and [sp_network_pair()]
#'
#' @return An S4 class of type [sp_multi_network-class()]
#' @family Constructors for spflow network classes
#' @export
#' @examples
#' sp_multi_network() # empty
#' sp_multi_network(germany_net,usa_net) # two networks, no pairs
sp_multi_network <- function(...) {

  input_nets <- unlist(list(...)) %||% list()
  is_net <- ulapply(input_nets, is, class2 = "sp_network_nodes")
  is_pair <- ulapply(input_nets, is, class2 = "sp_network_pair")

  assert(all(is_net | is_pair),warn = TRUE,
         "All supplied objects which are not of class sp_network or " %p%
         "sp_network_pair are discarded!")
  sp_networks <- input_nets[is_net]
  sp_network_pairs <- input_nets[is_pair]

  names(sp_networks) <- ulapply(sp_networks, "id")
  pair_ids <- lapply(sp_network_pairs, "id")
  names(sp_network_pairs) <- ulapply(pair_ids, "[", "pair", use.names = FALSE)

  # check if the node identification between networks and pairs are consistent
  # error & warning template for origins and destinations
  err_msg <-
    "Some of the %ss in the network pair object are not be identifyed " %p%
    "with the nodes in the %s network."
  warn <-
    "The %ss in the network pair object were reordered to match the " %p%
    "order of the nodes in the %s network."
  for (i in seq_along(pair_ids)) {
    this_pair <- pair_ids[[i]]["pair"]

    this_orig <- pair_ids[[i]]["orig"]
    if (this_orig %in% names(sp_networks)) {
      o_key <-
        attr_key_orig(dat(sp_network_pairs[[this_pair]]))
      o_key_levels <-
        levels(dat(sp_network_pairs[[this_pair]])[[o_key]])
      o_key_target <-
        attr_key_nodes(dat(sp_networks[[this_orig]]))
      o_key_target_levels <-
        levels(dat(sp_networks[[this_orig]])[[o_key_target]])

      assert(all(o_key_levels %in% o_key_target_levels),
             err_msg, "origin", "origin")

      if (!all(o_key_levels == o_key_target_levels)) {
        warning(sprintf(warn, "origin", "origin"))
        sp_network_pairs[[this_pair]]@pair_data[[o_key]] <-
          factor(sp_network_pairs[[this_pair]]@pair_data[[o_key]],
                 o_key_target_levels)
      }
    }

    this_dest <- pair_ids[[i]]["dest"]
    if (this_dest %in% names(sp_networks)) {
      d_key <-
        attr_key_dest(dat(sp_network_pairs[[this_pair]]))
      d_key_levels <-
        levels(dat(sp_network_pairs[[this_pair]])[[d_key]])
      d_key_target <-
        attr_key_nodes(dat(sp_networks[[this_dest]]))
      d_key_target_levels <-
        levels(dat(sp_networks[[this_dest]])[[d_key_target]])

      assert(all(d_key_levels %in% d_key_target_levels),
             err_msg, "destination", "destination")

      if (!all(d_key_levels == d_key_target_levels)) {
        warning(sprintf(warn,"destination", "destination"))
        sp_network_pairs[[this_pair]]@pair_data[[d_key]] <-
          factor(sp_network_pairs[[this_pair]]@pair_data[[d_key]],
                 d_key_target_levels)
      }
    }
  }

  return(new("sp_multi_network",
             networks = sp_networks,
             network_pairs = sp_network_pairs))
}

# ---- Helpers ----------------------------------------------------------------

#' @keywords internal
check_pair_completeness <- function(multi_net) {

  all_ids <- id(multi_net)

  pair_meta_data <- lapply(
    all_ids$network_pairs,
    function(.p_id) {
      od_id <- split_pair_id(.p_id)
      od_nnodes <- nnodes(multi_net@network_pairs[[.p_id]])
      has_o_id <- od_id[1] %in% all_ids$networks
      has_d_id <- od_id[2] %in% all_ids$networks

      data.frame(
        "ID_NET_PAIR" =
          .p_id,
        "NPAIRS" =
          npairs(multi_net@network_pairs[[.p_id]]),
        "COMPLETENESS" =
          npairs(multi_net@network_pairs[[.p_id]]) / prod(od_nnodes),
        "ID_ORIG_NET" = ifelse(has_o_id, od_id[1], "(missing)"),
        "ORIG_NNODES" = ifelse(has_o_id, od_nnodes[1], "(missing)"),
        "ID_DEST_NET" = ifelse(has_o_id, od_id[2], "(missing)"),
        "DEST_NNODES" = ifelse(has_o_id, od_nnodes[2], "(missing)"),
        row.names = NULL)})

  Reduce("rbind", pair_meta_data)

}

#' @keywords internal
pull_od_levels <- function(sp_net_pair, o_vs_d = "orig") {
  get_key <- match.fun("attr_key_" %p% o_vs_d)
  key <- get_key(sp_net_pair)
  levels(dat(sp_net_pair)[[key]])
}

#' @keywords internal
id_part <- function(.id, o_vs_d) {
  o_vs_d <- c("orig" = 1, "dest" = 2)[o_vs_d]
  unlist(strsplit(.id,"_"))[o_vs_d]
}


