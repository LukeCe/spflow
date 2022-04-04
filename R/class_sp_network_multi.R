#' @include class_generics_and_maybes.R class_sp_network_nodes.R class_sp_network_pair.R


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
# ---- ... id -----------------------------------------------------------------
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
  function(object) {
    return(list(
      "networks" = names(slot(object,"networks")),
      "network_pairs" = names(slot(object,"network_pairs")))
      )
  })


# ---- ... complete_pairs -----------------------------------------------------
#' @inheritParams pair_merge
#' @param network_pair_ids
#'   A character indicating of one or several [sp_network_pair()] objects
#' @rdname sp_multi_network-class
#' @export
setMethod(
  f = "complete_pairs",
  signature = "sp_multi_network",
  function(
    object,
    network_pair_ids = id(object)[["network_pairs"]][[1]],
    make_cartesian = FALSE,
    add_od_distance = TRUE,
    dist_function) {

    if (!add_od_distance & !make_cartesian)
      return(object)

    assert_is(network_pair_ids, "character")
    od_infos <- lapply(network_pair_ids, check_pair_completeness, object)

    if (!add_od_distance) {
      od_infos <- Filter(
        function(x) {
          x[["NPAIRS"]] < prod(x[c("ORIG_NNODES", "DEST_NNODES")])
          }, od_infos)
    }

    if (!make_cartesian) {
      od_infos <- Filter(
        function(x) {
          od <- c("ORIG_", "DEST_")
          all(x[paste0(od, "HAS_COORD")]) &
          has_equal_elements(x[paste0(od, "IS_COORD_LONLAT")])
          }, od_infos)
    }

    for (i in seq_along(od_infos)) {
      p_id <- od_infos[[i]][["ID_NET_PAIR"]]
      object@network_pairs[[p_id]]@pair_data <- pair_merge(
        object,
        network_pair_id = p_id,
        make_cartesian = make_cartesian,
        add_od_data = FALSE,
        add_od_distance = add_od_distance,
        add_od_coordinates = FALSE,
        dist_function = dist_function)
      }

    validObject(object)
    return(object)
  })


# ---- ... dat ----------------------------------------------------------------
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
  function(object, .id) {

    assert_is_single_x(.id, "character")

    all_ids <- id(object)
    which_id <- lapply(all_ids, "==", .id)
    assert(sum(unlist(which_id)) == 1,
           "The provided id does not correspond to any network object.")
    from <- names(Filter("any",which_id))
    return(dat(slot(object,from)[[.id]]))
})

# ---- ... dat <- -------------------------------------------------------------
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
setReplaceMethod(
  f = "dat",
  signature = "sp_multi_network",
  function(object, .id, value) {

    assert_is_single_x(.id, "character")

    all_ids <- id(object)
    which_id <- lapply(all_ids, "==", .id)
    assert(sum(unlist(which_id)) == 1,
           "The provided id does not correspond to any network object.")
    from <- names(Filter("any",which_id))
    dat(slot(object,from)[[.id]]) <- value
    return(object)
  })




# ---- ... neighborhood -------------------------------------------------------
#' @rdname sp_multi_network-class
setMethod(
  f = "neighborhood",
  signature = "sp_multi_network",
  function(object, .id = 1) {
    assert(.id %in% id(object)[["networks"]],
           "The provided id does not correspond to any sp_network_nodes.")
    return(neighborhood(object@networks[[.id]]))
  })

# ---- ... nnodes -------------------------------------------------------------
#' @rdname sp_multi_network-class
setMethod(
  signature = "sp_multi_network",
  f = "nnodes",
  function(object, .id) {
    nnodes(pull_member(object, .id))
  })

# ---- ... npairs -------------------------------------------------------------
#' @rdname sp_multi_network-class
setMethod(
  signature = "sp_multi_network",
  f = "npairs",
  function(object, .id) {
    npairs(pull_member(object, .id))
  })


# ---- ... pair_corr ----------------------------------------------------------
#' @title Compute a correlation matrix for OD pairs
#' @description
#' The method computes person correlations for all variables available for the
#' the origins, destinations, and OD-pairs inside for a given network pair.
#' The variables can be adjusted using the formula argument.
#'
#' @param object
#'   A [sp_multi_network-class()]
#' @param network_pair_id
#'   A character indicating the id of a [sp_network_pair-class()]
#' @param flow_formula
#'
#'
#' @return A matrix of pairwise correlations between variables.
#' @rdname pair_corr
#' @export
#' @examples
#' # long form data for flows from Germany to Germany
#' pair_merge(multi_net_usa_ge,"ge_ge")
#'
#' # long form data for flows from Germany to USA
#' pair_merge(multi_net_usa_ge,"ge_usa")
setMethod(
  f = "pair_corr",
  signature = "sp_multi_network",
  function(object,
           network_pair_id =  id(object)[["network_pairs"]][[1]] ,
           flow_formula,
           add_lags_x = TRUE,
           add_lags_y = FALSE) {

    od_ids <- id(object)[["network_pairs"]]
    assert(network_pair_id %in% od_ids,
           "Network pair with id %s was not found!", network_pair_id)

    if (missing(flow_formula))
      flow_formula <- 1 ~ .
    assert_is(flow_formula, "formula")

    flow_control <- list(
      model = ifelse(add_lags_y,"model_9", "model_1"),
      sdm_variables = ifelse(add_lags_x,"same", "none"))

    flow_control <- enhance_flow_control(
      flow_control = flow_control,
      net_pair = pull_member(object, network_pair_id))

    mat <- spflow_model_matrix(
      sp_multi_network = object,
      network_pair_id = network_pair_id,
      flow_formula = flow_formula,
      flow_control = flow_control,
      ignore_na = TRUE)

    mom <- compute_spflow_moments(mat, flow_control,ignore_na = TRUE)
    return(mom[["TCORR"]][-1,-1, drop = FALSE])
  })



# ---- ... pair_merge ---------------------------------------------------------
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
#' @param make_cartesian
#'   A logical, when set to `TRUE` the resulting data.frame contains all
#'   possible pairs of origins and destination, even if the data in the
#'   [sp_network_pair-class()] does not have them.
#' @param add_od_data
#'   A logical, when set to `TRUE` variables describing the origins and
#'   destinations are added to the data.
#' @param add_od_distance
#'   A logical, when set to `TRUE` add the pairwise distances in a column
#'   `DISTANCE`. To make this work the origins and destinations must have
#'   coordinates.
#' @param add_od_coordinates
#'   A logical, when set to `TRUE` the coordinates of origins and destinations
#'   are added to the data.
#' @param dist_function
#'   A function that computes pairwise distances from coordinates, the default
#'   is the great circle distance, for longitude-latitude and euclidean
#'   distance otherwise.
#'   When supplied, this function must take two arguments that are data.frames,
#'   as in `dist_function(coord_a, coord_b)`.
#'
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
  function(object,
           network_pair_id,
           make_cartesian = FALSE,
           add_od_data = TRUE,
           add_od_distance = FALSE,
           add_od_coordinates = FALSE,
           dist_function) {

    od_ids <- id(object)[["network_pairs"]]
    assert(network_pair_id %in% od_ids,
           "Network pair with id %s was not found!", network_pair_id)

    flow_data <- pull_relational_flow_data(object, network_pair_id)
    flow_infos <- check_pair_completeness(network_pair_id, object)
    pair_data <- flow_data[["pair"]]
    orig_data <- flow_data[["orig"]]
    dest_data <- flow_data[["dest"]]
    do_indexes <- pair_data[,attr_key_do(pair_data), drop = FALSE]
    do_indexes <- lapply(do_indexes, "as.integer")

    # complete missing OD pairs
    if (make_cartesian & flow_infos$COMPLETENESS < 1) {
      n_o <- flow_infos[["ORIG_NNODES"]]
      n_d <- flow_infos[["DEST_NNODES"]]
      o_keys <- orig_data[,attr_key_nodes(orig_data)]
      d_keys <- dest_data[,attr_key_nodes(dest_data)]
      pair_data <- data.frame(rep(d_keys, n_o), rep(o_keys, each = n_d))
      colnames(pair_data) <- names(do_indexes)

      other_cols <- setdiff(colnames(flow_data[["pair"]]), colnames(pair_data))
      other_cols <- rbind(flow_data[["pair"]][1,other_cols, drop = FALSE],NA)
      other_cols <- other_cols[2,, drop = FALSE]
      pair_data <- cbind(pair_data, other_cols, row.names = NULL)
      pair_index <- compute_pair_index_do(
        d_index = do_indexes[[1]],
        o_index = do_indexes[[2]],
        n_d = n_d)
      pair_data[pair_index,] <- flow_data[["pair"]][,names(pair_data)]
    }

    # add distances and coordinates
    o_coords <- attr_coord_col(flow_data[["orig"]])
    d_coords <- attr_coord_col(flow_data[["dest"]])
    need_coords <- add_od_distance | add_od_coordinates
    if (need_coords & !is.null(o_coords) & !is.null(d_coords)) {

      coord_data <- list(
        prefix_columns(dest_data[do_indexes[[1]], d_coords, drop = FALSE], "DEST_"),
        prefix_columns(orig_data[do_indexes[[2]], o_coords, drop = FALSE], "ORIG_"))

      if (missing(dist_function)) {
        dist_function <- function(x, y) stop("No distance function available!")

        o_lonlat <- isTRUE(attr_coord_lonlat(orig_data))
        d_lonlat <- isTRUE(attr_coord_lonlat(dest_data))
        if (o_lonlat & d_lonlat)
          dist_function <- function(x, y) haversine_distance(x[[1]], x[[2]], y[[1]], y[[2]])

        if (!o_lonlat & !d_lonlat)
          dist_function <- euclidean_distance
      }

      DISTANCE <- dist_function(coord_data[[1]], coord_data[[2]])
      pair_data <- cbind(
        pair_data,
        DISTANCE,
        coord_data %T% add_od_coordinates,
        row.names = NULL)
    }

    # add remaining variables
    if (add_od_data) {
      orig_data <- subset_keycols(orig_data,drop_keys = TRUE)
      orig_data <- prefix_columns(orig_data, "ORIG_")
      dest_data <- subset_keycols(dest_data,drop_keys = TRUE)
      dest_data <- prefix_columns(dest_data, "DEST_")

      pair_data <- cbind(
        pair_data,
        dest_data[do_indexes[[1]],,drop = FALSE],
        orig_data[do_indexes[[2]],,drop = FALSE], row.names = NULL)
    }

    attr_key_do(pair_data) <- names(do_indexes)
    return(pair_data)
})




# ---- ... pull_member --------------------------------------------------------
#' @rdname sp_multi_network-class
#' @export
#' @examples
#' ## access sp_network_nodes or sp_network_pair inside a sp_multi_network
#'
#' pull_member(multi_net_usa_ge, .id = "ge")
#' pull_member(multi_net_usa_ge, .id = "usa")
#' pull_member(multi_net_usa_ge, .id ="ge_ge")
#'
setMethod(
  f = "pull_member",
  signature = "sp_multi_network",
  function(object, .id = NULL) {

    assert_is_single_x(.id, "character")

    .id_type <- sapply(id(object), function(x) .id %in% x)
    assert(any(.id_type),
           "The provided id does not correspond to any sp_network_nodes " %p%
             "or sp_network_pair bject.")

    from <- names(.id_type[.id_type])
    return(slot(object,from)[[.id]])
  })

# ---- ... show ---------------------------------------------------------------
#' @keywords internal
setMethod(
  f = "show",
  signature = "sp_multi_network",
  function(object){

    cat("Collection of spatial network nodes and pairs")
    cat("\n")
    cat(print_line(50))
    plural <- function(s, x) paste0(s, ifelse(x == 1, "", "s"))
    multi_net_ids <- id(object)


    nodes_ids <- multi_net_ids$networks
    num_nodes <- length(nodes_ids)
    if(num_nodes >= 1) {
      cat("\nContains", num_nodes,
          "spatial network nodes ",
          "\n    With", plural("id", num_nodes), ": ",
          paste(nodes_ids, collapse = ", "))
    }

    pair_ids <- multi_net_ids$network_pairs
    num_pairs <- length(pair_ids)
    if( num_pairs >= 1) {
      cat("\nContains", num_pairs,
          "spatial network pairs ",
          "\n    With", plural("id", num_pairs), ": ",
          paste(pair_ids, collapse = ", "))

      cat("\n\nAvailability of origin-destination pair information:\n\n")
      od_pair_info <- Reduce("rbind", lapply(pair_ids, function(.id) {
        check_pair_completeness(.id, object)}))
      od_pair_info$COMPLETENESS <- format_percent(od_pair_info$COMPLETENESS)

      print(od_pair_info[1:7], row.names = FALSE)
    }

    cat("\n")
    invisible(object)
  })



# ---- ... validity -----------------------------------------------------------
setValidity("sp_multi_network", function(object) {

  ### check validity of pairs and nodes
  lapply(compact(object@network_pairs), "assert_is", "sp_network_pair")
  lapply(object@network_pairs, "validObject")
  lapply(compact(object@networks), "assert_is", "sp_network_nodes")
  lapply(object@networks, "validObject")

  ### check consistency of pairs and nodes...
  # ... naming: networks
  network_names <- lapply(object@networks, slot, name = "network_id")
  pair_names <- lapply(object@network_pairs, slot, name = "network_pair_id")
  unique_names <- has_distinct_elements(c(network_names, pair_names))
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


  warn_template <- "
  All supplied objects which are not of class
  sp_network or sp_network_pair are discarded!"
  is_net <- unlist(lapply(input_nets, is, class2 = "sp_network_nodes"))
  is_pair <- unlist(lapply(input_nets, is, class2 = "sp_network_pair"))
  assert(all(is_net | is_pair), warn_template, warn = TRUE)
  sp_networks <- input_nets[is_net]
  sp_network_pairs <- input_nets[is_pair]


  pair_ids <- lapply(sp_network_pairs, "id")
  pair_ids <- unlist(lapply(pair_ids, "[", "pair"),use.names = FALSE)
  net_ids <- unlist(lapply(sp_networks, "id"))
  names(sp_networks) <- net_ids
  names(sp_network_pairs) <- pair_ids


  ## The class contains relational data for origins, dests, and od-pairs
  # 1. we have to ensure that the identification is correct
  # 2. the order of observations is important for calculations
  error_template <- "
  In the network pair with id %s, it is not possible to identify
  all of the %ss with the nodes in the %s network."

  pair_ids <- lapply(sp_network_pairs, "id")

  for (i in seq_along(pair_ids)) {

    net_pair <- pair_ids[[i]]["pair"]
    pair_keys <- get_keys(sp_network_pairs[[net_pair]])
    wrong_od_order <- FALSE

    # check for origins
    orig_net <- pair_ids[[i]]["orig"]
    orig_keys <- orig_keys_od <- unique(pair_keys[[1]])
    if (orig_net %in% names(sp_networks)) {
      orig_keys <- get_keys(sp_networks[[orig_net]])[[1]]
      assert(all(as.character(orig_keys_od) %in% as.character(orig_keys)),
             error_template, net_pair, "origin", "origin")

      wrong_od_order <-
        length(levels(orig_keys_od)) != length(levels(orig_keys)) ||
        any(levels(orig_keys_od) != levels(orig_keys))
    }

    dest_net <- pair_ids[[i]]["dest"]
    dest_keys <- dest_keys_od <- unique(pair_keys[[2]])
    if (dest_net %in% names(sp_networks)) {
      dest_keys <- get_keys(sp_networks[[dest_net]])[[1]]
      assert(all(as.character(dest_keys_od) %in% as.character(dest_keys)),
             error_template, net_pair, "destination", "destination")
      wrong_od_order <- any(c(wrong_od_order,
                              levels(dest_keys_od) != levels(dest_keys)))
      }

    if (wrong_od_order) {
      warn_template <- "
      The od-pairs in the network pair object with id %s
      were reordered to match the order of the nodes in the networks."
      assert(FALSE, warn_template, net_pair, warn = TRUE)
      pair_keys[[1]] <- factor(pair_keys[[1]], levels(orig_keys))
      pair_keys[[2]] <- factor(pair_keys[[2]], levels(dest_keys))
      od_order <- order(pair_keys[[1]], pair_keys[[2]])
      od_key_cols <- names(pair_keys)

      new_key_data <- dat(sp_network_pairs[[net_pair]])
      new_key_data[od_key_cols[1]] <- pair_keys[[1]]
      new_key_data[od_key_cols[2]] <- pair_keys[[2]]
      dat(sp_network_pairs[[net_pair]]) <- new_key_data[od_order,]
    }
  }


  return(new("sp_multi_network",
             networks = sp_networks,
             network_pairs = sp_network_pairs))
}

# ---- Helpers ----------------------------------------------------------------

#' @keywords internal
check_pair_completeness <- function(pair_id, multi_net) {

  all_ids <- id(multi_net)
  this_pair <- pull_member(multi_net, pair_id)
  od_id <- id(this_pair)
  od_nnodes <- nnodes(this_pair)
  o_nnodes <- nnodes(pull_member(multi_net, od_id["orig"]))
  d_nnodes <- nnodes(pull_member(multi_net, od_id["dest"]))


  pair_infos <- data.frame(
    "ID_ORIG_NET" = od_id["orig"],
    "ID_DEST_NET" = od_id["dest"],
    "ID_NET_PAIR" = pair_id,
    "COMPLETENESS" = npairs(this_pair) / prod(od_nnodes),
    "C_PAIRS" = sprintf("%s/%s", npairs(this_pair), o_nnodes * d_nnodes),
    "C_ORIG" = sprintf("%s/%s", o_nnodes, od_nnodes["orig"]),
    "C_DEST" = sprintf("%s/%s", d_nnodes, od_nnodes["dest"]),
    "NPAIRS" = npairs(this_pair),
    "NORIG" = od_nnodes["orig"],
    "NDEST" = od_nnodes["dest"],
    "ORIG_NNODES" = o_nnodes,
    "DEST_NNODES" = d_nnodes)


  o_infos <- d_infos <- NULL
  if (od_id["orig"] %in% all_ids$networks) {
    o_infos <- check_node_infos(pull_member(multi_net, od_id["orig"]))
    names(o_infos) <- paste0("ORIG_", names(o_infos))
  }

  if (od_id["dest"] %in% all_ids$networks) {
    d_infos <- check_node_infos(pull_member(multi_net, od_id["dest"]))
    names(d_infos) <- paste0("DEST_", names(d_infos))
  }

  return(cbind(pair_infos, o_infos, d_infos, row.names = NULL))
}

#' @keywords internal
check_node_infos <- function(sp_net) {

  data.frame(
    HAS_DATA = !is.null(dat(sp_net)),
    HAS_NB = !is.null(neighborhood(sp_net)),
    HAS_COORD = is.character(attr_coord_col(sp_net)),
    IS_COORD_LONLAT = isTRUE(attr_coord_lonlat(sp_net))
    , row.names = FALSE)
}



#' @keywords internal
validate_od_keys <- function(o_keys, d_keys, od_keys) {

  error_template <- "The %s must be unique!"
  assert(has_distinct_elements(o_keys),
         "The %s keys must be unique!", "origin")
  assert(has_distinct_elements(d_keys),
         "The %s keys must be unique!", "destination")


}

#' @keywords internal
pull_node_coords <- function(sp_dat) {
  sp_dat[attr_coord_col(sp_dat)] %||% NULL
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

#' @keywords internal
compute_pair_index_do <- function(d_index, o_index, n_d) {
  pair_index <- d_index + (o_index - 1) * n_d
  return(pair_index)
}

#' @keywords internal
derive_pair_index <- function(pair_dat, n_d) {
  od_indexes <-  pair_dat[,attr_key_od(pair_dat), drop = FALSE]
  compute_pair_index_do(od_indexes[[1]], od_indexes[[2]], n_d)
}
