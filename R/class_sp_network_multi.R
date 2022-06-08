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
setClass("sp_multi_network", slots = c(
  networks = "list",
  network_pairs = "list"))
setClassUnion("maybe_sp_multi_network", c("NULL", "sp_multi_network"))


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
#' @param network_pair_ids
#'   A character indicating of one or several [sp_network_pair()] objects
#' @rdname sp_multi_network-class
#' @export
setMethod(
  f = "complete_pairs",
  signature = "sp_multi_network",
  function(
    object,
    network_pair_ids = id(object)[["network_pairs"]]) {

    assert_is(network_pair_ids, "character")
    od_infos <- lapply(network_pair_ids, check_pair_completeness, object)
    od_infos <- Filter(function(x) {
      x[["NPAIRS"]] < prod(x[c("ORIG_NNODES", "DEST_NNODES")])
      }, od_infos)


    for (i in seq_along(od_infos)) {
      p_id <- od_infos[[i]][["ID_NET_PAIR"]]
      object@network_pairs[[p_id]]@pair_data <- pair_merge(
        object,
        network_pair_id = p_id,
        make_cartesian = TRUE,
        pair_cols = names(dat(object, p_id)),
        dest_cols = NULL,
        orig_cols = NULL)
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

# ---- ... flow_moran_plots  ---------------------------------------------------
#' @title Plot the map of flows
#' @name flow_moran_plots
#' @rdname sp_multi_network-class
setMethod(
  f = "flow_moran_plots",
  signature = "sp_multi_network",
  function(object,
           network_pair_id = id(object)[["network_pairs"]][[1]],
           flow_var,
           model = "model_9",
           DW,
           OW,
           add_lines = TRUE) {



    if (missing(DW) | missing(OW)){
      nb_dat <- pull_spflow_neighborhood(object,network_pair_id)
      if (missing(OW)) OW <- nb_dat[["OW"]]
      if (missing(DW)) DW <- nb_dat[["DW"]]
    }

    assert(model != "model_1", "The Moran plot is for spatial models!")
    assert_is_single_x(flow_var, "character")
    assert(flow_var %in% names(dat(object, network_pair_id)),
           "Variable %s not found in network pair %s!", flow_var, network_pair_id)

    # create lags for the flow_var in matrix form
    flows_v <- dat(object, network_pair_id)[[flow_var]]
    valid_flows <- is.finite(flows_v)
    flows_v <- flows_v[valid_flows]

    do_keys <- get_do_keys(dat(object, network_pair_id))[valid_flows,,drop = FALSE]
    flows_indicator <- matrix_format_d_o(
      dest_index = as.integer(do_keys[,1]),
      orig_index = as.integer(do_keys[,2]),
      num_dest = nlevels(do_keys[,1]),
      num_orig = nlevels(do_keys[,2]),
      assume_ordered = TRUE)

    flows_m <- matrix_format_d_o(
      flows_v,
      dest_index = as.integer(do_keys[,1]),
      orig_index = as.integer(do_keys[,2]),
      num_dest = nlevels(do_keys[,1]),
      num_orig = nlevels(do_keys[,2]),
      assume_ordered = TRUE)

    flows_lag <- lag_flow_matrix(
      Y = flows_m,
      model = model,
      OW = OW,
      DW = DW,
      name = flow_var,
      flow_indicator = flows_indicator)

    flow_lag <- lapply(
      flows_lag[-1],
      FUN = "spflow_mat2format",
      do_keys = do_keys,
      type = "V")

    for (i in seq_along(flow_lag)) {
      Wi <- sub(paste0(flow_var, "."),replacement = "", names(flow_lag)[i])

      plot(y = flow_lag[[i]], x = flows_v,
           main = "Moran scatterplot",
           xlab = flow_var,
           ylab = bquote(W[.(Wi)] %*% .(flow_var) (lag)))
      if (add_lines)
        abline(lm.fit(x = cbind(1,flows_v), y = flow_lag[[i]]), col = "red")
    }
  })


# ---- ... flow_map  ----------------------------------------------------------
#' @title Plot the map of flows
#' @name flow_map
#' @rdname sp_multi_network-class
setMethod(
  f = "flow_map",
  signature = "sp_multi_network",
  function(object,
           network_pair_id = id(object)[["network_pairs"]][[1]],
           ...,
           flow_var) {

    assert(network_pair_id %in% id(object)[["network_pairs"]])
    flow_data <- pull_spflow_data(object, network_pair_id)

    assert_is_single_x(flow_var, "character")
    assert(flow_var %in% names(flow_data[["pair"]]),
           "Variable %s not found in network pair %s!", flow_var, network_pair_id)

    do_indexes <- get_do_keys(flow_data[["pair"]])
    flow_var <- flow_data[["pair"]][,flow_var]
    args <- list(
      "y" = flow_var,
      "index_o" = do_indexes[[2]],
      "index_d" = do_indexes[[1]])
    args <- c(args, list(...))

    if (is.null(args[["coords_s"]]))
      args[["coords_s"]] <- get_node_coords(object, network_pair_id)

    do.call("map_flows", args)
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
#' @param object
#'   A [sp_multi_network-class()]
#' @param network_pair_id
#'   A character indicating the id of a [sp_network_pair-class()]
#' @param flow_formula
#'   A formula specifying how variables should be used
#'   (for details see section Formula interface in the help page of [spflow()])
#' @param add_lags_x
#'   A logical, indicating whether spatial lags of the exogenous variables
#'   should be included.
#' @param add_lags_y
#'   A logical, indicating whether spatial lags of the dependent variables
#'   should be included.
#'
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
           network_pair_id = id(object)[["network_pairs"]][[1]] ,
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

    mat <- derive_spflow_matrices(
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
#' @param dest_cols
#'   A character, indicating the columns to be kept in the final data.frame
#'   that contain information on the nodes in the destination network.
#' @param orig_cols
#'   A character, indicating the columns to be kept in the final data.frame
#'   that contain information on the nodes in the origin network.
#' @param pair_cols
#'   A character, indicating the columns to be kept in the final data.frame
#'   that contain information on the origin-destination pairs.
#' @param keep_od_keys
#'   A logical, indicating whether the id of the od should be included.
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
           network_pair_id = id(object)[["network_pairs"]][[1]],
           dest_cols = NULL,
           orig_cols = NULL,
           pair_cols = NULL,
           make_cartesian = FALSE,
           keep_od_keys = TRUE) {

    od_ids <- id(object)[["network_pairs"]]
    assert(network_pair_id %in% od_ids,
           "No network pair with id %s!", network_pair_id)
    assert_is_single_x(make_cartesian, "logical")

    flow_data <- pull_spflow_data(object, network_pair_id)
    flow_infos <- check_pair_completeness(network_pair_id, object)
    do_keys <- subset_keycols(flow_data[["pair"]], drop_keys = FALSE)
    do_indexes <- lapply(do_keys, "as.integer")

    select_nonid_cols <- function(.cols,.data) {


      if (missing(.cols))
        return(subset_keycols(flow_data[[.data]]))

      if (is.null(.cols))
        return(NULL)

      assert_is(.cols, "character")

      has_cols <- .cols %in% colnames(flow_data[[.data]])
      assert(all(has_cols),
             "Columns not available in the %s-data: %s",
             .data, paste0(.cols[!has_cols], collapse = ", "))

      attr_coord_col(flow_data[[.data]]) <- NULL
      .data <- subset_keycols(flow_data[[.data]])
      .cols <- intersect(.cols, colnames(.data))
      return(.data[,.cols,drop = FALSE])
    }
    pair_data <- select_nonid_cols(pair_cols, "pair")
    dest_data <- select_nonid_cols(dest_cols, "dest")
    orig_data <- select_nonid_cols(orig_cols, "orig")

    # complete missing OD pairs (fill with NA's)
    make_cartesian <- make_cartesian & flow_infos$COMPLETENESS < 1
    if (make_cartesian) {
      n_o <- flow_infos[["ORIG_NNODES"]]
      n_d <- flow_infos[["DEST_NNODES"]]
      pair_index <- compute_pair_index_do(
        d_index = do_indexes[[1]],
        o_index = do_indexes[[2]],
        n_d = n_d)
      do_indexes <- list(
        rep(seq_len(n_d), times = n_o),
        rep(seq_len(n_o), each = n_d))

      if (length(pair_data) == 0)
        pair_data <- data.frame(row.names = seq_len(n_o * n_d))

      if (length(pair_data) > 0) {
        pair_data_old <- pair_data
        pair_data <- data.frame(row.names = seq_len(n_o * n_d))
        na_data <- lapply(pair_data_old[1,,drop = FALSE], "is.na<-")
        pair_data[,names(na_data)] <- na_data
        pair_data[pair_index,] <- pair_data_old
      }

      if (keep_od_keys) {
        d_keys <- factor_in_order(levels(do_keys[[1]]))
        o_keys <- factor_in_order(levels(do_keys[[2]]))
        key_names <- names(do_keys)
        do_keys <- data.frame(
          d_keys[do_indexes[[1]]],
          o_keys[do_indexes[[2]]])
        colnames(do_keys) <- key_names
      }
    }

    all_pair_infos <- named_list(c("ID","P","D","O"))
    all_pair_infos[["ID"]] <- do_keys %T% keep_od_keys
    all_pair_infos[["P"]] <- pair_data %||% NULL
    all_pair_infos[["D"]] <-
      prefix_columns(dest_data, "DEST_")[do_indexes[[1]],,drop = FALSE] %T%
      (length(dest_data) > 0)
    all_pair_infos[["O"]] <-
      prefix_columns(orig_data, "ORIG_")[do_indexes[[2]],,drop = FALSE] %T%
      (length(orig_data) > 0)

    pair_data <- Reduce("cbind", all_pair_infos)
    row.names(pair_data) <- NULL
    if (keep_od_keys)
      attr_key_do(pair_data) <- colnames(do_keys)

    if (any(sapply(flow_data, inherits, "data.table")) && require("data.table"))
      pair_data <- data.table::as.data.table(pair_data)

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
  all %ss with the nodes in the %s network!"

  pair_ids <- lapply(sp_network_pairs, "id")

  for (i in seq_along(pair_ids)) {

    net_pair <- pair_ids[[i]]["pair"]
    do_keys <- dat(sp_network_pairs[[net_pair]]) %|!|% get_do_keys
    wrong_od_order <- FALSE

    # check for origins
    orig_net <- pair_ids[[i]]["orig"]
    orig_keys <- orig_keys_od <- unique(do_keys[[2]])
    if (orig_net %in% names(sp_networks)) {
      orig_keys <- subset_keycols(dat(sp_networks[[orig_net]]), drop_keys = FALSE)[[1]]
      assert(all(as.character(orig_keys_od) %in% as.character(orig_keys)),
             error_template, net_pair, "origin", "origin")

      wrong_od_order <-
        length(levels(orig_keys_od)) != length(levels(orig_keys)) ||
        any(levels(orig_keys_od) != levels(orig_keys))
    }

    dest_net <- pair_ids[[i]]["dest"]
    dest_keys <- dest_keys_od <- unique(do_keys[[1]])
    if (dest_net %in% names(sp_networks)) {
      dest_keys <- subset_keycols(dat(sp_networks[[dest_net]]), drop_keys = FALSE)[[1]]
      assert(all(as.character(dest_keys_od) %in% as.character(dest_keys)),
             error_template, net_pair, "destination", "destination")
      wrong_od_order <- any(c(wrong_od_order, levels(dest_keys_od) != levels(dest_keys)))
    }

    if (wrong_od_order) {
      warn_template <- "
      The the data in the network pair %s is reordered!"
      assert(FALSE, warn_template, net_pair, warn = TRUE)
      do_key_cols <- names(do_keys)
      do_keys <- Map(factor, x = do_keys, levels = list(dest_keys, orig_keys))
      pdat_new <- dat(sp_network_pairs[[net_pair]])
      pdat_new[do_key_cols] <- do_keys
      pdat_new <- pdat_new[order(do_keys[[2]], do_keys[[1]]),,drop = FALSE]
      if (inherits(pdat_new, "data.table") && require("data.table"))
        pdat_new <- data.table::as.data.table(pdat_new)
      dat(sp_network_pairs[[net_pair]]) <- pdat_new
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
    HAS_NB = !is.null(neighborhood(sp_net))
    , row.names = FALSE)
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

#' @keywords internal
get_node_coords <- function(sp_multi, net_pair_id) {

  od_ids <- id(pull_member(sp_multi, net_pair_id))

  orig_coords <- subset_keycols(dat(sp_multi, od_ids["orig"]), drop_keys = FALSE)
  assert(ncol(orig_coords) == 3, "No valid coordinates!")

  if (od_ids["orig"] == od_ids["dest"])
    return(data.frame(orig_coords[,-1], row.names = orig_coords[,1]))

  dest_coords <- subset_keycols(dat(sp_multi, od_ids["dest"]), drop_keys = FALSE)
  assert(ncol(dest_coords) == 3, "No valid coordinates!")
  colnames(dest_coords) <- colnames(orig_coords)


  node_coords <- unique(rbind(orig_coords, dest_coords))
  return(data.frame(node_coords[,-1], row.names = node_coords[,1]))
}
