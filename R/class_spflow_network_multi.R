#' @include class_generics_and_maybes.R class_spflow_network_nodes.R class_spflow_network_pairs.R

#' @title Class spflow_network_multi
#'
#' @description
#' An S4 class that gathers information on multiple [spflow_network_nodes()] and [spflow_network_pairs()].
#' Its purpose is to ensure that the identification between the nodes that
#' serve as origins or destinations, and the OD-pairs is consistent
#' (similar to relational data bases).
#'
#' @slot nodes A list of [spflow_network_nodes-class()] objects
#' @slot pairs A list of [spflow_network_pairs-class()] objects
#'
#' @param object spflow_network_multi-class
#' @family spflow network classes
#' @name spflow_network_multi-class
#' @export
setClass("spflow_network_multi", slots = c(
  nodes = "list",
  pairs = "list"))
setClassUnion("maybe_spflow_network_multi", c("NULL", "spflow_network_multi"))


# ---- Methods ----------------------------------------------------------------
# ---- ... id -----------------------------------------------------------------
#' @rdname spflow_network_multi-class
#' @export
#' @examples
#' ## access the id of the objects inside the spflow_network_multi
#'
#' id(multi_net_usa_ge)$nodes
#' id(multi_net_usa_ge)$pairs
#'
setMethod(
  f = "id",
  signature = "spflow_network_multi",
  function(object) {
    return(list(
      "nodes" = names(slot(object,"nodes")),
      "pairs" = names(slot(object,"pairs")))
      )
  })


# ---- ... complete_pairs -----------------------------------------------------
#' @param ids_spflow_pairs
#'   A character indicating of one or several [spflow_network_pairs()] objects
#' @rdname spflow_network_multi-class
#' @export
setMethod(
  f = "complete_pairs",
  signature = "spflow_network_multi",
  function(
    object,
    ids_spflow_pairs = id(object)[["pairs"]]) {

    assert_is(ids_spflow_pairs, "character")
    od_infos <- lapply(ids_spflow_pairs, check_pair_completeness, object)
    od_infos <- Filter(function(x) {
      x[["NPAIRS"]] < prod(x[c("ORIG_NNODES", "DEST_NNODES")])
      }, od_infos)


    for (i in seq_along(od_infos)) {
      p_id <- od_infos[[i]][["ID_NET_PAIR"]]
      object@pairs[[p_id]]@pair_data <- pair_merge(
        object,
        id_spflow_pairs = p_id,
        make_cartesian = TRUE,
        pair_cols = names(dat(object, p_id)),
        dest_cols = NULL,
        orig_cols = NULL)
    }

    validObject(object)
    return(object)
  })

# ---- ... dat ----------------------------------------------------------------
#' @rdname spflow_network_multi-class
#' @param .id A character indicating the id of a [spflow_network_nodes-class()] or a
#'   [spflow_network_pairs-class()] inside the [spflow_network_multi-class()].
#' @export
#' @examples
#' ## access the data inside a spflow_network_multi
#'
#' dat(multi_net_usa_ge, "ge")    # extract data of nodes
#' dat(multi_net_usa_ge, "ge_ge") # extract data of pairs
#'
setMethod(
  f = "dat",
  signature = "spflow_network_multi",
  function(object, .id) {

    assert_is_single_x(.id, "character")

    all_ids <- id(object)
    which_id <- lapply(all_ids, "==", .id)
    assert(sum(unlist(which_id)) == 1, ".id not found!")
    from <- names(Filter("any",which_id))
    return(dat(slot(object,from)[[.id]]))
})

# ---- ... dat <- -------------------------------------------------------------
#' @rdname spflow_network_multi-class
#' @param .id A character indicating the id of a [spflow_network_nodes-class()] or a
#'   [spflow_network_pairs-class()] inside the [spflow_network_multi-class()].
#' @param value A data.frame to replace the existing data
#' @export
#' @examples
#' ## access the data of a network or a network_pair inside a multi_network
#'
#' dat(multi_net_usa_ge, "ge")    # extract data of nodes
#' dat(multi_net_usa_ge, "ge_ge") # extract data of pairs
#'
setReplaceMethod(
  f = "dat",
  signature = "spflow_network_multi",
  function(object, .id, value) {

    assert_is_single_x(.id, "character")

    all_ids <- id(object)
    which_id <- lapply(all_ids, "==", .id)
    assert(sum(unlist(which_id)) == 1, ".id not found!")
    from <- names(Filter("any",which_id))
    dat(slot(object,from)[[.id]]) <- value
    return(object)
  })

# ---- ... neighborhood -------------------------------------------------------
#' @rdname spflow_network_multi-class
setMethod(
  f = "neighborhood",
  signature = "spflow_network_multi",
  function(object, .id) {
    assert(.id %in% id(object)[["nodes"]], ".id not found among spflow_network_nodes!")
    return(neighborhood(object@nodes[[.id]]))
  })

# ---- ... nnodes -------------------------------------------------------------
#' @param .id
#'   A character, corresponding to the id of a `spflow_network_nodes()` object
#'   contained in the `spflow_network_multi`
#' @rdname spflow_network_multi-class
setMethod(
  signature = "spflow_network_multi",
  f = "nnodes",
  function(object, .id) {
    nnodes(pull_member(object, .id))
  })

# ---- ... npairs -------------------------------------------------------------
#' @rdname spflow_network_multi-class
setMethod(
  signature = "spflow_network_multi",
  f = "npairs",
  function(object, .id) {
    npairs(pull_member(object, .id))
  })


# ---- ... pair_cor ----------------------------------------------------------
#' @param id_spflow_pairs
#'   A character indicating the id of a [spflow_network_pairs-class()]
#' @param spflow_formula
#'   A formula specifying how variables should be used
#'   (for details see section Formula interface in the help page of [spflow()])
#' @param add_lags_x
#'   A logical, indicating whether spatial lags of the exogenous variables
#'   should be included.
#' @param add_lags_y
#'   A logical, indicating whether spatial lags of the dependent variables
#'   should be included.
#'
#' @rdname pair_cor
#' @export
#' @examples
#'
#' # Used with a spflow_network_multi ...
#' cor_mat <- pair_cor(multi_net_usa_ge, "ge_ge") # without transformations
#' cor_image(cor_mat)
#'
#' cor_mat <- pair_cor( # with transformations and spatial lags
#'   multi_net_usa_ge,
#'   "ge_ge",
#'   y9 ~ . + P_(log(DISTANCE + 1) + .),
#'   add_lags_y = TRUE)
#' cor_image(cor_mat)
#'
setMethod(
  f = "pair_cor",
  signature = "spflow_network_multi",
  function(object,
           id_spflow_pairs = id(object)[["pairs"]][[1]] ,
           spflow_formula,
           add_lags_x = TRUE,
           add_lags_y = FALSE) {

    pair_ids <- id(object)[["pairs"]]
    assert_is_single_x(id_spflow_pairs, "character")
    assert(id_spflow_pairs %in% pair_ids,
           "spflow_network_pairs with id %s was not found!", id_spflow_pairs)
    od_id <- id(pull_member(object, id_spflow_pairs))

    if (missing(spflow_formula))
      spflow_formula <- 1 ~ .
    assert_is(spflow_formula, "formula")

    estimation_control <- list(
      model = ifelse(add_lags_y,"model_9", "model_1"),
      sdm_variables = ifelse(add_lags_x,"same", "none"))
    estimation_control <- enhance_spflow_control(
      estimation_control = estimation_control,
      is_within = od_id["orig"] == od_id["dest"])

    spflow_matrices <- derive_spflow_matrices(
      id_spflow_pairs = id_spflow_pairs,
      spflow_networks = object,
      spflow_formula = spflow_formula,
      spflow_control = estimation_control,
      na_rm = TRUE)

    spflow_indicators <- spflow_matrices[["spflow_indicators"]]
    spflow_matrices[["spflow_indicators"]] <- NULL
    spflow_obs <- spflow_indicators2obs(spflow_indicators)
    wt <- spflow_indicators2mat(spflow_indicators, do_filter = "IN_SAMPLE", do_values = "WEIGHTS")
    mom <- derive_spflow_moments(
      spflow_matrices = spflow_matrices,
      n_o = spflow_obs[["N_orig"]],
      n_d = spflow_obs[["N_dest"]],
      N = spflow_obs[["N_sample"]],
      wt = wt,
      na_rm = TRUE)


    N <- mom[["N"]]
    UU <- mom[["UU"]]
    UY <- mom[["UY"]]
    TSS <- mom[["TSS"]]

    # covariance matrix
    TCORR <- rbind(cbind(UU,UY), cbind(UY %|!|% t, TSS))
    TCORR <- TCORR - (outer(TCORR[1,], TCORR[1,])/N)
    TCORR <- TCORR / outer(sqrt(diag(TCORR)), sqrt(diag(TCORR)))
    diag(TCORR[-1,-1]) <- 1

    return(TCORR)
  })

# ---- ... pair_merge ---------------------------------------------------------
#' @param object
#'   A [spflow_network_multi-class()]
#' @param id_spflow_pairs
#'   A character indicating the id of a [spflow_network_pairs-class()]
#' @param make_cartesian
#'   A logical, when set to `TRUE` the resulting data.frame contains all
#'   possible pairs of origins and destination, even if the data in the
#'   [spflow_network_pairs-class()] does not have them.
#' @param dest_cols
#'   A character, indicating the columns to be kept in the final data.frame
#'   that contain information on the nodes in the destination network.
#'   (TRUE is a shortcut for everything).
#' @param orig_cols
#'   A character, indicating the columns to be kept in the final data.frame
#'   that contain information on the nodes in the origin network.
#'   (TRUE is a shortcut for everything).
#' @param pair_cols
#'   A character, indicating the columns to be kept in the final data.frame
#'   that contain information on the origin-destination pairs.
#'   (TRUE is a shortcut for everything).
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
  signature = "spflow_network_multi",
  function(object,
           id_spflow_pairs = id(object)[["pairs"]][[1]],
           dest_cols = NULL,
           orig_cols = NULL,
           pair_cols = NULL,
           make_cartesian = FALSE,
           keep_od_keys = TRUE) {

    od_ids <- id(object)[["pairs"]]
    assert(id_spflow_pairs %in% od_ids, "no spflow_network_pairs with id %s!", id_spflow_pairs)
    assert_is_single_x(make_cartesian, "logical")

    flow_data <- pull_spflow_data(object, id_spflow_pairs)
    flow_infos <- check_pair_completeness(id_spflow_pairs, object)
    do_keys <- subset_keycols(flow_data[["pair"]], drop_keys = FALSE)
    do_indexes <- lapply(do_keys, "as.integer")

    select_nonid_cols <- function(.cols,.data) {


      if (isTRUE(.cols))
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
      prefix_columns(dest_data, "D_")[do_indexes[[1]],,drop = FALSE] %T%
      (length(dest_data) > 0)
    all_pair_infos[["O"]] <-
      prefix_columns(orig_data, "O_")[do_indexes[[2]],,drop = FALSE] %T%
      (length(orig_data) > 0)

    pair_data <- Reduce("cbind", all_pair_infos)
    row.names(pair_data) <- NULL
    if (keep_od_keys)
      attr_key_do(pair_data) <- colnames(do_keys)

    if (any(sapply(flow_data, inherits, "data.table")) && requireNamespace("data.table", quietly = TRUE))
      pair_data <- data.table::as.data.table(pair_data)

    return(pair_data)
  })


# ---- ... pull_member --------------------------------------------------------
#' @rdname spflow_network_multi-class
#' @export
#' @examples
#' ## access spflow_network_nodes or spflow_network_pairs inside a spflow_network_multi
#'
#' pull_member(multi_net_usa_ge, .id = "ge")
#' pull_member(multi_net_usa_ge, .id = "usa")
#' pull_member(multi_net_usa_ge, .id ="ge_ge")
#'
setMethod(
  f = "pull_member",
  signature = "spflow_network_multi",
  function(object, .id = NULL) {

    assert_is_single_x(.id, "character")
    .id_type <- sapply(id(object), function(x) .id %in% x)
    .id_type <- names(.id_type[.id_type])

    res <- if (length(.id_type) == 0) NULL else slot(object,.id_type)[[.id]]
    return(res)
  })

# ---- ... show ---------------------------------------------------------------
#' @keywords internal
setMethod(
  f = "show",
  signature = "spflow_network_multi",
  function(object){

    cat("Collection of spatial network nodes and pairs")
    cat("\n")
    cat(print_line(50))
    plural <- function(s, x) paste0(s, ifelse(x == 1, "", "s"))
    multi_net_ids <- id(object)

    nodes_ids <- multi_net_ids$nodes
    num_nodes <- length(nodes_ids)
    if(num_nodes >= 1) {
      cat("\nContains", num_nodes,
          "spatial network nodes ",
          "\n    With", plural("id", num_nodes), ": ",
          paste(nodes_ids, collapse = ", "))
    }

    pair_ids <- multi_net_ids$pairs
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


# ---- ... spflow_map  --------------------------------------------------------
#' @rdname spflow_map
#' @inheritParams spflow
#' @param flow_var A character, indicating one variable from the network pair data
#' @export
#' @examples
#'
#'  # Used with a spflow_network_multi ...
#'  spflow_map(multi_net_usa_ge, "ge_ge",flow_var = "y9")
#'
setMethod(
  f = "spflow_map",
  signature = "spflow_network_multi",
  function(object,
           id_spflow_pairs = id(object)[["pairs"]][[1]],
           ...,
           flow_var) {

    assert(id_spflow_pairs %in% id(object)[["pairs"]])
    flow_data <- pull_spflow_data(object, id_spflow_pairs)

    assert_is_single_x(flow_var, "character")
    assert(flow_var %in% names(flow_data[["pair"]]),
           "Variable %s not found in network pair %s!", flow_var, id_spflow_pairs)

    do_indexes <- get_do_keys(flow_data[["pair"]])
    flow_var <- flow_data[["pair"]][,flow_var]
    args <- list(
      "y" = flow_var,
      "index_o" = do_indexes[[2]],
      "index_d" = do_indexes[[1]])
    args <- c(args, list(...))

    if (is.null(args[["coords_s"]]))
      args[["coords_s"]] <- get_od_coords(object, id_spflow_pairs)

    do.call("map_flows", args)
  })


# ---- ... spflow_moran_plots  ------------------------------------------------
#' @rdname spflow_moran_plots
#' @inheritParams spflow
#' @param flow_var
#'   A character, indicating one variable from the network pair data
#' @export
#' @examples
#'
#'  # Used with a spflow_network_multi ...
#'  # To check the if there is spatial correlation in any variable
#'  spflow_moran_plots(multi_net_usa_ge, "ge_ge",flow_var = "y9")
#'
setMethod(
  f = "spflow_moran_plots",
  signature = "spflow_network_multi",
  function(object,
           id_spflow_pairs = id(object)[["pairs"]][[1]],
           flow_var,
           model = "model_9",
           DW,
           OW,
           add_lines = TRUE) {



    if (missing(DW) | missing(OW)){
      nb_dat <- pull_spflow_neighborhood(object,id_spflow_pairs)
      if (missing(OW)) OW <- nb_dat[["OW"]]
      if (missing(DW)) DW <- nb_dat[["DW"]]
    }

    assert(model != "model_1", "The Moran plot is for spatial models!")
    assert_is_single_x(flow_var, "character")
    assert(flow_var %in% names(dat(object, id_spflow_pairs)),
           "Variable %s not found in network pair %s!", flow_var, id_spflow_pairs)

    # create lags for the flow_var in matrix form
    flows_v <- dat(object, id_spflow_pairs)[[flow_var]]
    valid_flows <- is.finite(flows_v)
    flows_v <- flows_v[valid_flows]

    do_keys <- get_do_keys(dat(object, id_spflow_pairs))[valid_flows,,drop = FALSE]
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
      M_indicator = flows_indicator)

    flow_lag <- lapply(
      flows_lag[-1],
      FUN = "spflow_mat2format",
      do_keys = do_keys,
      return_type = "V")

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


# ---- ... update_dat ---------------------------------------------------------
#' @rdname spflow_network_multi-class
#' @param new_dat
#'   A named list of data.frames that contain the new data.
#'   The names should correspond to spflow_network_nodes or spflow_pair
#'   objects contained in the [spflow_network_multi-class()].
#' @param value A data.frame to replace the existing data
#' @export
#' @examples
#'
#' # update data for individual observations
#' new_ge <- dat(multi_net_usa_ge, "ge")[1:2,1:2]
#' new_ge$X <- new_ge$X * .25
#' multi_net_usa_ge2 <- update_dat(multi_net_usa_ge, list("ge" = new_ge))
#' dat(multi_net_usa_ge2, "ge")
#'
setMethod(
  f = "update_dat",
  signature = "spflow_network_multi",
  function(object, new_dat) {

    assert_is(new_dat, "list")
    assert(all(sapply(new_dat, inherits, "data.frame")),
           "All elements in the new_dat list must be data.frames!")

    all_ids <- id(object)
    assert(all(names(new_dat) %in% unlist(all_ids)),
           "All names of the new_dat list must correspond to an id among %s!",
           deparse(as.character(all_ids)))

    for (i in seq_along(new_dat)) {
      this_id <- names(new_dat)[i]
      this_type <- lapply(all_ids, "==", this_id)
      this_type <- names(Filter("any",this_type))
      slot(object, this_type)[[this_id]] <-
        update_dat(slot(object,this_type)[[this_id]], new_dat[[i]])
    }

    return(object)
  })

# ---- ... validity -----------------------------------------------------------
setValidity("spflow_network_multi", function(object) {

  ### check validity of pairs and nodes
  lapply(compact(object@pairs), "assert_is", "spflow_network_pairs")
  lapply(object@pairs, "validObject")
  lapply(compact(object@nodes), "assert_is", "spflow_network_nodes")
  lapply(object@nodes, "validObject")

  ### check consistency of pairs and nodes...
  # ... naming: nodes
  network_names <- lapply(object@nodes, "id")
  pair_names <- lapply(object@pairs, function(x) id(x)[["pair"]])
  unique_names <- has_distinct_elements(c(network_names, pair_names))
  if (!unique_names) {
    error_msg <-
      "The identification of all nodes and pairs must be unique!"
    return(error_msg)
  }

  # ... identification: nodes to origins and destinations
  check_pair_key_levels <- function(pair_ids){
    this_pair <- pair_ids["pair"]
    this_orig <- pair_ids["orig"]
    this_dest <- pair_ids["dest"]

    err_msg <- "
    The %ss in the spflow_network_pairs cannot be identifyed with the %s nodes.
    All keys musst be present in the splfow_nodes!"
    if (this_orig %in% all_ids$nodes) {
      o_key <- attr_key_orig(dat(pull_member(object, this_pair)))
      o_levels <- dat(pull_member(object, this_pair))[[o_key]]
      o_key_target <- attr_key_nodes(dat(pull_member(object, this_orig)))
      o_levels_target <- dat(pull_member(object, this_pair))[[o_key_target]]

      if (!all(levels(o_levels_target) == levels(o_levels))
          || any(is.na(o_levels_target))
          || any(is.na(o_levels)))
        return(sprintfwrap(err_msg, "origin", "origin"))
    }

    if (this_dest %in% all_ids$nodes) {
      d_key <- attr_key_orig(dat(pull_member(object, this_pair)))
      d_levels <- dat(pull_member(object, this_pair))[[d_key]]
      d_key_target <- attr_key_nodes(dat(pull_member(object, this_dest)))
      d_levels_target <- dat(pull_member(object, this_pair))[[d_key_target]]

      if (!all(levels(d_levels_target) == levels(d_levels))
          || any(is.na(d_levels_target))
          || any(is.na(d_levels)))
        return(sprintfwrap(err_msg, "destination", "destination"))
    }
  }
  all_ids <- id(object)
  all_pairs <- names(all_ids[["pairs"]])
  lapply(all_pairs, "check_pair_key_levels")

  # object is valid
  return(TRUE)
})


# ---- Constructors -----------------------------------------------------------

#' Constructor for the [spflow_network_multi-class()]
#'
#' @param ... objects of type [spflow_network_nodes()] and [spflow_network_pairs()]
#'
#' @return An S4 class of type [spflow_network_multi-class()]
#' @seealso spflow_network_classes
#' @export
#' @examples
#' spflow_network_multi() # empty
#' spflow_network_multi(germany_net,usa_net) # two nodes, no pairs
spflow_network_multi <- function(...) {

  input_nets <- unlist(list(...)) %||% list()

  warn_template <- "
  All supplied objects which are not of class
  sp_network or spflow_network_pairs are discarded!"
  is_net <- unlist(lapply(input_nets, is, class2 = "spflow_network_nodes"))
  is_pair <- unlist(lapply(input_nets, is, class2 = "spflow_network_pairs"))
  assert(all(is_net | is_pair), warn_template, warn = TRUE)
  sp_nodes <- input_nets[is_net]
  spflow_network_pairs <- input_nets[is_pair]


  pair_ids <- lapply(spflow_network_pairs, "id")
  pair_ids <- unlist(lapply(pair_ids, "[", "pair"),use.names = FALSE)
  net_ids <- unlist(lapply(sp_nodes, "id"))
  names(sp_nodes) <- net_ids
  names(spflow_network_pairs) <- pair_ids


  ## The class contains relational data for origins, dests, and od-pairs
  # 1. we have to ensure that the identification is correct
  # 2. the order of observations is important for calculations
  error_template <- "
  In the network pair with id %s, it is not possible to identify
  all %ss with the nodes in the %s network!"

  pair_ids <- lapply(spflow_network_pairs, "id")

  for (i in seq_along(pair_ids)) {

    net_pair <- pair_ids[[i]]["pair"]
    do_keys <- dat(spflow_network_pairs[[net_pair]]) %|!|% get_do_keys
    wrong_od_order <- FALSE

    # check for origins
    orig_net <- pair_ids[[i]]["orig"]
    orig_keys <- orig_keys_od <- unique(do_keys[[2]])
    if (orig_net %in% names(sp_nodes)) {
      orig_keys <- subset_keycols(dat(sp_nodes[[orig_net]]), drop_keys = FALSE)[[1]]
      assert(all(as.character(orig_keys_od) %in% as.character(orig_keys)),
             error_template, net_pair, "origin", "origin")

      wrong_od_order <-
        length(levels(orig_keys_od)) != length(levels(orig_keys)) ||
        any(levels(orig_keys_od) != levels(orig_keys))
    }

    dest_net <- pair_ids[[i]]["dest"]
    dest_keys <- dest_keys_od <- unique(do_keys[[1]])
    if (dest_net %in% names(sp_nodes)) {
      dest_keys <- subset_keycols(dat(sp_nodes[[dest_net]]), drop_keys = FALSE)[[1]]
      assert(all(as.character(dest_keys_od) %in% as.character(dest_keys)),
             error_template, net_pair, "destination", "destination")
      wrong_od_order <- wrong_od_order ||
        length(levels(dest_keys_od)) != length(levels(dest_keys)) ||
        any(levels(dest_keys_od) != levels(dest_keys))
    }

    if (wrong_od_order) {
      warn_template <- "
      The the data in the network pair %s is reordered!"
      assert(FALSE, warn_template, net_pair, warn = TRUE)
      do_key_cols <- names(do_keys)
      do_keys <- Map(factor, x = do_keys, levels = list(dest_keys, orig_keys))
      pdat_new <- dat(spflow_network_pairs[[net_pair]])
      pdat_new[do_key_cols] <- do_keys
      pdat_new <- pdat_new[order(do_keys[[2]], do_keys[[1]]),,drop = FALSE]
      if (inherits(pdat_new, "data.table") && requireNamespace("data.table", quietly = TRUE))
        pdat_new <- data.table::as.data.table(pdat_new)
      dat(spflow_network_pairs[[net_pair]]) <- pdat_new
    }
  }


  return(new("spflow_network_multi",
             nodes = sp_nodes,
             pairs = spflow_network_pairs))
}

# ---- Helpers ----------------------------------------------------------------

#' @keywords internal
check_pair_completeness <- function(pair_id, multi_net) {

  all_ids <- id(multi_net)
  this_pair <- pull_member(multi_net, pair_id)
  od_id <- id(this_pair)
  od_nnodes <- nnodes(this_pair)

  # node informations
  o_nnodes <- pull_member(multi_net, od_id["orig"])
  o_nnodes <- o_nnodes %|!|% nnodes(o_nnodes)
  d_nnodes <- pull_member(multi_net, od_id["dest"])
  d_nnodes <- d_nnodes %|!|% nnodes(d_nnodes)

  pair_infos <- data.frame(
    "ID_ORIG_NET" = od_id["orig"],
    "ID_DEST_NET" = od_id["dest"],
    "ID_NET_PAIR" = pair_id,
    "COMPLETENESS" = npairs(this_pair) / prod(od_nnodes),
    "C_PAIRS" = sprintf("%s/%s", npairs(this_pair), (o_nnodes * d_nnodes) %||% "(?)"),
    "C_ORIG" = sprintf("%s/%s", od_nnodes["orig"], o_nnodes %||% "(?)"),
    "C_DEST" = sprintf("%s/%s", od_nnodes["dest"], d_nnodes %||% "(?)"),
    "NPAIRS" = npairs(this_pair),
    "NORIG" = od_nnodes["orig"],
    "NDEST" = od_nnodes["dest"],
    "ORIG_NNODES" = o_nnodes %||% NA,
    "DEST_NNODES" = d_nnodes %||% NA)


  if (od_id["orig"] %in% all_ids$nodes) {
    o_infos <- check_node_infos(pull_member(multi_net, od_id["orig"]))
    names(o_infos) <- paste0("ORIG_", names(o_infos))
    pair_infos <- cbind(pair_infos, o_infos, row.names = NULL)
  }

  if (od_id["dest"] %in% all_ids$nodes) {
    d_infos <- check_node_infos(pull_member(multi_net, od_id["dest"]))
    names(d_infos) <- paste0("DEST_", names(d_infos))
    pair_infos <- cbind(pair_infos, d_infos, row.names = NULL)

  }


  return(pair_infos)
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
  return(d_index + (o_index - 1L) * n_d)
}

#' @keywords internal
derive_pair_index_do <- function(pair_dat, do_keycols = attr_key_do(pair_dat)) {
  compute_pair_index_do(
    d_index = as.integer(pair_dat[[do_keycols[1]]]),
    o_index = as.integer(pair_dat[[do_keycols[2]]]),
    n_d = nlevels(pair_dat[[do_keycols[2]]]))
}

#' @keywords internal
get_od_coords <- function(
    network_multi,
    net_pair_id = id(network_multi)[["pairs"]][1]) {

  assert(net_pair_id %in% id(network_multi)[["pairs"]],
         "No network_pair corresponds to the id %s!", net_pair_id)

  od_ids <- id(pull_member(network_multi, net_pair_id))
  orig_coords <- subset_keycols(dat(network_multi, od_ids["orig"]), drop_keys = FALSE)
  assert(ncol(orig_coords) == 3, "No valid coordinates!")

  if (od_ids["orig"] == od_ids["dest"])
    return(data.frame(orig_coords[,-1], row.names = orig_coords[,1]))

  dest_coords <- subset_keycols(dat(network_multi, od_ids["dest"]), drop_keys = FALSE)
  assert(ncol(dest_coords) == 3, "No valid coordinates!")
  colnames(dest_coords) <- colnames(orig_coords)


  node_coords <- unique(rbind(orig_coords, dest_coords))
  assert(has_distinct_elements(node_coords[,1]),
         "The coordinates the are not uniquely identifyed!")
  return(data.frame(node_coords[,-1], row.names = node_coords[,1]))
}
