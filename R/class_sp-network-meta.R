#' @title spflow network classes
#'
#' @description
#' The spflow packages provides three additional classes to the R environment
#' that allow to handle origin-destination flow data efficiently.
#'
#' The main idea is to exploit the relational structure or origin-destination
#' data which reduces the memory requirements.
#' Data on origins and destinations are stored in the
#' [sp_network_nodes-class()] and data on the origin-destination pairs are
#' stored in an [sp_network_pair-class()].
#'
#' A third object of type [sp_multi_network-class()] is then used to store
#' information on the nodes and pairs in an efficient relational storage.
#' It makes sure that all origin-destination pairs can be identified with the
#' nodes at the origin and destination.
#'
#'
#' @include class_sp-network-nodes.R class_sp-network-pair.R
#' @name sp_network_classes
#' @family [spflow network classes][sp_network_classes()]
#' @family Constructors for [spflow network classes][sp_network_classes()]
#' @examples
#'
#' ### An example use case for the spflow network classes and model estimation
#' \dontrun{
#' # load example data
#' example_data_sets <- c("paris10km_nodes",
#'                        "paris10km_mat_nb",
#'                        "paris10km_node_pairs")
#' data(list = example_data_sets)
#'
#' # define the sp_network_nodes...
#' # ... they are used as origins and destinations
#' # ... their neighborhood is based on contiguity
#' paris10km_net <- sp_network_nodes(
#'   network_id = "paris10km",
#'   node_neighborhood = paris10km_mat_nb$by_border,
#'   node_data = sf::st_drop_geometry(paris10km_nodes),
#'   node_key_column = "ID")
#'
#' # define the sp_network_pair...
#' # ... contains pairwise data (flows and distances)
#' # ... must be linked to an origin and a destination network
#' paris10km_net_pairs <- sp_network_pair(
#'   orig_net_id = "paris10km",
#'   dest_net_id = "paris10km",
#'   pair_data = paris10km_node_pairs,
#'   orig_key_column = "ORIG_ID",
#'   dest_key_column = "DEST_ID")
#'
#'
#' # define the sp_network_pair...
#' # ... combines information on nodes and pairs
#' paris10km_multi_net <- sp_multi_network(paris10km_net,paris10km_net_pairs)
#'
#' clog <- function(x) {
#'   y <- log(x)
#'   y - mean(y)
#' }
#'
#' # define the model that we use to explain the flows...
#' # ... D_() contains destination variables
#' # ... O_() contains origin variables
#' # ... D_() contains intra-regional variables (when origin == destination)
#' # ... G_() contains pair variables (distances)
#' model_formula <- formula_1 <-
#'   flow_formula <-
#'   log(COMMUTE_FLOW + 1) ~
#'   D_(clog(NB_COMPANY + 1) + clog(MED_INCOME + 1)) +
#'   O_(clog(POPULATION + 1) + clog(NB_COMPANY + 1) + clog(MED_INCOME + 1)) +
#'   I_(clog(NB_COMPANY + 1) + clog(POPULATION + 1)) +
#'   G_(log(DISTANCE + 1))
#'
#' # define what variables to use in an SDM specification
#' # ... if not given all will be used
#' sdm_formula <- ~
#'   D_(clog(NB_COMPANY + 1) + clog(MED_INCOME + 1))
#'
#' # define the list of control parameters
#' flow_control <- spflow_control(sdm_variables = sdm_formula)
#' }
NULL

# ---- Virtual classes --------------------------------------------------------
# combine the two classes that hold data as they can share some methods
#' @keywords internal
setClassUnion("sp_network_nodes_pairs" ,
              c("sp_network_nodes", "sp_network_pair"))


# ---- methods ----------------------------------------------------------------

#' @title
#'   Internal method that allows to extract the schema of the data inside
#'   [sp_network_classes()]
#'
#' @description
#'   The method extract the columns of the data with zero rows which allows to
#'   use it as a template for other functions or to extract the column names.
#'
#' @param col_subset
#'   A character vector indicating columns that should be kept for the template
#' @param object A [sp_network_nodes-class()] or [sp_network_pair-class()]
#' @rdname dat_template
#' @keywords internal
setMethod(
  f = "dat_template",
  signature = c("sp_network_nodes_pairs"),
  function(object, col_subset = NULL) { # ---- dat_template -------------------

    key_cols <- data.table::key(dat(object))

    if (is.null(col_subset)) {
      template <- dat(object)[0,!key_cols, with = FALSE]
    }

    if (!is.null(col_subset)) {
      stopifnot(all(key_cols != col_subset),
                all(col_subset %in% variable_names(object)))

      template <- dat(object)[0,col_subset, with = FALSE]
    }

    return(invisible(template))
  })

#' @title
#'   Internal method that allows to drop columns from the data inside
#'   [sp_network_classes()]
#'
#' @description
#'   The function uses reference semantic and allows to manipulate the data
#'   inside the object efficiently.
#'
#' @param drop_columns A vector indicating columns of the data;
#'     either character or integer.
#' @inheritParams dat_template
#' @rdname drop_columns
#' @keywords internal
setMethod(
  f = "drop_columns",
  signature = c("sp_network_nodes_pairs"),
  function(object, drop_columns) { # ---- drop_columns ------------------------
    dat(object)[ , c(drop_columns) := NULL]
    return(invisible(dat(object)))
  })


#' @title
#'   Internal method that allows to overwrite columns in the data inside
#'   [sp_network_classes()]
#'
#' @param new_cols New columns to be added to existing data can be one of;
#'   scalar, vector, data.frame
#' @rdname set_columns
#' @keywords internal
setMethod(
  f = "set_columns",
  signature = c("sp_network_nodes_pairs"),
  function(object, new_cols) { # ---- set_columns --------------------------------

    new_cols <- data.frame(new_cols,check.names = TRUE)
    assert(nrow(new_cols) == 1 || nrow(new_cols) == nrow(dat(object)),
           "The dimensions of the new data does not match with the existing!")
    dat(object)[ , names(new_cols) := new_cols]

    return(invisible(dat(object)))
  })


#' @title Access the column names of the data inside [sp_network_classes()]
#' @inheritParams dat_template
#' @rdname variable_names
#' @aliases variable_names<-
#' @keywords internal
setMethod(
  f = "variable_names",
  signature = c("sp_network_nodes_pairs"),
  function(object) { # ---- variable_names ------------------------------------
    return(names(dat(object)))
  })

#' @param value A character of new variable names
#' @rdname variable_names
#' @name variable_names<-
#' @keywords internal
setReplaceMethod(
  f = "variable_names", # ---- variable_names<- -------------------------------
  signature = c("sp_network_nodes_pairs"),
  function(object,value) {
    names(dat(object)) <- value
    if (validObject(object))
      return(object)
  })
