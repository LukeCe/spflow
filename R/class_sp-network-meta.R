#' @title spflow network classes
#'
#' @description
#' The spflow package provides three additional classes to the R environment
#' that allow to handle origin-destination flow data efficiently.
#'
#' The main idea is to exploit the relational structure of origin-destination
#' data to reduce the memory requirements.
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
#' @name spflow_network_classes
#' @family spflow network classes
#' @family Constructors for spflow network classes
#' @examples
#'
#' ### An example use case for the spflow network classes and model estimation
#' # load example data
#' data("paris10km_municipalities")
#' data("paris10km_neighborhood")
#' data("paris10km_commuteflows")
#'
#' # define the sp_network_nodes...
#' # ... they are used as origins and destinations
#' # ... their neighborhood is based on contiguity
#' paris10km_net <- sp_network_nodes(
#'   network_id = "paris10km",
#'   node_neighborhood = paris10km_neighborhood$by_contiguity,
#'   node_data = sf::st_drop_geometry(paris10km_municipalities),
#'   node_key_column = "ID_MUN")
#'
#' # define the sp_network_pair...
#' # ... contains pairwise data (flows and distances)
#' # ... must be linked to an origin and a destination network
#' paris10km_net_pairs <- sp_network_pair(
#'   orig_net_id = "paris10km",
#'   dest_net_id = "paris10km",
#'   pair_data = paris10km_commuteflows,
#'   orig_key_column = "ID_ORIG",
#'   dest_key_column = "ID_DEST")
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
#' flow_formula <-
#'   log(COMMUTE_FLOW + 1) ~
#'   D_(log(NB_COMPANY) + clog(MED_INCOME)) +
#'   O_(log(POPULATION) + log(NB_COMPANY) + clog(MED_INCOME)) +
#'   I_(log(NB_COMPANY) + log(POPULATION)) +
#'   G_(log(DISTANCE + 1))
#'
#' # define what variables to use in an SDM specification
#' # ... if not given all will be used
#' sdm_formula <-
#'  ~ D_(log(NB_COMPANY) + clog(MED_INCOME))
#'
#' # define the list of control parameters
#' flow_control <- spflow_control(sdm_variables = sdm_formula)
#'
#'
#' # Estimate the model
#' spflow(flow_formula, paris10km_multi_net, flow_control = flow_control)
NULL
