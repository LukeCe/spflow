#' Simulated data for stylized versions of Germany and the USA
#'
#' The package uses the same stylized country examples as those presented by
#' by \insertCite{Yang2017;textual}{spflow}.
#' The first example is a stylized version of Germany with 16 states.
#' The second example is a stylized version of the USA with 51 states.
#'
#'
#' We provide the spatial objects which contains the geography of our example
#' as well as the prepared [sp_network_nodes()] objects.
#' Finally there is a multi_network object which contains the two networks and
#' four network pairs based on the same examples.
#'
#' The simulation parameters are given as `rho` for the auto-regressive
#' parameter, `delta` for the impact of the exogenous variables and `sd_error`
#' for standard deviation of the simulated gaussian noise.
#'
#'
#' @rdname example_data
#' @name example_data
"multi_net_usa_ge"

#' @rdname example_data
"germany_grid"

#' @rdname example_data
"usa_net"

#' @rdname example_data
"usa_grid"

#' @rdname example_data
"germany_net"

#' @rdname example_data
"rho"

#' @rdname example_data
"delta"

#' @rdname example_data
"sd_error"


#' Example data for commuting flows of paris
#'
#' The packge includes an example data set that contains home-to-work commuting
#' flows for 71 municipalities arround the center of paris.
#'
#'
#' @rdname paris_data
#' @name paris_data
"paris10km_nodes"

#' @rdname paris_data
"paris10km_node_pairs"

#' @rdname paris_data
"paris10km_mat_nb"

