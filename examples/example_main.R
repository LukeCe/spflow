# load example data
example_data_sets <- c("paris10km_nodes",
                       "paris10km_mat_nb",
                       "paris10km_node_pairs")
data(list = example_data_sets)

# define the sp_network_nodes...
# ... they are used as origins and destinations
# ... their neighborhood is based on contiguity
paris10km_net <- sp_network_nodes(
  network_id = "paris10km",
  node_neighborhood = paris10km_mat_nb$by_border,
  node_data = paris10km_nodes %>% sf::st_drop_geometry(.),
  node_id_column = "ID")

# define the sp_network_pair...
# ... contains pairwise data (flows and distances)
# ... must be linked to an origin and a destination network
paris10km_net_pairs <- sp_network_pair(
  orig_net_id = "paris10km",
  dest_net_id = "paris10km",
  pair_data = paris10km_node_pairs,
  orig_key_column = "ORIG_ID",
  dest_key_column = "DEST_ID")


# define the sp_network_pair...
# ... combines information on nodes and pairs
paris10km_multi_net <- sp_multi_network(paris10km_net,paris10km_net_pairs)

clog <- function(x) {
  y <- log(x)
  y - mean(y)
}

# define the model that we use to explain the flows...
# ... D_() contains destination variables
# ... O_() contains origin variables
# ... D_() contains intra-regional variables (when origin == destination)
# ... G_() contains pair variables (distances)
model_formula <- formula_1 <-
  flow_formula <-
  log(COMMUTE_FLOW + 1) ~
  D_(clog(NB_COMPANY + 1) + clog(MED_INCOME + 1)) +
  O_(clog(POPULATION + 1) + clog(NB_COMPANY + 1) + clog(MED_INCOME + 1)) +
  I_(clog(NB_COMPANY + 1) + clog(POPULATION + 1)) +
  G_(log(DISTANCE + 1))

# define what variables to use in an SDM specification
# ... if not given all will be used
sdm_formula <- ~
  D_(clog(NB_COMPANY + 1) + clog(MED_INCOME + 1))

# define the list of control parameters
flow_control <- spflow_control(sdm_variables = sdm_formula)
