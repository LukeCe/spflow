# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - integration test for data preparation steps
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# Test if all objects harmonize with each other and that inconcistent data is
# detected.
# - - - - - - - - - - - - - - - - - - -
# Date: April 2020

# setup test examples
attributes_1 <- iris
attributes_2 <- cbind(attributes_1,attributes_1)

key1 <- "iris1"
key2 <- "iris2"

pair_key11 <- key1 %p% "_" %p% key2
nb_nodes_1 <- nrow(attributes_1)
nb_nodes_2 <- nrow(attributes_2)



# nodes -----------------------------------------------------------------------
context("Format node data")

# valid objects
nodes1 <- sp_network(key1,node_neighborhood = diag(2,nb_nodes_1,nb_nodes_1))
nodes2 <- sp_network(key2,node_neighborhood = diag(2,nb_nodes_2,nb_nodes_2))

# invalid objects
nodes1_fail <- nodes1
nodes1_fail@node_count <- nb_nodes_1 + 1
nodes2_fail <- nodes2
nodes2_fail@node_neighborhood <- Diagonal(2,nb_nodes_2 + 1)

describe("Format node data", {

  it("Recognizes inconsitencies",{
    expect_error({validObject(nodes1_fail)})
    expect_error({validObject(nodes2_fail)})
  })
})

# pairs -----------------------------------------------------------------------
context("Format od pair data")

# valid objects
pairs_11 <- sp_network_pair(
  origin_network_id = key1,
  destination_network_id = key1,
  node_pair_data = data.frame(rnorm(nb_nodes_1^2)),
  origin_node_count = nb_nodes_1)

pairs_22 <- sp_network_pair(
  origin_network_id = key2,
  destination_network_id = key2,
  node_pair_data = data.frame(rnorm(nb_nodes_2^2)),
  origin_node_count = nb_nodes_2
)

pairs_12 <- sp_network_pair(
  origin_network_id = key1,
  destination_network_id = key2,
  node_pair_data = data.frame(rnorm(nb_nodes_2*nb_nodes_1)),
  origin_node_count = nb_nodes_1)

nb_nodes_1_dbl <- nb_nodes_1*2
pairs_11_dbl <- sp_network_pair(
  origin_network_id = key1,
  destination_network_id = key1,
  node_pair_data = data.frame(rnorm(nb_nodes_1_dbl^2)),
  origin_node_count = nb_nodes_1_dbl)

# invalid_objects
pairs_11_fail <- pairs_11
pairs_11_fail@origin_node_count <- nb_nodes_1 + 1

pairs_22_fail <- pairs_22
pairs_22_fail@node_pair_data <-
  data.table::data.table(rnorm(1 + (nb_nodes_2^2)))

describe("Format origin destination pair data", {

  it("Recognizes inconsitencies",{
    expect_error({validObject(pairs_11_fail)})
    expect_error({validObject(pairs_22_fail)})
  })
})

# multi network ---------------------------------------------------------------
context("Combine to network data")


multi_network <- sp_multi_network(nodes1,nodes2,pairs_11,pairs_22,pairs_12)

describe("Combine nodes and pairs into multinet", {

  it("Recognizes inconsistencies",{
    expect_error({sp_multi_network(nodes1,nodes2,pairs_11_dbl)},
                 "^[The number of nodes in network ].*\\[*\\] is not consistent\\!$")

    expect_error({sp_multi_network(nodes1_fail)})
    expect_error({sp_multi_network(pairs_11_fail)})
  })

  it("Recognizes duplication",{
    expect_error({sp_multi_network(pairs_11,pairs_11,pairs_22,pairs_12)},
                 "The identification of all networks and network_pairs must be unique!")
    expect_error({sp_multi_network(nodes1,nodes1,nodes2)},
                 "The identification of all networks and network_pairs must be unique!")
  })
})


