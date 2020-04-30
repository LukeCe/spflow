# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - integration test for data preparation steps
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# Test of all objects harmonize with each other and that inconcistent data is
# detected.
# - - - - - - - - - - - - - - - - - - -
# Date: avril 2020

# setup test examples
attributes_1 <- cars
attributes_2 <- cbind(cars,cars)

key1 <- "cars1"
key2 <- "cars2"

nb_nodes_1 <- nrow(attributes_1)
nb_nodes_2 <- nrow(attributes_2)



# nodes -----------------------------------------------------------------------
context("Format node data")

# valid objects
nodes1 <- node_information(key1,neighborhood = diag(2,nb_nodes_1,nb_nodes_1))
nodes2 <- node_information(key2,neighborhood = diag(2,nb_nodes_2,nb_nodes_2))

# invalid objects
nodes1_fail <- nodes1
nodes1_fail@count <- nb_nodes_1 + 1
nodes2_fail <- nodes2
nodes2_fail@neighborhood <- Diagonal(2,nb_nodes_2 + 1)

describe("Format node data", {

  it("Recognizes inconsitencies",{
    expect_error({validObject(nodes1_fail)})
    expect_error({validObject(nodes2_fail)})
  })
})

# pairs -----------------------------------------------------------------------
context("Format od pair data")

# valid objects
pairs_11 <- od_pair_information(
  origin_id = key1,
  destination_id = key1,
  pair_attributes = matrix(rnorm(nb_nodes_1^2),nrow = nb_nodes_1, ncol = nb_nodes_1))

pairs_22 <- od_pair_information(
  origin_id = key2,
  destination_id = key2,
  pair_attributes = matrix(rnorm(nb_nodes_2^2),nrow = nb_nodes_2, ncol = nb_nodes_2))

pairs_12 <- od_pair_information(
  origin_id = key1,
  destination_id = key2,
  pair_attributes = matrix(rnorm(nb_nodes_2^2),nrow = nb_nodes_1, ncol = nb_nodes_2))

nb_nodes_1_dbl <- nb_nodes_1*2
pairs_11_dbl <- od_pair_information(
  origin_id = key1,
  destination_id = key1,
  pair_attributes = matrix(rnorm(nb_nodes_1_dbl^2),nrow = nb_nodes_1_dbl, ncol = nb_nodes_1_dbl))

# invalid_objects
pairs_11_fail <- pairs_11
pairs_11_fail@origin_count <- nb_nodes_1 + 1

pairs_22_fail <- pairs_22
pairs_22_fail@pair_attributes <-
  list(Matrix(rnorm((nb_nodes_2 + 1)^2),
              nrow = nb_nodes_2 + 1,
              ncol = nb_nodes_2 + 1))

describe("Format origin destination pair data", {

  it("Recognizes inconsitencies",{
    expect_error({validObject(pairs_11_fail)})
    expect_error({validObject(pairs_22_fail)})
  })
})

# multi network ---------------------------------------------------------------
context("Combine to network data")


multi_network <- network_data(nodes1,nodes2,pairs_11,pairs_22,pairs_12)

describe("Combine nodes and pairs into multinet", {

  it("Recognizes inconsitencies",{
    expect_error({network_data(nodes1,nodes2,pairs_11_dbl)},
                 "^[The number of nodes in network ].*\\[*\\] is not consitent\\!$")

    expect_error({network_data(nodes1_fail)})
    expect_error({network_data(pairs_11_fail)})
  })

  it("Recognizes duplication",{
    expect_error({network_data(pairs_11,pairs_11,pairs_22,pairs_12)},
                 "The identification of all sets of nodes and od_pairs must be unique!")
    expect_error({network_data(nodes1,nodes1,nodes2)},
                 "The identification of all sets of nodes and od_pairs must be unique!")
  })
})
