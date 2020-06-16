test_that("spflow: abusive input => error", {

    # bad formula
    expect_error(spflow(flow_formula = "A",sp_multi_network = NULL),
                 "A valid formula is required!")

    # bad data
    expect_error(spflow(y ~ x + z, sp_multi_network = cars),
                 "The data musst be a network data object!")

    # bad ids
    expect_error(spflow(y ~ x + z,
                        sp_multi_network = sp_multi_network(),
                        network_pair_id = "test_fail"),
                 "The the network pair id \\[test_fail\\] is not available!")

    expect_error(spflow(y ~ x + z,
                        sp_multi_network = sp_multi_network(),
                        network_pair_id = 1),
                 "The network_pair_id musst be a character of length 1!")

    # bad control object is tested trough spflow_control

})
