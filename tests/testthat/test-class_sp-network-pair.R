test_that("Abusive input ==> ERROR", {

    expect_error({
        test_object <- sp_network_pair(
            origin_network_id = "cars",
            destination_network_id = "cars",
            node_pair_data = "cars")
    }, "^[Object ].*[ must be coercible to a ].*\\!$")

    expect_error({
        test_object <- sp_network_pair(
            origin_network_id = 1,
            destination_network_id = 2,
            node_pair_data = cars)
    })
})


test_that("Incomplete input ==> ERROR", {
    expect_error({
        sp_network_pair(
            origin_network_id = "cars",
            destination_network_id = "cars",
            node_pair_data = cars)
    },
    "The provided information does not suffice*")
})


test_that("Correct S4 construction", {
    test_object <- sp_network_pair(
        origin_network_id = "cars",
        destination_network_id = "cars",
        node_pair_data = cars,
        destination_node_count = 5)

    expect_s4_class(test_object, "sp_network_pair")
})


test_that("Correct S4 accessors", {

    test_object <- sp_network_pair(
        origin_network_id = "cars",
        destination_network_id = "cars",
        node_pair_data = cars,
        origin_node_count = 5)

    expect_equal(test_object@node_pair_data, dat(test_object))
    expect_equal(test_object@node_pair_data %>% names,
                 variable_names(test_object))
    expect_equivalent(c("cars_cars", "cars", "cars"), id(test_object))
    expect_equivalent(c(5, 10, 50), count(test_object))
})


test_that("Correct data replacements", {

    test_object <- sp_network_pair(
        origin_network_id = "cars",
        destination_network_id = "cars",
        node_pair_data = cars,
        origin_node_count = 5)

    dat(test_object,origin_node_count = 10) <- rbind(cars,cars)
    expect_equivalent(c(10, 10, 100), count(test_object))
})
