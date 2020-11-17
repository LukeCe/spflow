test_df <- cbind(A = rep(LETTERS[1:10],5),
                 B = rep(LETTERS[1:10],each = 5),
                 cars)

# ---- constructor ------------------------------------------------------------
test_that("sp_network_pair: => construction", {
    test_object <- sp_network_pair(
        orig_net_id = "cars",
        dest_net_id = "cars",
        pair_data = test_df,
        orig_key_column = "A",
        dest_key_column = "B")
    expect_s4_class(test_object, "sp_network_pair")
})

test_that("sp_network_pair: abusive input => error", {

    expect_error({
        test_object <- sp_network_pair(
            orig_net_id = "cars",
            dest_net_id = "cars",
            pair_data = "cars",
            orig_key_column = "A",
            dest_key_column = "B")
    }, "^[Object ].*[ must be coercible to a ].*\\!$")

    expect_error({
        test_object <- sp_network_pair(
            orig_net_id = 1,
            dest_net_id = 2,
            pair_data = cars,
            orig_key_column = "A",
            dest_key_column = "B")
    })
})

test_that("sp_network_pair: inconsistent input => error", {

    # non existing key column
    expect_error({
        sp_network_pair(
            orig_net_id = "cars",
            dest_net_id = "cars",
            orig_key_column =  "A",
            dest_key_column = "B",
            pair_data = cars)
    },"The origin and destination key columns are not found in the pair data!")
})


# ---- assessor methods -------------------------------------------------------
test_that("sp_network_pair: => correct assessors", {

    test_object <- sp_network_pair(
        orig_net_id = "cars",
        dest_net_id = "cars",
        pair_data = test_df,
        orig_key_column = "A",
        dest_key_column = "B")

    # data...
    keys <- c("ORIG_ID", "DEST_ID")
    final_cols <- c(keys,"speed","dist")
    test_dt <- copy(test_df) %>% setDT()
    test_dt <- test_dt[,c(keys) := list(factor(A),factor(B))
                       ][, ..final_cols] %>% setkeyv(keys)
    expect_equal(dat(test_object),test_dt)
    expect_equal(variable_names(test_object), colnames(test_dt))

    # ids...
    expect_equal(id(test_object, "orig"), c(orig = "cars"))
    expect_equal(id(test_object, "dest"), c(dest = "cars"))
    expect_equal(id(test_object, "pair"), c(pair = "cars_cars"))
    # counts...
    expect_equal(nnodes(test_object, "orig"), c(orig = 10))
    expect_equal(nnodes(test_object, "dest"), c(dest = 10))
    expect_equal(npairs(test_object), nrow(test_df))
})

# ---- replacement methods ----------------------------------------------------
test_that("sp_network_pair: => correct replacements", {

    test_object <- sp_network_pair(
        orig_net_id = "cars",
        dest_net_id = "cars",
        pair_data = test_df,
        orig_key_column = "A",
        dest_key_column = "B")

    test_dt2 <- dat(test_object)[,c("const") := 1]
    expect_equal(c(orig = 10), nnodes(test_object, "orig"))
    expect_equal(c(dest = 10), nnodes(test_object, "dest"))
    expect_equal(50, npairs(test_object))
    expect_equal(dim(dat(test_object)), dim(test_df) + c(0,1))

    # minimal case
    test_object <- sp_network_pair(orig_net_id = "cars", dest_net_id = "cars")
    expect_null(nnodes(test_object, "orig"))
    expect_null(nnodes(test_object, "dest"))
    expect_null(npairs(test_object))
})

# ---- show method ------------------------------------------------------------
test_that("sp_network_pair: correct show-method", {
    test_object <- sp_network_pair(
        orig_net_id = "cars",
        dest_net_id = "cars",
        pair_data = test_df,
        orig_key_column = "A",
        dest_key_column = "B")

    # complete
    expect_output(show(test_object))
    # no data
    dat(test_object) <- NULL
    expect_output(show(test_object))

    # minimal
    test_object <- sp_network_pair(orig_net_id = "cars", dest_net_id = "cars")
    expect_output(show(test_object))
})

