# ---- matrix_format_d_o ------------------------------------------------------
expect_equal({
  od_vec <- data.frame(vals = 1:16,
                       d = rep(1:4, 4),
                       o = rep(1:4, each = 4))
  spflow:::matrix_format_d_o(
    values = 1:16,
    dest_index =  od_vec$d,
    orig_index =  od_vec$o,
    assume_ordered = FALSE)
},matrix(1:16,4,4))

expect_equal({

  od_vec <- data.frame(vals = 1:10,
                       d = unlist(Map("seq", 1, 4:1)),
                       o = unlist(Map("rep", 1:4, 4:1)))
  spflow:::matrix_format_d_o(
    values = 1:10,
    dest_index = od_vec$d,
    orig_index = od_vec$o)

}, cbind(c(1:4),c(5:7,0),c(8:9,0,0),c(10,0,0,0)))

expect_equal({
  od_vec <- data.frame(vals = 1:6,
                       d = unlist(Map("seq", 1, 3:1)),
                       o = unlist(Map("rep", 1:3, 3:1)))
  spflow:::matrix_format_d_o(
    values = 1:6,
    dest_index =  od_vec$d,
    orig_index =  od_vec$o,
    num_dest = 5,
    num_orig = 5,
    assume_ordered = FALSE)
},as(rbind(cbind(c(1:3),c(4:5,0),c(6,0,0),0,0),0,0),"Matrix"))
