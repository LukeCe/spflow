expect_equal(spflow:::count_pattern(c("aab","bbc", "acc"), "b"),
             c(1,2,0))
