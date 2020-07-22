# errors because of non-loaded RdMarcros...
skip <- TRUE

if (requireNamespace('spelling', quietly = TRUE) & !skip) {
  spelling::spell_check_test(vignettes = TRUE,
                             error = TRUE,
                             skip_on_cran = TRUE)
}

