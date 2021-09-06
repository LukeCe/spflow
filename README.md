
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spflow

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/spflow)](https://CRAN.R-project.org/package=spflow)
[![R-CMD-check](https://github.com/LukeCe/spflow/workflows/R-CMD-check/badge.svg)](https://github.com/LukeCe/spflow/actions)
[![test-coverage](https://codecov.io/gh/LukeCe/spflow/branch/master/graph/badge.svg)](https://github.com/LukeCe/spflow/actions)
[![status](https://tinyverse.netlify.com/badge/spflow)](https://CRAN.R-project.org/package=spflow)
[![Downloads](https://cranlogs.r-pkg.org/badges/spflow?color=brightgreen)](https://www.r-pkg.org/pkg/spflow)
<!-- badges: end -->

The [**spflow**](https://lukece.github.io/spflow/) package allows to
estimate spatial econometric interaction models. It is designed to
exploit the relational structure of flow data, reducing the
computational burden and memory requirements.

## Installation

<!-- You can install the released version of spflow from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("spflow") -->
<!-- ``` -->
<!-- And the  -->

You can install development version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("LukeCe/spflow")
```

## Example

The package provides a new `sp_multi_network-class` that combines
information on the origins, the destinations, and the origin-destination
pairs. Once this object is created, we can estimate an interaction model
with the `spflow()` function. The [package
vignette](https://lukece.github.io/spflow/articles/paris_commute_flows.html)
contains a more detailed example.

``` r
library("spflow")
spflow(y9 ~ . + G_(DISTANCE), multi_net_usa_ge)
#> --------------------------------------------------
#> Spatial interaction model estimated by: MLE  
#> Autocorrelation structure: model_9 (SDM)  
#> Observations: 256  
#> 
#> --------------------------------------------------
#> Coefficients:
#>                 est    sd  t.stat  p.value
#> rho_d          0.48  0.03   15.75     0.02
#> rho_o          0.36  0.03   10.59     0.03
#> rho_w         -0.25  0.04   -5.95     0.05
#> (Intercept)   10.00  1.92    5.21     0.06
#> (Intra)       11.32  3.07    3.69     0.08
#> DEST_X         0.94  0.06   14.97     0.02
#> DEST_X.lag1    0.62  0.11    5.80     0.05
#> ORIG_X        -0.78  0.04  -21.20     0.02
#> ORIG_X.lag1   -0.32  0.08   -3.93     0.08
#> INTRA_X        1.95  0.08   23.98     0.01
#> INTRA_X.lag1  -0.34  0.19   -1.76     0.16
#> DISTANCE      -2.80  0.34   -8.25     0.04
#> 
#> --------------------------------------------------
#> R2_corr: 0.9927709
```

## License

[GPL 3](https://www.r-project.org/Licenses/GPL-3)
