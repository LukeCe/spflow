
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

You can install the released version of spflow from
[CRAN](https://CRAN.R-project.org/package=spflow) with:

``` r
install.packages("spflow")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LukeCe/spflow")
```

## Example

The package provides a new `spflow_network_multi-class` that combines
information on the origins, the destinations, and the origin-destination
pairs. Once this object is created, we can estimate an interaction model
with the `spflow()` function. The [package
vignette](https://lukece.github.io/spflow/articles/paris_commute_flows.html)
contains a more detailed example.

``` r
library("spflow")
data("multi_net_usa_ge")

spflow(y9 ~ O_(X) + D_(X) + I(X) + P_(DISTANCE), multi_net_usa_ge)
#> --------------------------------------------------
#> Spatial interaction model estimated by: MLE  
#> Spatial correlation structure: SDM (model_9)
#> 
#> --------------------------------------------------
#> Coefficients:
#>                est    sd  t.stat  p.val
#> rho_d         0.49  0.03   16.66   0.02
#> rho_o         0.33  0.04    9.10   0.04
#> rho_w        -0.23  0.04   -5.13   0.08
#> (Intercept)  10.58  2.14    4.96   0.08
#> (Intra)       9.75  1.53    6.37   0.06
#> DEST_X        0.99  0.07   14.59   0.02
#> DEST_X.lag1   0.53  0.11    4.64   0.09
#> ORIG_X       -0.77  0.04  -20.38   0.02
#> ORIG_X.lag1  -0.38  0.09   -4.14   0.10
#> INTRA_I(X)    2.04  0.08   24.64   0.01
#> DISTANCE     -2.69  0.38   -7.13   0.05
#> 
#> --------------------------------------------------
#> R2_corr: 0.9921277  
#> Observations: 256  
#> Model coherence: Validated
```

## License

[GPL 3](https://www.r-project.org/Licenses/GPL-3)
