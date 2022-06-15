# spflow (development version)

# spflow 0.1.0.9003 (development version)

* Restructured the `spflow_model-class` 
* Added support for coordinates in the `sp_network_nodes-class`
* Added test for parameter space validity
* Added `pair_corr()` method for `spflow_model-class` and `sp_multi_network-class`. The method creates a correlation matrix for origin, destination and OD-pair variables and their spatial lags. These correlations can be visualized with the `corr_map()` function.
* Added `plot()` method for some quick diagnostics of the `spflow_model`
* Added support for in sample predictions, with methods "TS", "TC"
* Added support for out of sample predictions, with method "TC"
* Added `map_flows()` to visualize flow data
* Added support for non-cartesian and rectangular cases
* Added handling of `NA` values during the estimation

# spflow 0.1.0 (2021-09-08)

* First version of the **spflow** package that is released to CRAN.
