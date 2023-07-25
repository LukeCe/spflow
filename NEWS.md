# spflow (development version)

## New functions

* Added `pair_cor()` method for `spflow_model-class` and `spflow_network_multi-class`. The method creates a correlation matrix for origin, destination and OD-pair variables and their spatial lags.
* Added `cor_image()` to visualize such correlations.
* Added `plot()` method for some quick diagnostics of the `spflow_model`
* Added `map_flows()` to visualize flow data
* Added `spflow_moran_plots()` for Moran scatter plots of flow-data and residuals of interaction models 
* Added `compare_results()` to quickly compare multiple model
* Added `spflow_refit()` to efficiently estimate nested models (this functionality is still experimental)

## Additional options, features and fixes

* Added support for coordinates in the `spflow_network-class`
* Added test for parameter space validity
* Added support for in sample predictions, with methods "TS", "TC"
* Added support for out of sample predictions, with method "TC"
* Added support for non-cartesian and rectangular cases (see <https://www.tse-fr.eu/publications/generalized-framework-estimating-spatial-econometric-interaction-models/>)
* Added handling of `NA` values during the estimation

## Changes in data structures

These are heavy changes, which may break existing code.
Such changes will not occur in future releases and are due to the early stage of the package.

* Restructured the `spflow_model-class` (to better handle `NA` values and to enable predictions)
* Renamed and restructured classes
  * `sp_multinetwork-class` -> `spflow_network_multi-class`
  * `sp_network_nodes-class` -> `spflow_network-class`
  * `sp_network_pairs-class` -> `spflow_network_pair-class`

## Other changes in arguments etc.

* the spflow_formula use `P_()` instead of `G_()` to refer to variables on the level of OD-pairs
* arguments in spflow()


# spflow 0.1.0 (2021-09-08)

* First version of the **spflow** package that is released to CRAN.
