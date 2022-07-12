# spflow 0.1.0.9006 (development version)

## New functions

* Added `pair_corr()` method for `spflow_model-class` and `spflow_multinet-class`. The method creates a correlation matrix for origin, destination and OD-pair variables and their spatial lags.
* Added `corr_map()` to visualize such correlations.
* Added `plot()` method for some quick diagnostics of the `spflow_model`
* Added `map_flows()` to visualize flow data
* Added `spflow_moran_plots()` for Moran scatter plots of flow-data and residuals of models 

## Additional options, features and fixes

* Added support for coordinates in the `spflow_nodes-class`
* Added test for parameter space validity
* Added support for in sample predictions, with methods "TS", "TC"
* Added support for out of sample predictions, with method "TC"
* Added support for non-cartesian and rectangular cases (see <https://www.tse-fr.eu/publications/generalized-framework-estimating-spatial-econometric-interaction-models/>)
* Added handling of `NA` values during the estimation

## Changes in data structures

These are heavy changes, which may break existing code.
Such changes will not occur in future releases and are due to the early stage of the package.

* Restructured the `spflow_model-class` (to better handle `NA` data and to make predictions easier)
* Renamed and restructured classes
  * `sp_multi_network-class` -> `spflow_multinet-class`
  * `sp_network_nodes-class` -> `spflow_nodes-class`
  * `sp_network_pairs-class` -> `spflow_pairs-class`

# spflow 0.1.0 (2021-09-08)

* First version of the **spflow** package that is released to CRAN.
