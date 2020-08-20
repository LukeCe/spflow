# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - create an exmaple dataset for illustrations and examples
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# The example contains commuting flows between all communities that are within
# a 10km radius of the center of Paris.
# = = = = = = = = = = = = = = = = = = =
# Notes:
#
# The data is prepared with functions that are contained in the package
# spflowDataTool written by Gabriel Watkinson.
# - - - - - - - - - - - - - - - - - - -
# Date: August 2020

devtools::load_all()
if (!require(spflowDataTool)) {
  data_tool_version <- "0.0.0.9000"
  repo <-  "LukeCe/spflowDataTool" %p% "@v" %p% data_tool_version
  remotes::install_github(repo,auth_token = Sys.getenv("GITHUB_PAT"))
}



