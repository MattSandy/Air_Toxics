Air_Toxics
==========

Interactive web App for displaying maps, charts and data tables of Air Data

To run from R:

require("dplyr")</br>
require("devtools")</br>

devtools::install_github("shiny", "rstudio")</br>
devtools::install_github('rCharts', 'ramnathv')</br>

library(shiny)</br>
runGitHub("Air_Toxics", "dKvale")

