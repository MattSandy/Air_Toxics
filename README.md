Air_Toxics
==========

Interactive web App for displaying maps, charts and data tables of Air Data

**To run from R:**

require("dplyr")  
require("devtools")  

devtools::install_github("shiny", "rstudio")  
devtools::install_github('rCharts', 'ramnathv')  

library(shiny)  
runGitHub("Air_Toxics", "dKvale")

