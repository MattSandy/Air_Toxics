#Create chart, map, and summary of chosen Air Toxics data,  ui.R
#library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  tags$head(
    tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
    tags$style(type="text/css", "select { max-width: 100px; }"),
    tags$style(type="text/css", ".jslider { max-width: 250px; }"),
    tags$style(type='text/css', ".well { max-width: 250px; }"),
    tags$style(type='text/css', ".span4 { max-width: 260px; }")
  ),
  
  #Title
  headerPanel("Monitor Data 2002-2012: Air Toxics"),
  #Title
  title = "Monitor Data 2002-2012: Air Toxics",
  
  fluidRow(
    column(3,
           #br(),
           h4("Pollutant:"),
           uiOutput("pollutants")
           #img(src="https://familysearch.org/learn/wiki/en/images/2/2a/Minnesota-county-map.gif", style = "width: 75%; margin-left: 30px; margin-right: 0")
    ),
    column(3,
           #br(),    
           h4("Years:"),
           uiOutput("yearRange")
           
    ),
    column(3,
           #br(),           
           h4("Region:"),
           uiOutput("regions")     
    ),
    
    column(3,
           #br(),
           h4("Site ID:"),
           uiOutput("siteids")           
           #submitButton(text = "Submit")      
    )
  ),
  
  hr(),
    
  # Show 3 tabs: chart, map and summary of the dataset
  mainPanel(
    tags$head(
      tags$style(type='text/css', ".span8 { margin: 0px; padding-left: 0px; padding-right: 0px; width: 100%; }"),
      tags$style(type='text/css', ".tabbable {  margin: 0px; padding-right: 0px; width: 99.7%; }"),
      tags$style(type='text/css', ".nav nav-tabs {  margin: 0px; padding: 0px; width: 100%; }"),
      tags$style(type='text/css', ".tab-content {  margin: 0px; padding: 0px; width: 100%; }"),
      tags$style(type='text/css', ".tab-pane {  margin-left: auto; margin-top:0; padding: 0px; width: 100%; }")
     
    ),
  tabsetPanel(
    tabPanel("Graph Results", plotOutput("plot", height = 480)),
    tabPanel("Monitor Map", plotOutput("map", width= 400, height = 440)),
    tabPanel("Data Summary", dataTableOutput("table"))
  
   ))
  
  
))
