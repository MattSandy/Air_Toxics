#Create chart, map, and summary of chosen Air Toxics data,  ui.R
library(shiny)
library(dplyr)
library(rCharts)

shinyUI(fluidPage(
  tags$head(    
        tags$link(rel="stylesheet", type="text/css", href="bootstrap.css"),
  ),
  
  #Web Title
  title = "Air Toxics Monitor Data: 2002-2013",
  
  #Page Header
  
  fluidRow(
    column(12,
           h1("Air Toxics Summary: 2002-2013", style="margin-left:9px; margin-top:15px; font-size:36px; margin-bottom:0px; padding-bottom:5px;")
           #hr(class="hr")
           
    )),
  
  
  fluidRow(
    column(2,
           #https://drive.google.com/file/d/0B9Ub5HCtyNmkME55b1FwU3l1NkE/edit?usp=sharing
           img(src="https://drive.google.com/uc?id=0B9Ub5HCtyNmkME55b1FwU3l1NkE", style = "margin:auto; margin-top:10px; padding-top:10px; width:108%; height:180px; max-width:200px; max-height:190px; ")
    ),
    
    column(3,
           br(),
           h4("Pollutant:", style="margin-botom:3px;"), uiOutput("pollutants"),
           h4("Years:", style="margin-botom:10px; margin-top: 8px;"), uiOutput("yearRange")
           
    ),
    column(3,
           br(),   
           h4("Region:", style="margin-botom:3px;"), uiOutput("regions"),
           h4("Site ID:", style="margin-top: 8px; margin-botom:3px;"), uiOutput("siteids")
    ),
    column(3,
           br(),           
           h4("Summary:", style="margin-botom:-6px;"), 
           div(style="margin-top:-4px;", radioButtons("time", " ",
                                                      list("Annual Average" = "km_mean",
                                                           "Annual Maximum" = "Annual_Max"))),  
           h4("Download:", style="margin-botom:-4px; margin-top: 0px; padding-bottom:-8px;"),
           downloadButton("download", label = "Save Data", class = "download")
    )
    
    
  ),
  
  fluidRow(
    column(12, 
           hr(class="hr")   
    )),
  
  # Show 3 tabs: chart, map and summary of the dataset
  mainPanel(
    tags$head(
        tags$link(rel="stylesheet", type="text/css", href="mainPanel.css"),
    ),
    
    
    tabsetPanel(id ="tabs1",
                tabPanel("Maps", h5(textOutput("title")), h5(textOutput("risk")), showOutput("map", "leaflet"), tags$style('.leaflet {min-width: 99.9%; width:99.9%; padding:0; height: 405px; margin:0;}')),
                tabPanel("Trends", showOutput("trends", "highcharts")),
                tabPanel("Bar Charts", showOutput("barplot", "highcharts"), p("*Error bars extend to the 95% upper confidence limit of the mean.")),
                tabPanel("Data Table", checkboxInput("allData", label = "Show All Columns", value = F), dataTableOutput("table")),
                tabPanel("Monitors", h5(textOutput("titleM"), id="title"), showOutput("monitorMap", "leaflet"), tags$style('.leaflet {min-width: 99.9%; width:99.9%; padding:0; height: 405px; margin:0;}')),
                tabPanel("About", 
                         #   h5("Overview", id="title"), p("This tool presents ambient air data collected from MPCA's monitoring network.", id ="para"),br(),
                         #   h5("Monitoring Network", id="title"), p(""),br(),
                         #   h5("Sample Collection", id="title"), p(""),br(),
                         #   h5("Health Standards", id="title"), p(""),br(),
                         h5("Summary Values", id="title"), p("The averages presented 
                            in this tool are calculated using the Kaplan-Meier method
                            to account for samples with concentrations below the detection limit.
                            The annual maximum is an estimation of the maximum 1 hour concentration of a
                            pollutant and is calculated by taking the second highest 24 hour
                            value and multiplying it by five.", id ="para"), br(),
                         h5("Contacts", id="title"), p("For more information please contact Dorian Kvale at Dorian.Kvale@state.mn.us or Cassie McMahon at Cassie.Mcmahon@state.mn.us.", id ="para"),br(),
                         #   h5("Current Data", id="title"), p(""),br(),
                         #   h5("Limitations", id="title"), p(""),br(),
                         tags$style('.title {min-width: 99.9%; width:99.9%; padding:0; height: 405px; margin:0;}'))
                
    ),
    
    fluidRow(column(12, hr(class="hr") ))
  )
))


