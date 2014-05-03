#Create chart, map, and summary of chosen Air Toxics data,  ui.R
#library(shiny)
#library(ggplot2)
#library(rCharts)

shinyUI(fluidPage(
  tags$head(    
    tags$style(type='text/css', "select { max-width: 160px; }"),
    tags$style(type='text/css', ".jslider { max-width: 250px; }"),
    tags$style(type='text/css', ".row-fluid { margin:auto; max-width: 100%; margin-top: 0px; margin-bottom:0; z-index:1; padding-top: 0px; padding-bottom: 0px; }"),
    tags$style(type='text/css', ".span4 { max-width: 260px; margin:0; }"),
    tags$style(type='text/css', ".span2 { max-width: 160px; margin:0; padding-left: 10px;}"),
    tags$style(type='text/css', ".span3 { max-width: 220px; padding-top: 0px; padding-left: 10px; margin-left: 0px;  margin-top: 0px; }"),
    tags$style(type='text/css', ".span12 { margin-left: 1px; padding-left: 5px; margin-bottom: -5px; margin-top: 0px; padding-top:0px; padding-bottom: 0px;}"),
    tags$style(type='text/css', "hr { padding: 0px; max-height: 3px; margin-left: auto; margin-right: auto; width: 100%; height: 3px; margin-top: 10px; background-color: #D8D8D8; color: #D8D8D8; }"),
    tags$style(type='text/css', "h1 { margin-left: 5px; margin-bottom: 0px; padding-bottom:0; font-size: 27px; }"),
    tags$style(type='text/css', "h4 { margin-top: 0; padding-top:0; }"),
    tags$style(type='text/css', ".download { width: 82%; height: 19px; max-width: 100px; color: #0000FF; padding-top: 4px; margin:0px; margin-top: 7px; margin-bottom:0px}"),
    tags$style(type='text/css', ".container-fluid { margin-top: 0; margin-bottom: 0; padding-bottom:0; padding-top: 0; }"),
    tags$style(type='text/css', ".shiny-html-output { margin: auto; margin-top: 0;  padding-top:0; }")
    
  ),
  
  #Web Title
  title = "Monitor Data 2002-2014: Air Toxics",
  
  #Page Header
  
  fluidRow(
    column(12,
       headerPanel("Air Toxics Data 2002-2014"),
       br()
       #hr(class="hr")
       
    )),
  
  
  fluidRow(
    column(2,
       br(),
       #https://drive.google.com/file/d/0B9Ub5HCtyNmkME55b1FwU3l1NkE/edit?usp=sharing
       img(src="https://drive.google.com/uc?id=0B9Ub5HCtyNmkME55b1FwU3l1NkE", style = "width: 107%; max-width: 220px; height: 108%; margin: 0px; ")
       #img(src="https://familysearch.org/learn/wiki/en/images/2/2a/Minnesota-county-map.gif", style = "width: 75%; margin-left: 20px; margin-right: 0;")
       
    ),
    
    column(3,
           br(),
           h4("Pollutant:"), uiOutput("pollutants"),
           h4("Summary:"),  
           radioButtons("time", " ",
                        list("Annual Average" = "km_mean",
                             "2nd Highest Concentration" = "Second_Highest"))       
    ),
    column(3,
           br(),   
           h4("Years:"), 
           uiOutput("yearRange"),
           br(),
           h4("Region:"),
           uiOutput("regions")
    ),
    column(3,
           br(),           
           h4("Site ID:"),
           uiOutput("siteids"), 
           h4("Download:"),
           downloadButton("download", label = "Save Data", class = "download" )
    )
           
           
    ),
                 
           fluidRow(
           column(12, 
           hr(class="hr")   
           )),
           
           # Show 3 tabs: chart, map and summary of the dataset
           mainPanel(
           tags$head(
           tags$style(type='text/css', ".span8 { margin: 0; margin-bottom:0;  margin-top: 0; padding: 0px; width: 100%; }"),
           tags$style(type='text/css', ".tabbable {  margin: 0;  padding: 0px; width: 100%; }"),
           tags$style(type='text/css', ".tabbable tabs-above {  margin: 0;padding: 0px; width: 100%; }"),
           tags$style(type='text/css', ".nav nav-tabs { margin: 0; padding: 0px; width: 100%; }"),
           tags$style(type='text/css', ".tab-content {  margin:0; margin-top:-9px;  padding: 0px; width: 100%; }"),
           tags$style(type='text/css', ".tab-pane {  margin:0; padding: 0px; width: 100%; }"),
           tags$style(type='text/css', ".tab-pane active {  margin: 0; padding: 0px; width: 100%; }"),
           tags$style(type='text/css', "#tab-3964-1. {margin: 0; padding: 0px; width: 100%; }"),
           tags$style(type='text/css', ".checkbox { zindex:5; float=F; margin-top: -5px; margin-left: 9px; padding-bottom: 0px; margin-bottom: 25px;"),
           tags$style(type='text/css', ".shiny-datatable-output {margin-left: 5px; margin-right:5px; margin-top: 5px; padding-top: 0px; padding-right:0px; width: 99%;}"),
           tags$style(type='text/css', ".span { margin-top: 0 px; margin-bottom: 0px; padding:0px; }"),
           tags$style(type='text/css', ".span6 { height:0px; margin-top: 0 px; margin-bottom: 0px; padding:0px; }"),
           tags$style(type='text/css', ".active { margin:auto; margin-top: 0px; margin-bottom: 0px; padding:0px; }"),
           tags$style(type='text/css', "#map {margin:0;  padding: 0px; margin-top: 0px; height:405px; width:100%; }"),
           tags$style(type='text/css', "#title {padding:0;  font-size:17px;  padding-left:12px; margin:0;  margin-top: 1px;}"),
           tags$style(type='text/css', "#risk {color:darkred; font-size:14.5px; font-weight:550; padding:0;  padding-left:12px; margin:0; margin-top: 4px; margin-bottom: 2px;}"),
           tags$style(type='text/css', "h5 { margin:0; padding-top:0;  }"),
           tags$style(type='text/css', "#trends {margin: 0; margin-top: 1px; padding:0; width: 98%; height: 460px;}")
          
           ),
    
     
    tabsetPanel(
      tabPanel("Maps", h5(textOutput("title")), h5(textOutput("risk")),mapOutput("map"), tags$style('.leaflet {width: 100%; height:405px;}')),
      tabPanel("Trends", showOutput("trends", "highcharts")),
      tabPanel("Bar Charts", plotOutput("barplot", height = 470)),
      tabPanel("Data Table", checkboxInput("allData", label = "Show All Columns", value = F), dataTableOutput("table"))
      
    ),
    
    HTML("<hr noshade size='1'/>") ),
  
  fluidRow(
    column(12,  
           p("*Averages represent the Kaplan-Meier mean. Error bars extend to the 95% upper confidence limit calculated by bootstrapping. Last updated 4/30/2014.")
           
    ))
  
  
))
