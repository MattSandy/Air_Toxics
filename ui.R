#Create chart, map, and summary of chosen Air Toxics data,  ui.R
library(shiny)
library(dplyr)
library(rCharts)

shinyUI(fluidPage(
    tags$head(    
        tags$style(type='text/css', ".select  { max-width: 180px; }"),
        tags$style(type='text/css', ".jslider { max-width: 240px; }"),
        
        tags$style(type='text/css', ".span4 { max-width: 250px; margin:0; min-width: 150px; }"),
        tags$style(type='text/css', ".span2 { max-width: 170px; margin:0; min-width: 150px; padding-left: 10px;}"),
        tags$style(type='text/css', ".span3 { max-width: 220px; padding-top: 0px; padding-left: 10px; margin-left: 0px;  margin-top: 0px; }"),
        tags$style(type='text/css', ".span12 { margin-left: 1px; padding-left: 5px; margin-bottom: 0px; margin-top: 0px; padding-top:0px; padding-bottom: 0px;}"),
        tags$style(type='text/css', ".hr { padding: 0px; max-height: 3px; margin-left: auto; margin-right: auto; width: 100%; height: 3px; margin-top: 10px; background-color: #D8D8D8; color: #D8D8D8; }"),
        tags$style(type='text/css', ".h1 { margin-left:5px; margin-bottom:0px; padding-bottom:0; font-size:26px;  }"),
        tags$style(type='text/css', ".h4 { margin-top: 0; padding-top:0; }"),
        tags$style(type='text/css', ".download { width: 92%; height: 34px; max-width: 120px; color: #0000FF; padding-top: 6px; margin:0px; margin-top: 15px; margin-bottom:0px}"),
        tags$style(type='text/css', ".container-fluid { margin-top: 0; margin-bottom: 0; padding-bottom:0; padding-top: 0; }"),
        tags$style(type='text/css', ".mainPanel { margin-top: 0; margin-bottom: 0; padding-bottom:0; padding-top: 0; width: 100%;}"),
        tags$style(type='text/css', ".shiny-html-output { margin: auto; margin-top: 0;  padding-top:0; }"),
        tags$style(type='text/css', ".shiny-plot-output {margin-left: 5px; margin-right:5px; margin-top: 5px; padding-top: 0px; padding-right:0px; width: 99%;}"),
        
            tags$style(type='text/css', ".checkbox { zindex:5; float=F; margin-top: 15px; margin-left: 9px; padding-bottom: 0px; margin-bottom: 25px;"),
            tags$style(type='text/css', ".shiny-datatable-output {margin-left: 5px; margin-right:5px; margin-top: 5px; padding-top: 0px; padding-right:0px; width: 99%;}"),
            tags$style(type='text/css', "#title {padding:0;  font-size:17px;  padding-left:12px; margin:0;  margin-top: 1px;}"),
            tags$style(type='text/css', "#titleM {padding:0;  font-size:17px;  padding-left:12px; margin:0;  margin-top: 1px;}"),
            tags$style(type='text/css', "#para {padding:0;  font-size:14.5px; margin:0;  margin-left:12px; margin-top: 4px; margin-right: 14px;}"),
            tags$style(type='text/css', "#risk {color:darkred; font-size:14.5px; font-weight:550; padding:0;  padding-left:12px; margin:0; margin-top: 4px; margin-bottom: 2px;}"),
            tags$style(type='text/css', ".h5 { margin:0; padding-top:0;  }"),
           tags$style(type='text/css', "#trends {margin: 0; margin-top: 8px; padding:0; width: 99.5%; height: 460px;}"),
           tags$style(type='text/css', ".leaflet {margin: 0; margin-top: 1px; padding:0; width: 99.5%; height: 460px;}"),
            tags$style(type='text/css', "#barplot {margin: 0; margin-top: 8px; padding:0; width: 99.5%; height: 460px;}")
        
    ),
    
     #Web Title
    title = "Air Toxics Monitor Data: 2002-2013[beta]",
    
    #Page Header
    
    fluidRow(
        column(12,
               h1("(draft) Air Toxics Summary: 2002-2013", style="margin-left:9px; margin-top:15px; font-size:36px; margin-bottom:0px; padding-bottom:5px;")
               #hr(class="hr")
               
        )),
    
    
    fluidRow(
        column(2,
               #https://drive.google.com/file/d/0B9Ub5HCtyNmkME55b1FwU3l1NkE/edit?usp=sharing
               img(src="https://cloud.githubusercontent.com/assets/6283030/6784538/3802de68-d14e-11e4-8469-50e62df90740.png", style = "margin:auto; padding-right:10px; margin-top:30px; padding-top:25px; width:108%; height:180px; max-width:200px; max-height:190px; ")
        ),
        
        column(3,
               br(),
               h4("Pollutant:", style="margin-botom:3px;"), uiOutput("pollutants"),
               h4("Years:", style="margin-botom:10px; margin-top: 4px;"), uiOutput("yearRange")
               
        ),
        column(3,
               br(),   
               h4("Region:", style="margin-botom:3px;"), uiOutput("regions"),
               h4("Site ID:", style="margin-top: 8px; margin-botom:3px;"), uiOutput("siteids")
        ),
        column(3,
               br(),           
               h4("Summary:", style="margin-botom:-18px;"), 
               div(style="margin-top:-13px;", radioButtons("time", " ",
                                                          list("Annual Average" = "km_mean",
                                                               "Annual Maximum" = "Annual_Max"))),  
               h4("Download:", style="margin-botom:0px; margin-top: 15px; padding-bottom:0px;"),
               downloadButton("download", label = "Save Data", class = "download")
        )
        
        
    ),
    
    fluidRow(
        column(12
               #hr(class="hr")   
        )),
    
    # Show 3 tabs: chart, map and summary of the dataset
    fluidRow(
        column(12,
        
        tabsetPanel(id ="tabs1",
                    tabPanel("Maps", h5(textOutput("title")), h5(textOutput("risk")), showOutput("map", "leaflet"), tags$style('.leaflet {min-width: 99.9%; width:99.9%; padding:0; height: 405px; margin:0;}')),
                    tabPanel("Trends", showOutput("trends", "highcharts"), tags$style('.trends {min-width: 99.9%; width:99.9%; height: 405px; }')),
                    tabPanel("Bar Charts", showOutput("barplot", "highcharts"), p("*Error bars extend to the 95% upper confidence limit of the mean."), tags$style('.barplot {min-width: 99.9%; width:99.9%; height: 405px; }')),
                    tabPanel("Data Table", checkboxInput("allData", label = "Show All Columns", value = F), dataTableOutput("table")),
                    tabPanel("Monitors", h5(textOutput("titleM")), showOutput("monitorMap", "leaflet"), tags$style('.leaflet {min-width: 99.9%; width:99.9%; padding:0; height: 405px; margin:0;}')),
                    tabPanel("About", 
                             #   h5("Overview", id="title"), p("This tool presents ambient air data collected from MPCA's monitoring network.", id ="para"),br(),
                             #   h5("Monitoring Network", id="title"), p(""),br(),
                             #   h5("Sample Collection", id="title"), p(""),br(),
                             #   h5("Health Standards", id="title"), p(""),br(),
                             h5("Summary Values"), p("The averages presented 
                            in this tool are calculated using the Kaplan-Meier method
                            to account for samples with concentrations below the detection limit.
                            The annual maximum is an estimation of the maximum 1 hour concentration of a pollutant and is calculated by taking the second highest 24 hour value and multiplying it by five.", id ="para"), br(),
                             h5("Contacts", id="title"), p("For more information please contact...", id ="para"),br(),
                             #   h5("Current Data", id="title"), p(""),br(),
                             #   h5("Limitations", id="title"), p(""),br(),
                             tags$style('.title {min-width: 99.9%; width:99.9%; padding:0; height: 405px; margin:0;}'))
                    
        ),
        
        fluidRow(column(12, hr()))
    ))
))

