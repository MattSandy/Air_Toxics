#Create chart, map, and summary of chosen Air Toxics data,  ui.R

library(shiny)

# Define UI for viewer app
shinyUI(pageWithSidebar(
  
  #Title
  headerPanel("Monitor Data 2002-2012: Air Toxics"),
  
  # Sidebar with controls to select a dataset and specify the number of observations to view
  
  sidebarPanel(
    tags$head(
      tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
      tags$style(type="text/css", "select { max-width: 200px; }"),
      tags$style(type="text/css", ".jslider { max-width: 200px; }"),
      tags$style(type='text/css', ".well { max-width: 250px; }"),
      tags$style(type='text/css', ".span4 { max-width: 310px; }")
              ),
    
    img(src="https://familysearch.org/learn/wiki/en/images/2/2a/Minnesota-county-map.gif", style = "width: 70%; margin-left: 30px;
    margin-right: 0"),
    
     br(), h4("Pollutant:"),
     selectInput("pollutant", "",
                 choices = c("Formaldehyde", pol_list)),
                
    
    br(),h4("Region:"),
    selectInput("region", "", choices = c("Minnesota", "Metro", "North")),
    
    
    br(),h4("Years:"),
    sliderInput('years', '', min=2002, max=2012,
          value=c(2007, 2012),  format="####", step=1),
            
    br(),
    #submitButton(text = "Submit")
    
  ),
  
  # Show 3 tabs: chart, map and summary of the dataset
  mainPanel(
    tabsetPanel(
       
      tabPanel("Graph", plotOutput("plot")),
      tabPanel("Monitor Map", plotOutput("map")),
      tabPanel("Data Summary", tableOutput("view"))
                )  
            )
  
  
))
