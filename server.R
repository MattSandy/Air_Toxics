#summary server.R

#library(ggmap)

#ON SERVER!!!  WOOOP!
#library("shiny", lib.loc="/var/www/shinys/shiny")

#Only one package needed::::
#runApp("Air_Toxics", port=5555, host="0.0.0.0", launch.browser=F)

#detach(package:shiny, unload=T, force=T)
#library("shiny", lib.loc="/home/kvale/Desktop/shiny4044")

library(maps)
library(ggplot2)
library(grid)
library(dplyr)

options("digits"=4)
#warn=-1
#site="minneapolis, mn"
#a= get_map(location = site, zoom = 12, color="bw", source = "osm")
#toxics<- read.csv(file="summary2002_12.csv", header=T, stringsAsFactors=F, nrows=7000 )
toxics<- readRDS(file="summary2002_12.rds")
hbvs <- read.csv(file="hbvs.csv", header=T, stringsAsFactors=F, nrows=70 )
#load("summary2002_12.rda")
#toxics<-na.omit(toxics)
#toxics$year <- as.numeric(toxics$year)
#toxics$MPCAID <- as.numeric(toxics$MPCAID)
pol_list<-levels(as.factor(toxics$Pollutant))

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  #Generate Pollutant List
  output$pollutants <- renderUI({
    selectInput("pollutant", "", selected = "Formaldehyde", choices = pol_list ) })
  
  output$regions <- renderUI({
  selectInput("region", "", selected ="All", choices = c("All", "Central", "Metro", "Northern")) })
  
  getyearRange <- reactive({
    #print(paste("pollutant is:", input$pollutant))
    if(is.null(input$pollutant)){c(2002,2011) }
    else{range(filter(toxics, Pollutant == input$pollutant)$year) }
  })
  
  #Generate Years
  output$yearRange <- renderUI({
    sliderInput("years", "", min=getyearRange()[1], max=getyearRange()[2], value=c(getyearRange()[2]-ceiling(getyearRange()[2]-getyearRange()[1])/5, getyearRange()[2]),  format="####", step=1)})
    
  # Return the requested dataset
  dataset <- reactive({
    #print(getyearRange())
    years = c(2002,2011)
    #print(paste("input$years =", input$years))
    ifelse(is.null(input$years), years <- c(2002,2011), years <- input$years)
    #print(paste("years =", years))
    pollutant <- ifelse(is.null(input$pollutant), "Formaldehyde", input$pollutant)
    region <- ifelse(is.null(input$region), "All", input$region)
    #print(paste("region:", region))
    years[1] = ifelse(years[1] < getyearRange()[1] | years[1] > getyearRange()[2], getyearRange()[1], years[1])
    years[2] = ifelse(years[2] > getyearRange()[2] | years[2] < getyearRange()[1], getyearRange()[2], years[2])
    #print(paste("years =", years))
    filter(toxics, Pollutant == pollutant, RegionName == region | region == "All", year >= years[1], year <= years[2])
    
    #if(input$region == "All") {
          #toxics[toxics$Pollutant == input$pollutant & toxics$year >= input$years[1] & toxics$year <= input$years[2],]
          #filter(toxics, Pollutant == input$pollutant, year >= input$years[1], year <= input$years[2])
          #                   }
    #else {
          #toxics[toxics$Pollutant == input$pollutant & toxics$RegionName == input$region & toxics$year >= input$years[1] & toxics$year <= input$years[2],]               
                 
  })
  
  #Generate Site ID List
  output$siteids <- renderUI({
     selectInput("siteid", "", selected = "All", choices = c("All", levels(as.factor(dataset()$MPCAID))))  
    })
     
  dataset2 <- reactive({
    #print(paste("dataset is:", is.null(dataset())))
    if(is.null(dataset())) return(NULL)
    #print(paste("Site id is: ", input$siteid))
    siteid2 <- ifelse(is.null(input$siteid), "All", input$siteid)
    filter(dataset(), siteid2 == "All"| MPCAID == siteid2 )  
    
  })
 
  get_cas <- reactive({
    #print(paste("dataset2 is:", is.null(dataset2())))
    #print(head(dataset()))
    #print(head(dataset2()))
    #print(dataset2()$CAS[1])
    if(is.null(dataset2())) return("50-00-0")
    dataset2()$CAS[1]
  })
  
  risk.1 <- reactive({
    #print(paste("get_cas is:", get_cas()))
    #if(is.null(get_cas())) return(0)
      min_risk = suppressWarnings(min(hbvs[hbvs$CAS == get_cas(),c(6,7)], na.rm=T))
      ifelse(min_risk == Inf, 0, min_risk)
  })
  
  risk.type <- reactive({
    #print(paste("risk.1 is:", risk.1))
    if(get_cas() == "7440-47-3") return("Based on Chromium VI, Cancer Risk = 1 in 100,000 at ")
    if(risk.1() != 0) {
    type = suppressWarnings(which.min(hbvs[hbvs$CAS == get_cas(),c(6,7)] ))
    ifelse(type == 1, "Cancer Risk = 1 in 100,000 at ", "Non-Cancer Hazard Index = 1 at ")
    } else {
      "*No risk value available*"
    }
  })
  
  risk.is <- reactive({
    #if(is.null(dataset2())) return(NULL)
    ifelse( ( risk.1() == 0 | risk.1() > 1.5*suppressWarnings(max(dataset2()$km_UCL)) ), 0, 1)
    
  })

  #output$datais <- reactive({
      #dataset2()
  #})
  

  output$plot <- renderPlot({
    if(!is.null(input$siteid) ) {   
      a <- suppressWarnings(ggplot(data=dataset2(), environment=environment(), aes(x= reorder(groupid2,year), y=km_mean)) +                        
          geom_bar(aes(fill = as.factor(year)), stat="identity") +
          theme(axis.text.x = element_text(size =12.5, lineheight=1, angle = 55, hjust = unit(.8, "cm"), vjust= unit(.6, "cm"), colour="black"), 
                         axis.text.y = element_text(size =13, face="plain", color = "#383838", hjust= unit(-.3, "cm")), 
                         axis.title.x = element_text(size =13.5, face="bold", vjust=-1.2), 
                         axis.title.y = element_text(angle = 90, size =13.4, face="bold", vjust=-0.1), 
                         axis.ticks = element_line(color="grey94"), axis.ticks.length = unit(.3, "cm"),
                         axis.ticks.margin = unit(-.1, "cm"),
                         title = element_text(size =14, face="bold"), 
                         panel.grid.major = element_line(colour = "grey94"), 
                         panel.grid.minor = element_blank(), panel.background = element_blank(), 
                         legend.text = element_text(size=14), legend.key = element_rect(fill=NA, color=NA),
                         legend.background = element_rect(fill=NA, color=NA), legend.position = "top", 
                         legend.title=element_blank(), legend.key.height = unit(.05,"cm"), 
                         plot.margin = unit(c(0,0,.5,.55), "cm"), panel.margin = unit(c(0,0,0,0), "cm")) +          
          geom_errorbar(aes(ymax=km_UCL, ymin=km_mean), color="grey55", show_guide=F) +
          labs( x = "Site ID", y = paste(input$pollutant, "Concentration (ug/m3)"), title= paste("Average", input$pollutant, "Concentration")) +
          scale_x_discrete(labels=as.numeric(dataset2()$MPCAID)) +
          #scale_x_discrete(labels=paste(dataset2()$MPCAID,".", dataset()$poc, sep="")) 
          geom_hline(aes(yintercept= risk.1()*risk.is(), colour = "red" ), linetype= "dashed", size =1.1, alpha = risk.is()*.6, show_guide=T) +    
          scale_fill_discrete("year", guide = guide_legend(override.aes=aes(color=NA), order = 2  ))  +
          scale_colour_manual("zzz", values= "darkred",  labels = paste(risk.type(), risk.1(), " (ug/m3)", sep=""), guide = guide_legend(order= 1, label.theme = element_text(color = "darkred", size=14, angle = 0))) +
          scale_y_continuous(expand = c(0, 0)) )
   }     
   else {
      df <- data.frame()
      a  <- ggplot(df) + 
            theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
                  panel.grid = element_blank(),
                  plot.margin = unit(c(0,0,.5,.55), "cm"), panel.margin = unit(c(0,0,0,0), "cm")) +
            xlim(0, 10) + ylim(0, 10) + 
            #labs(x = "Site ID", y = paste("Concentration in (ug/m3)"), title= "Average Pollutant Concentration") +
            geom_text(aes(5,5, label="No data available for this selection.\n Try selecting 'All' regions or a wider year range."))
           }
    
      suppressWarnings(print(a))
    
  })
  
  output$map <- renderPlot({
    if(is.null(input$siteid)) return(NULL)
    #site <- paste(input$region, ", mn", sep="")
    map("state", "Minnesota", mar=c(0,0,0,0), proj="mercator", myborder=NA, fill=T, bg=NA, col="lightblue")
    map("county", fill = F, col = "darkgreen",
        resolution = 1, lty = 1, lwd=1, bg=NA,
        myborder=NA, mar = c(0,0,0,0), region="minnesota", add=T, proj = "mercator")
    #a<- get_map(location = site, zoom = 12, color="bw", source = "osm")
    #print(qmap(site, color="bw", zoom = 13, source = "osm"))
  })
  
  output$table = renderDataTable({
    if(is.null(input$siteid)) return(NULL)
    dataset2()[,c(3,5,7,14,16,19,20,22,31:35)]
  })

  
})
