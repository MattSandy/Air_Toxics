#summary server.R

library(shiny)
library(ggmap)
library(maps)
library(ggplot2)
require(grid)

options("digits"=4)
warn=-1
#site="minneapolis, mn"
#a= get_map(location = site, zoom = 12, color="bw", source = "osm")

toxics<- read.csv(file="summary2002_12.csv", header=T, stringsAsFactors=F, nrows=7000 )
hbvs <- read.csv(file="hbvs.csv", header=T, stringsAsFactors=F, nrows=70 )
#load("summary2002_12.rda")
toxics<-na.omit(toxics)
pol_list<-levels(as.factor(toxics$Pollutant))

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  #Generate Pollutant List
  output$pollutants <- renderUI({
    selectInput("pollutant", "", choices = pol_list, selected = "Formaldehyde" )})
  
  #Generate Site ID List
  output$siteids <- renderUI({
    selectInput("siteid", "", choices = c("All", levels(as.factor(dataset()$MPCAID)) ), selected = "All" ) })
  
  
  # Return the requested dataset
  dataset <- reactive({
    if(input$region == "All") {
          toxics[toxics$Pollutant == input$pollutant & toxics$year >= input$years[1] & toxics$year <= input$years[2],]
                             }
    else{
          toxics[toxics$Pollutant == input$pollutant & toxics$RegionName == input$region & toxics$year >= input$years[1] & toxics$year <= input$years[2],]               
                             }
  })
  
  dataset2 <- reactive({
    if(input$siteid == "All") {
      dataset()
    }
    else {
      dataset()[dataset()$MPCAID == input$siteid,]
    }
  })
  
  get_cas <- reactive({
    dataset2()$CAS[1]
  })
  
  risk.1 <- reactive({
      min_risk = suppressWarnings(min(hbvs[hbvs$CAS == get_cas(),c(6,7)], na.rm=T))
      ifelse(min_risk == Inf, 0, min_risk)
  })
  
  risk.type <- reactive({
    if(risk.1() != 0) {
    type = suppressWarnings(which.min(hbvs[hbvs$CAS == get_cas(),c(6,7)] ))
    ifelse(type == 1, "Cancer Risk = 1 in 100,000 at ", "Hazard Index = 1 at ")
    } else {
      ""
    }
  })
  
  risk.is <- reactive({
    ifelse( ( risk.1() == 0 | risk.1() > 1.5*suppressWarnings(max(dataset2()$km_UCL)) ), 0, 1)
    
  })
  
  
  # Generate a summary of the dataset
  #output$summary <- renderPrint({
  #summary(dataset())
  #})
  
  output$plot <- renderPlot({
    #data <- dataset()
    print(get_cas())
    print(hbvs[hbvs$CAS == get_cas(),])
    print(risk.1())
    print(risk.is())
    print(as.factor(dataset2()$year))
    cond = c("A", "B")
    x="d"
    a <- suppressWarnings(ggplot(data=dataset2(), environment=environment(), aes(x=reorder(groupid2, year), y=km_mean)) )
    theme_update(axis.text.x = element_text(size =12.5, lineheight=1, angle = 55, hjust = 1, colour="black"), axis.text.y = element_text(size =13, face="plain", color = "#383838"), legend.text = element_text(size=13), axis.title.x = element_text(size =13.5, face="bold", vjust=0), axis.title.y = element_text(angle = 90, size =13.2, face="bold", vjust=0.2), title = element_text(size =14, face="bold"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_line(color="gray"), legend.position = "top")
    b <- a + geom_bar(aes(fill = as.factor(dataset2()$year)), alpha =.9, stat="identity", position = "stack") +
          geom_errorbar(aes(ymax=km_UCL, ymin=km_mean), color="gray", show_guide=F) +
          labs( x = "Site ID", y = paste(input$pollutant, "Concentration (ug/m3)"), title= paste("Average", input$pollutant, "Concentration"), size = 15, show_guide=F) +
          scale_x_discrete(labels=as.numeric(dataset2()$MPCAID)) +
          #scale_x_discrete(labels=paste(dataset2()$MPCAID,".", dataset()$poc, sep="")) 
          geom_hline(aes(yintercept= risk.1()*risk.is(), colour = "red" ), linetype= "dashed", size =1.1, alpha = risk.is()*.6, show_guide=T) +
          #geom_text(aes(0, max(km_UCL),label = paste(risk.type(), risk.1(), " (ug/m3)", sep=""), vjust = 1, hjust= 0), alpha = risk.is(), color="#cc0000", size=4.8, family="sans", fontface="plain") +
          #guides(colour = guide_legend(title=paste(risk.type(), "( ", risk.1(), " ug/m3 )", sep=""), nrow=1, label =T, show =T, breaks = paste(risk.type(), "( ", risk.1(), " ug/m3 )", sep="") )) +
          #guides(fill = guide_legend ()) +
          theme(legend.key.height = unit(.1,"cm"), plot.margin = unit(c(0,0,.2,.2), "cm"), panel.margin = unit(c(0,0,0,0), "cm")) +
          scale_fill_discrete("year", guide = guide_legend(override.aes=aes(color=NA)  ))  +
          #guides(fill = guide_legend("year", border ="white", density = "white", color = "white")) +
          scale_colour_manual(" ", values="darkred", labels = paste(risk.type(), risk.1(), " (ug/m3)", sep=""), guide = guide_legend(label.theme = element_text(color = "darkred", angle = 0) )) +
          theme(legend.title=element_blank())
    
    suppressWarnings(print(b))
    
  })
  
  output$map <- renderPlot({
    #site <- paste(input$region, ", mn", sep="")
    map("state", "Minnesota")
    map("county", fill = F, col = "darkgreen",
        resolution = 0, lty = 1, lwd=1,
        myborder = 0, mar = c(0,0,0,0), region="minnesota", add=T)
    #a<- get_map(location = site, zoom = 12, color="bw", source = "osm")
    #print(qmap(site, color="bw", zoom = 13, source = "osm"))
  })
  
  output$table = renderDataTable({
    #dataset2()[,c(3,5,7,20,22,33,34)]
    dataset2()[,-c(1,2,8:20,22:27)]
  })
  
})
