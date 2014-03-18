#summary server.R

library(shiny)
library(ggmap)
library(maps)
library(ggplot2)

options("digits"=4)

#site="minneapolis, mn"
#a= get_map(location = site, zoom = 12, color="bw", source = "osm")

toxics<- read.csv(file="summary2002_12.csv", header=T, stringsAsFactors=F, nrows=7000 )
hbvs <- read.csv(file="hbvs.csv", header=T, stringsAsFactors=F, nrows=70 )
#load("summary2002_12.rda")
toxics<-na.omit(toxics)
pol_list<-levels(as.factor(toxics$Pollutant))

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
  dataset <- reactive({
    toxics[toxics$Pollutant == input$pollutant & toxics$year >= input$years[1] & toxics$year <= input$years[2],]
  })
  
  cas <- reactive({
    dataset()$CAS[[1]]
  })
  
  risk <- reactive({
      min(hbvs[hbvs$CAS == cas(),c(6,7)], na.rm=T)   
  })
  
  risk_type <- reactive({
    names(which.min(hbvs[hbvs$CAS == cas(),c(6,7)])
  })
  
  #Generate Pollutant List
  output$pollutants <- renderUI({
    selectInput("pollutant", "", choices = pol_list, selected = "Formaldehyde" )})
  
  
  # Generate a summary of the dataset
  #output$summary <- renderPrint({
  #summary(dataset())
  #})
  
  output$plot <- renderPlot({
    #data <- dataset()
    a<-ggplot(data=dataset(), aes(x=reorder(groupid2, year), y=km_mean, fill=factor(year)), position="dodge")
    theme_update(axis.text.x = element_text(size =13.5, angle = 90, hjust = 1.5, vjust = .3, lineheight = 1, colour="black"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.position = "top")
    b=a + geom_bar(stat="identity") +
          geom_errorbar(aes(ymax=km_UCL, ymin=km_mean), color="gray") +
          geom_hline(aes(yintercept=2), color="#cc0000", linetype="dashed") +
          geom_text(aes(groupid2[1],2,label = " Chronic Cancer Risk = 1 in 100,000", vjust = -.5, hjust=0), color="#cc0000", size=4.8, family="sans", fontface="plain") +
          labs( x = "Site ID", y = paste(input$pollutant, "Concentration"), title= paste(input$pollutant, "Concentration"), fill= "") +
          guides(color=F, fill = guide_legend(reverse=F)) +
          scale_x_discrete(labels=paste(dataset()$MPCAID,".", dataset()$poc, sep=""))
    print(b)
    
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
    dataset()[,c(3,5,7,20,22,33,34)]
  })
  
})
