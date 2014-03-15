#summary server.R

library(shiny)
library(ggmap)
library(maps)
library(ggplot2)

options("digits"=4)


#site="minneapolis, mn"
#a= get_map(location = site, zoom = 12, color="bw", source = "osm")

toxics<-read.csv(file="summary2002_12.csv", header=T, stringsAsFactors=F, nrows=7000 )
#load("summary2002_12.rda")
toxics<-na.omit(toxics)
pol_list<-levels(as.factor(toxics$Pollutant))


# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
  dataset <- reactive({
         toxics[toxics$Pollutant == input$pollutant & toxics$year >= input$years[1] & toxics$year <= input$years[2],]
                      })
  
  
  # Create a summary Table
  output$view <- renderTable({
    dataset <- toxics
    dataset <- subset(dataset, Pollutant == input$pollutant)
    #dataset <- subset(dataset, RegionName == input$region)
    dataset <- na.omit(dataset)
    dataset <- dataset[dataset$year >= input$years[1] & dataset$year <= input$years[2],]
    head(dataset, n=8)
  })
  
  output$plot <- renderPlot({
    data <- dataset()
    a<-ggplot(data=dataset(), aes(x=reorder(groupid2, year), y=km_mean, fill=factor(year)), position="dodge")
    theme_update(axis.text.x = element_text(size =13.5, angle = 90, hjust = 2, vjust = .3, lineheight = 1, colour="black"), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.position = "top")
    b=a + geom_bar(stat="identity") + geom_errorbar( aes(ymax=km_UCL, ymin=km_mean), color="gray", position="dodge") + labs( x = "Site ID", y = paste(input$pollutant, "Concentration"), title= paste(input$pollutant, "Concentration"), fill= "") + guides(color=F, fill = guide_legend(reverse=F)) + scale_x_discrete(labels=paste(dataset()$MPCAID,".", dataset()$poc, sep=""))
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
  
})
