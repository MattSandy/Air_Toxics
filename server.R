#Air Toxics server.R
require("grid")
require("ggplot2")
require("scales")
library(dplyr)
library(rCharts)
library(shiny)

options("digits"= 4)


toxics<- read.csv(file="toxics_2014.csv", header=T, stringsAsFactors=F, nrows=7000 )
#saveRDS(toxics, file="toxics_2013.rds")
#toxics<- readRDS(file="toxics_2013.rds")
hbvs <- read.csv(file="hbvs.csv", header=T, stringsAsFactors=F, nrows=70 )
pol_list<-levels(as.factor(toxics$Pollutant))


#*#*#*#*# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
  
  #Generate Pollutant List
  output$pollutants <- renderUI({
    selectInput("pollutant", "", selected = "Formaldehyde", choices = pol_list ) })
  
   getyearRange <- reactive({
     ranges <- c(2002,2013)
    if(!is.null(input$pollutant)) ranges <- range(filter(toxics, Pollutant == input$pollutant)$year)
    ranges
   })
  
  
    value_has_not_changed <- reactiveValues(x=0)
  
  in_years <- reactive ({
    years <- c(2002,2004)
    ifelse(is.null(input$years), years<- c(2002,2004), years<- input$years)
    years
  })
  
  #Generate Years
  output$yearRange <- renderUI({
    newvalue <- c(2002,2004)
    ranges <- getyearRange()  
    slider = isolate(in_years())
    if(!is.null(slider)){
      newvalue <- c(max(c(slider[1], ranges[1])), min(c(slider[2], ranges[2])))
      newvalue[1] = ifelse(newvalue[1] < getyearRange()[1] | newvalue[1] >= getyearRange()[2], getyearRange()[1], newvalue[1])
      newvalue[2] = ifelse(newvalue[2] > getyearRange()[2] | newvalue[2] <= getyearRange()[1], getyearRange()[2], newvalue[2])
      if(newvalue[1] == slider[1] & newvalue[2] == slider[2]) value_has_not_changed[["x"]] <-  isolate(value_has_not_changed[["x"]]) + 1
      }
    
    sliderInput("years", "", min=ranges[1], max= ifelse(ranges[1]==ranges[2],ranges[2]+1,ranges[2]), value=c(newvalue[1], ifelse(ranges[1]==ranges[2],newvalue[2]+1, newvalue[2])),  format="####", step=1)
    
    })

  
  # Return the requested dataset
  dataset <- reactive({
    w <- value_has_not_changed[["x"]] 
    years <- c(2002,2004)
    if(!is.null(input$years)) years <- isolate(input$years)
    ifelse(is.null(isolate(input$pollutant)), pollutant <-  "Formaldehyde", pollutant <- isolate(input$pollutant))
    filter(toxics, Pollutant == pollutant, year >= years[1], year <= years[2])
    
  })
 
  #Generate Regions List
    output$regions <- renderUI({
      selectInput("region", "", selected= "All", choices = c("All", levels(as.factor(dataset()$RegionName)), levels(as.factor(dataset()$County)))) })
  
  #Filter by region
  dataset1 <- reactive({
    region <- ifelse(is.null(input$region), "All", input$region)
    filter(dataset(), region == "All" | RegionName == region | County == region )
  })
  
  #Generate Site ID List
  output$siteids <- renderUI({
     selectInput("siteid", "", selected = "All", choices = c("All", levels(as.factor(dataset1()$SiteId ))))  
    })
     
  output$download <- downloadHandler(
       
       filename = function() { paste("MPCA_Air_Data_2002_2014.csv", sep="") },
       content = function(con) {
         toxics = left_join(toxics, hbvs[,c(1,4:7)], by="CAS")
         write.csv(toxics, con, row.names=F)}
  )
  
  dataset2 <- reactive({
    siteid2 <- ifelse(is.null(input$siteid), "All", input$siteid)
    filter(dataset1(), siteid2 == "All"| SiteId == siteid2 )  
    #if(nrow(dataset()==0)) return(NULL)
  })
     
  get_cas <- reactive({
    if(nrow(dataset2())==0) return(NULL)
    dataset2()$CAS[1]
  })
  
  risk.1 <- reactive({
  
     if(is.null(get_cas())) return(NULL)
     if(input$time == "Second_Highest"){min_risk = hbvs[hbvs$CAS == get_cas(),4][1]}
     else{min_risk = suppressWarnings(min(hbvs[hbvs$CAS == get_cas(),c(6,7)], na.rm=T))}
     ifelse(min_risk == Inf | is.na(min_risk), 0, min_risk)
  })
  
  risk.type <- reactive({
       
    if(is.null(risk.1())) return(NULL)
    if(input$time == "Second_Highest") ifelse( risk.1() != 0, return("Acute Hazard Index = 1 at "), return("          *No risk value available"))
    
    if(get_cas() == "7440-47-3") return("Assuming 10% as Chromium VI, Cancer Risk = 1 in 100,000 at ")
    
    if(risk.1() != 0) {
    type = suppressWarnings(which.min(hbvs[hbvs$CAS == get_cas(),c(6,7)] ))
    ifelse(type == 1, "Cancer Risk = 1 in 100,000 at ", "Non-Cancer Hazard Index = 1 at ")
    } else {"          *No risk value available"}
    
  })
  
  units.are <- reactive({
    if(is.null(risk.1())) return(NULL)
    ifelse(risk.1() != 0, paste(format(risk.1(),big.mark=','), "ug/m3   "), "                 ")
    
  })
  
  risk.is <- reactive({
    if(is.null(risk.1())) return(0)
    if(input$time == "Second_Highest") { ifelse( ( risk.1() == 0 | risk.1() > 1.5*suppressWarnings(max(dataset2()[,"Second_Highest"])) ), 0, 1)}
    else {ifelse( ( risk.1() == 0 | risk.1() > 1.5*suppressWarnings(max(dataset2()$km_UCL)) ), 0, 1)}
    
  })
  
  titlez <- reactive({
    if(is.null(risk.1()) | !nrow(dataset2())> 0) return("No data available for this selection.")
    ifelse(input$time=="km_mean", paste("Average Annual", isolate(input$pollutant), "Concentration"), paste("Second Highest", isolate(input$pollutant), "Concentration"))
  })
  
  risk <- reactive({
    if(is.null(risk.1()) | !nrow(dataset2())> 0) return("Try selecting additional years.")
    paste(risk.type(), units.are(),sep="")
    })
  
  output$title <- renderText({
    titlez()
    })
  
  output$risk <- renderText({
    risk()
  })
  
  #output$credits <- renderText({
   # paste("hello",sep="")
  #})
  
 #--------------------------------# 
  
  output$map <- renderMap({
    
    if(is.null(isolate(input$pollutant)) | is.null(dataset2()) ) {map <- Leaflet$new()
                                                                  map$setView(c(46.15, -94.6), zoom = 6)
                                                                  map$tileLayer(provider = "Stamen.TonerLite", zoom=8)
                                                                  #map$set(width=1150, height = 460) 
            }
    
    else if(nrow(dataset2())>0){
      d2 <- dataset2()[,c("year","MPCAID", "lat","long", "SiteId", input$time)]
      names(d2)[6] <- "Conc"
      d2 <- group_by(d2, MPCAID) %.% mutate(avg = mean(Conc)) %.% filter(year == year[1])
      nums <- length(unique(d2$avg))
      labs <- if(input$time == "km_mean") c("#081D58","#0088EE", "#44BBCC","#99DDBB","#d9f0a3","#f7fcb9") else c("#67001f","#810f7c", "#88419d","#8c96c6","#9ebcda","#e0ecf4")  
      cuts <- quantile(c(d2$avg), c(.01,.25,.5,.8,.995))
      #cuts <- seq(from=.95*min(d2$avg), to=max(d2$avg), (max(d2$avg)-.95*min(d2$avg))/4)
      #cuts2 <- seq(from=.95*min(d2$avg), to=max(d2$avg),(max(d2$avg)-.95*min(d2$avg))/10)
      options(digits=7)
      cuts2 <- quantile(c(d2$avg,.985*min(d2$avg), 1.015*max(d2$avg)), seq(from=0, to=1, 1/30))
      if(length(cuts2[duplicated(cuts2)])>0) cuts2 = sapply(1:31, function(x) ifelse(cuts2[x] %in% cuts2[-c(1:x)], .9999*cuts2[x], cuts2[x]))
      if(length(cuts2[duplicated(cuts2)])>0) cuts2 = seq(from=.9*min(d2$avg), to=1.05*max(d2$avg),(1.05*max(d2$avg)-.9*min(d2$avg))/30)
      #dat_list <- toJSONArray2(d1, json = F)
      d2$Conc = signif(d2$avg,3)
      dat_list <- lapply(1:nrow(d2), function(x) d2[x,])
      dat_list <- lapply(dat_list, function(station){within(station, {
        fillColor = cut(avg, breaks = cuts2, right=F, labels = colorRampPalette(rev(labs[1:5]))(30), include.lowest=T)
        popup = iconv(whisker::whisker.render(
          '<b>{{SiteId}}</b><br><hr style="height = 3px; margin:0; margin-bottom:5px; padding:0; background-color: {{fillColor}}; border-color: {{fillColor}};"/>
          Concentration  =  <b><code style="border:0; background-color: white; color: black;"> {{Conc}} ug/m3 </b></code> <br>
          <p>Coordinates: {{lat}},  {{long}}</p>'
        ), from = 'latin1', to = 'UTF-8')   }) })
      map <- Leaflet$new()
      map$setView(c(mean(range(d2$lat)), mean(range(d2$long))), zoom = 13+round(-4.7*((max(d2$lat)-min(d2$lat))^(1/2.5))))
      map$tileLayer(provider = "Stamen.TonerLite")
      map$geoJson(toGeoJSON(dat_list, lat = 'lat', lon = 'long' ),
                  onEachFeature = '#! function(feature, layer){layer.bindPopup(feature.properties.popup)} !#',
                  pointToLayer =  "#! function(feature, latlng){return L.circleMarker(latlng, {
                  radius: 11, fillColor: feature.properties.fillColor || 'grey',    
                  color: '#000', weight: 1, fillOpacity: 0.88, title: feature.properties.popup }) } !#"                                                               )
      map$legend(position = 'topleft', colors = if(nums==1) labs[1:2] else labs[1:5], labels = as.vector(if(nums==1) c(signif(rev(cuts)[1]*c(1.01,.95),2)) else c(rev(signif(cuts,2)))))
      #map$fullScreen(TRUE)
    }
    else{          
      map <- Leaflet$new()
      map$setView(c(46.15, -94.6), zoom = 6)
      map$tileLayer(provider = "Stamen.TonerLite", zoom=8)
      map$enablePopover(TRUE)
      map$fullScreen(TRUE)
    }
    suppressWarnings(return(map))
    
  })
  
  output$barplot <- renderPlot({
    
    if(is.null(isolate(input$pollutant)) | is.null(dataset2()) ) {
      df <- data.frame()
      a  <- ggplot(df) + 
        theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
              panel.grid = element_blank(), plot.margin = unit(c(0,0,.5,.55), "cm"),
              panel.margin = unit(c(0,0,0,0), "cm")) +
              xlim(0, 10) + ylim(0, 10) 
              }
      else if(nrow(dataset2())>0) {
            bar2 <- dataset2()[,c("year","groupid2", "SiteId", input$time, "km_UCL")]
            names(bar2)[4] <- "Conc"
            bar2[bar2$SiteId == "964  -  Minneapolis-St Paul International Airport", "SiteId"] <- "964  -  MSP Intl. Airport"
            num <-length(unique(bar2$groupid2))
          a <- suppressWarnings(ggplot(data= bar2, environment=environment(), aes(x= reorder(groupid2,year), y=bar2$Conc) ) +                        
            geom_bar(aes(fill = as.factor(year)), stat="identity") +
            theme(axis.text.x = element_text(size = ifelse(num <20, 14, 49/(num^.38)), lineheight=1, angle = 45, hjust = 1, vjust= 1, color="#080808"), 
                         axis.text.y = element_text(size =13.5, face="plain", color = "#383838", hjust= unit(-.25, "cm")), 
                         axis.title.x = element_text(size =13.6, face="bold", vjust=-1.2), 
                         axis.title.y = element_text(angle = 90, size =13.4, face="bold", vjust=-0.1), 
                         axis.ticks = element_line(color="grey89"), axis.ticks.length = unit(.2, "cm"),
                         axis.ticks.margin = unit(0.05, "cm"),
                         title = element_text(size =14, face="bold"), 
                         panel.grid.major = element_line(colour = "grey90"), 
                         panel.grid.minor = element_blank(), panel.background = element_blank(), 
                         legend.text = element_text(size=14), legend.key = element_rect(fill=NA, color=NA),
                         legend.background = element_rect(fill=NA, color=NA), legend.position = "top", 
                         legend.title=element_blank(), 
                         #legend.key.height = unit(.09,"cm"), legend.key.width = unit(.8,"cm"), legend.key.size = unit(1.3,"cm"),
                         plot.margin = unit(c(0,0,.53,1.55), "cm"), panel.margin = unit(c(0,0,0,0.6), "cm")) +          
            geom_errorbar(aes(ymax=km_UCL*as.numeric(input$time=="km_mean"), ymin=Conc*as.numeric(input$time=="km_mean")), color="grey55", show_guide=F) +
            labs( x = "Site ID", y = paste(isolate(input$pollutant), "Concentration (ug/m3)"), title= titlez()  ) +
            scale_x_discrete(breaks = bar2$groupid2, labels=bar2$SiteId, expand = c(0.01, 0.01)) + 
            geom_hline(aes(yintercept= risk.1()*risk.is(), colour = "darkred" ), linetype= "dashed", size =1.2, alpha = risk.is()*.7, show_guide=T) +    
            scale_fill_discrete("year", labels= paste("  ", levels(factor(bar2$year)), " "), guide = guide_legend(override.aes=aes(color=NA), keywidth=2.7, keyheight=.8, key.size=unit(4, "cm"), label.position = "bottom", order = 2, hjust= unit(1.25, "cm") ))  +
            scale_colour_manual("risk", values= "darkred",  labels = risk(), guide = guide_legend(order= 1, keywidth=2.9, label.theme = element_text(color = "darkred", size=14, lineheight=.5, angle=0))) +
            scale_y_continuous(limits = c(0, max(c(bar2$Conc, bar2$km_UCL))*1.1)) 
          )
   }     
   else{
      df <- data.frame()
      a  <- ggplot(df) + 
            theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
                  panel.grid = element_blank(),
                  plot.margin = unit(c(0,0,.5,.55), "cm"), panel.margin = unit(c(0,0,0,0), "cm")) +
            xlim(0, 10) + ylim(0, 10) + 
            geom_text(aes(5,5, label="No data available for this selection.\n Try selecting additional years."), color="red")
           }
    
      suppressWarnings(print(a))
    
  })
  
  output$trends <- renderChart({
    
    if(is.null(isolate(input$pollutant)) | is.null(dataset2()) ) {
      df <- data.frame(km_mean = rep(NA,12), Year= rep(NA,12))
      suppressWarnings(h1 <- hPlot(x="Year", y = "km_mean", type="line", data = df))
      h1$addParams(dom = 'trends')
      h1$xAxis(title = list(text=""), labels = list(enabled=F), tickLength=0, lineWidth=0)
      h1$yAxis(title = list(text = ""))
      h1$legend(enabled=F)
      h1$chart(height=450, spacingLeft=5, marginBottom=45, marginRight=0, spacingRight=0)
      
    }
    else if(nrow(dataset2())>0){
       trend2 <- dataset2()[,c("year","MPCAID", "SiteId", input$time)]
       counts <- group_by(trend2, MPCAID) %.% summarise(count = length(MPCAID))
       if(length(unique(trend2$MPCAID)) > 7) trend2 <- trend2[trend2$MPCAID %in% counts[counts$count >1,1], ] 
       if(length(unique(trend2$MPCAID)) < 2) trend2 <- dataset2()[,c("year","MPCAID", "SiteId", input$time)]
       names(trend2)[4] <- "Conc"
       trend2$Conc <- signif(trend2$Conc, digits=3)
       h1 <- hPlot(x="year", y = "Conc", type="line", data = trend2, group="SiteId", radius=4.2)
         h1$addParams(dom = 'trends')
         h1$tooltip(followPointer =T, hideDelay = 0, animation=T, shared=F)
         h1$xAxis(categories = unique(trend2$year), title = list(style=list(fontSize="13px"), text="Year"), type="category")
         h1$yAxis(plotLines=list(list(zIndex=1, value=risk.1()*risk.is(), color="darkred", width=2*risk.is(), dashStyle="shortdash", label=list(align="top",verticalAlign="top", text="Health Standard", style=list(fontWeight="bold", color="darkred")))), min = 0.95*min(trend2$Conc), max = 1.05*max(trend2$Conc), ceiling = 100, title = list(style=list(fontSize="13px"), text = "Concentration (ug/m3)"))
         h1$chart(height=450, spacingLeft=5, spacingTop=0, marginBottom=50, marginRight=0, spacingRight=0)
         h1$legend(margin=8, redraw=F, symbolWidth=20, symbolPadding=5, x=40, y=42, align="center", verticalAlign="top", floating=F, borderWidth=0, padding=10, width=770)
         h1$title(margin=35, style= list(fontWeight="bold", color="black"), text = titlez()) 
         h1$subtitle(y=40, style= list(color="darkred", fontSize="13.5px", fontWeight="bold"), text=risk())
         h1$plotOptions(series=list(shadow=T, data= list(3)), pointStart= 2002, pointInterval=1)
    }     
    else {df <- data.frame(km_mean = rep(NA,12), Year= seq(from=2002, to=2013))
          suppressWarnings(h1 <- hPlot(x="Year", y = "Average Concentration", type="line", data = df))
          h1$addParams(dom = 'trends')
          h1$xAxis(title = list(style=list(fontSize="13px"), text="Year"), type="category", categories = df$Year, min =2002, max=2013)
          h1$yAxis(min = 0, ceiling = 10, title = list(style=list(fontSize="13px"), text = paste(isolate(input$pollutant), "Concentration (ug/m3)")))
          h1$legend(enabled=F)
          h1$chart(height=450, spacingLeft=5, marginBottom=45, marginRight=0, spacingRight=0, plotBackgroundColor= "#E8E8E8")
          h1$title(style= list(color="red"), text = "No data available for this selection.")
          h1$subtitle(text = "Try selecting additional years.")        
    }
    
    suppressWarnings(return(h1))
    
  })


  output$table = renderDataTable({
    if(is.null(input$siteid)) return(NULL)
    data<-dataset2()
    data$lat=as.character(round(data$lat, digits=3))
    data$long=as.character(round(data$long, digits=3))
    options("digits"= 2)
    names(data)[c(19,1,3,7:9,13,14,21,17:18,10:12,15 )] <- c("Site_Id","AQS_ID","Year","Region","Pollutant","CAS","Second_Max","Average","UCL_95","Lat","Long", "Parameter", "Detects", "Detects_pct", "StdDev")
    if(is.null(input$allData) | input$allData == F) data[ ,c(19,22,3,8,14,21,13,23)] 
    else {options("digits"= 3)
          data <- left_join(data, hbvs[,c(1,4:7)], by="CAS")
          data[,c(19,22,3,8,9,14,21,13,15,11,12,7,23,17,18,24:27)] }
  }, options= list(bLengthChange=T, aLengthMenu = c(5, 10, 30), iDisplayLength = 5, bFilter=T))
  

})
