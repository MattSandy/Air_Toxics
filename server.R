#server.R for Air Toxics 2002-2013
library(shiny)
library(dplyr)
library(rCharts)

# shinyapps::configureApp("Air_Toxic", size="large")
#options("digits"= 4)
#toxics<- read.csv(file="toxics_2014.csv", header=T, stringsAsFactors=F, nrows=7000 )
#saveRDS(toxics, file="toxics_2014.rds")

toxics <- readRDS(file="toxics_2014.rds")
hbvs <- read.csv(file="hbvs.csv", header=T, stringsAsFactors=F, nrows=70 )
pol_list <-levels(as.factor(toxics$Pollutant))

#*# Server logic to summarize the selected dataset and map the monitor network
shinyServer(function(input, output, session) {
  
  #Generate Pollutant List
  output$pollutants <- renderUI({
    selectInput("pollutant", "", selected = ifelse(is.null(input$pollutant),"Formaldehyde",input$pollutant), 
                choices = if(input$tabs1=='Monitors') c("All",pol_list) else pol_list) 
    })
  
  getyearRange <- reactive({
    ranges <- c(2010,2013)
    if(!is.null(input$pollutant)) ranges <- range(filter(toxics, Pollutant == input$pollutant | input$pollutant =="All")$year)
    ranges
  })
  
  
  value_has_not_changed <- reactiveValues(x=0)
  
  in_years <- reactive ({
    years <- c(2010,2013)
    ifelse(is.null(input$years), years<- c(2010,2013), years<- input$years)
    years
  })
  
  #Generate Years
  output$yearRange <- renderUI({
    newvalue <- c(2010,2013)
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
  
  
  # Return the requested dataset Filtered by Pollutant
  dataset <- reactive({
    w <- value_has_not_changed[["x"]] 
    years <- c(2010,2013)
    if(!is.null(input$years)) years <- isolate(input$years)
    ifelse(is.null(isolate(input$pollutant)), pollutant <-  "Formaldehyde", pollutant <- isolate(input$pollutant))
    filter(toxics, Pollutant == pollutant | pollutant == "All", year >= years[1], year <= years[2])
    
  })
  
  #Generate Regions List
  output$regions <- renderUI({
    selectInput("region", "", selected= "All", choices = c("All", levels(as.factor(toxics$RegionName)), levels(as.factor(toxics$County)))) })
  
  #Filter dataset by selected region 
  dataset1 <- reactive({
    region <- ifelse(is.null(input$region), "All", input$region)
    filter(dataset(), region == "All" | RegionName == region | County == region )
  })
  
  #Generate Site ID List
  output$siteids <- renderUI({
    selectInput("siteid", "", selected = "All", choices = c("All", levels(as.factor(dataset1()$SiteId ))))  
  })
  
#Filter dataset by selected Site ID
  dataset2 <- reactive({
    siteid2 <- ifelse(is.null(input$siteid), "All", input$siteid)
    filter(dataset1(), siteid2 == "All"| SiteId == siteid2 )  
    #if(nrow(dataset()==0)) return(NULL)
  })
  
#Download Button
output$download <- downloadHandler(
  
  filename = function() { paste("MPCA_Air_Data_2002_2014.csv", sep="") },
  content = function(con) {
    toxics = left_join(toxics, hbvs[,c(1,4:7)], by="CAS")
    write.csv(toxics, con, row.names=F)}
)

#CAS number of selected pollutant
  get_cas <- reactive({
    if(nrow(dataset2())==0) return(NULL)
    dataset2()$CAS[1]
  })
  
#Get minimum risk value
  risk.1 <- reactive({ 
    if(is.null(get_cas())) return(NULL)
    if(input$time == "Second_Highest"){min_risk = hbvs[hbvs$CAS == get_cas(),4][1]}
    else{min_risk = suppressWarnings(min(hbvs[hbvs$CAS == get_cas(),c(6,7)], na.rm=T))}
    ifelse(min_risk == Inf | is.na(min_risk), 0, min_risk)
  })
  
#Get risk description for charts and map
  risk.type <- reactive({
    
    if(is.null(risk.1())) return(NULL)
    if(input$time == "Second_Highest") ifelse( risk.1() != 0, return("Acute Hazard Index of 1 at "), return("          No risk value available"))
    if(risk.1() != 0) {
      type = suppressWarnings(which.min(hbvs[hbvs$CAS == get_cas(),c(6,7)] ))
      ifelse(type == 1, "Cancer Risk of 1 in 100,000 at ", "Acute Hazard Index of 1 at ")
    } else {"          No risk value available"}
    
  })
  
#Get units
  units.are <- reactive({
    if(is.null(risk.1())) return(NULL)
    if(risk.1() == 0) return("                 ")
    if(get_cas() == "7440-47-3") return( paste(format(risk.1(),big.mark=','), "ug/m3 (assuming 10% as Chromium VI)" ))
    paste(format(risk.1(),big.mark=','), "ug/m3")
  })
  
#Hide risk value if 1.5X above sample measurments 
  risk.is <- reactive({
    if(is.null(risk.1())) return(0)
    if(input$time == "Second_Highest") { ifelse( ( risk.1() == 0 | risk.1() > 1.5*suppressWarnings(max(dataset2()[,"Second_Highest"])) ), 0, 1)}
    else {ifelse( ( risk.1() == 0 | risk.1() > 1.5*suppressWarnings(max(dataset2()$Boot_UCL)) ), 0, 1)}
    
  })

#Paste risk description with units
risk <- reactive({
  if(is.null(risk.1()) | !nrow(dataset2())> 0) return("Try selecting additional years.")
  paste(risk.type(), units.are(),sep="")
})

# Paste title together
  titlez <- reactive({
    if(is.null(risk.1()) | !nrow(dataset2())> 0) return("No data available for this selection.")
    ifelse(input$time=="km_mean", paste("Average", isolate(input$pollutant), "Concentration"), paste("Second Highest", isolate(input$pollutant), "Concentration"))
  })
  
# Paste Monitor Network title 
  titleM <- reactive({
    paste0("MPCA Air Monitoring Network ", input$years[1], " - ", input$years[2])
})

  output$title <- renderText({
    titlez()
  })

  output$titleM <- renderText({
    titleM()
  })

  output$risk <- renderText({
    risk()
  })
  
  output$map <- renderMap({
    
    if(is.null(isolate(input$pollutant)) | is.null(dataset2()) ) {map <- Leaflet$new()
                                                                  map$setView(c(46.15, -94.1), zoom = 6)
                                                                  map$tileLayer(provider = "Stamen.TonerLite", zoom=8)
                                                                  map$set(width=1150, height = 460) 
    }
    
    else if(nrow(dataset2())>0){
      d2 <- dataset2()[,c("year","MPCAID", "lat","long", "SiteId", input$time)]
      names(d2)[6] <- "Conc"
      if(input$time == "km_mean") d2 <- group_by(d2, MPCAID) %.% mutate(avg = mean.default(Conc)) %.% filter(year == year[1])
      else d2 <- group_by(d2, MPCAID) %.% mutate(avg = max(Conc)) %.% filter(year == year[1])
      nums <- length(unique(d2$avg))
      labs <- if(input$time == "km_mean") c("#081D58","#0088EE", "#44BBCC","#99DDBB","#d9f0a3","#f7fcb9") else c("#67001f","#810f7c", "#88419d","#8c96c6","#9ebcda","#e0ecf4")  
      cuts <- quantile(c(d2$avg), c(.01,.25,.5,.8,.998))
      options(digits=7)
      cuts2 <- quantile(c(d2$avg,.985*min(d2$avg), 1.015*max(d2$avg)), seq(from=0, to=1, 1/30))
      if(length(cuts2[duplicated(cuts2)])>0) cuts2 = sapply(1:31, function(x) ifelse(cuts2[x] %in% cuts2[-c(1:x)], .9999*cuts2[x], cuts2[x]))
      if(length(cuts2[duplicated(cuts2)])>0) cuts2 = seq(from=.9*min(d2$avg), to=1.05*max(d2$avg),(1.05*max(d2$avg)-.9*min(d2$avg))/30)
      d2$avg = signif(d2$avg,3)
      dat_list <- lapply(1:nrow(d2), function(x) d2[x,])
      dat_list <- lapply(dat_list, function(station){within(station, {
        fillColor = cut(avg, breaks = cuts2, right=F, labels = colorRampPalette(rev(labs[1:5]))(30), include.lowest=T)
        popup = iconv(whisker::whisker.render(
          '<b>{{SiteId}}</b><br><hr style="height:2px; margin:0; margin-bottom:5px; padding:0; background-color: {{fillColor}}; border-color: {{fillColor}};"/>
          Concentration  =  <b><code style="border:0; background-color: white; color: black;"> {{avg}} ug/m3 </b></code> <br>
          <p>Coordinates: {{lat}},  {{long}}</p>'
        ), from = 'latin1', to = 'UTF-8')  }) })
      map <- Leaflet$new()
      map$tileLayer(provider = "Stamen.TonerLite", maxZoom=16)
      map$geoJson(toGeoJSON(dat_list, lat = 'lat', lon = 'long' ),
                  onEachFeature = '#! function(feature, layer){layer.bindPopup(feature.properties.popup)} !#',
                  #onEachFeature = '#! function(feature, layer){layer.setOpacity('0.5') } !#',
                  pointToLayer =  "#! function(feature, latlng){return L.circleMarker(latlng, {
                  radius: 11, fillColor: feature.properties.fillColor || 'grey',    
                  color: '#000', weight: 1, fillOpacity: 0.87, title: feature.properties.SiteId }) } !#")
      map$legend(position = 'topleft', colors = if(nums==1){
        cut(d2$avg*c(1.01,.9993), breaks = cuts2, right=F, labels = colorRampPalette(rev(labs[1:5]))(30), include.lowest=T) }
        else {labs[1:5]}, labels = as.vector(if(nums==1) c(signif(d2$avg*c(1.017,.989),2)) else c(rev(signif(cuts,2)))))
      map$setView(c(mean(range(d2$lat)), mean(range(d2$long))), zoom = max(6, 13+round(-5*(max(c(.35+(max(d2$long)-min(d2$long)),max(d2$lat)-min(d2$lat)))^(1/2.5)))))
    }
    else{          
      map <- Leaflet$new()
      map$setView(c(46.15, -94.6), zoom = 7)
      map$tileLayer(provider = "Stamen.TonerLite", zoom=8)
      map$enablePopover(TRUE)
    }
    suppressWarnings(return(map))
    
  })
  
  output$barplot <- renderChart({
    
    if(is.null(isolate(input$pollutant)) | is.null(dataset2()) ) {
      df <- data.frame(km_mean = rep(NA,12), Year= rep(NA,12))
      suppressWarnings(h1 <- hPlot(x="Year", y = "km_mean", type="column", data = df))
      h2$addParams(dom = 'barplot')
      h2$xAxis(title = list(text=""), labels = list(enabled=F), tickLength=0, lineWidth=0)
      h2$yAxis(title = list(text = ""))
      h2$legend(enabled=F)
      h2$chart(height=450, spacingLeft=5, marginBottom=45, marginRight=0, spacingRight=0)
      
    }
    else if(nrow(dataset2())>0){
      bar2 <- dataset2()[,c("year","MPCAID", "SiteId", input$time, "Boot_UCL")]
      names(bar2)[4] <- "Conc"
      bar2<-arrange(bar2,year)
      for(years in unique(bar2$year)) { for(site in unique(bar2$SiteId)) {if(nrow(filter(bar2,SiteId==site, year == years))<1) bar2<-rbind(bar2, c(years, filter(bar2,SiteId==site)$MPCAID[1], site, -1,-1)) }}
      bar2[bar2==-1] <- NA
      bar2$year  <- as.numeric(bar2$year)
      bar2$Conc  <- as.numeric(bar2$Conc)
      bar2$Boot_UCL  <- as.numeric(bar2$Boot_UCL)
      bar2$Conc <- signif(bar2$Conc, digits=3)
      bar2$Boot_UCL <- signif(bar2$Boot_UCL, digits=3)
      #h1 <- hPlot(x="SiteId", y = "Conc", type="column", data = bar2 %.% group_by(SiteId), group="year")
      bar2<-arrange(bar2,year, MPCAID)
      h2 <- Highcharts$new()
      if(input$time=="km_mean") h2$colors(c('#2f7ed8', '#8bbc21', '#EAC530', '#1aadce', '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a','#0d233a','grey'))
      else h2$colors(c('#7cb5ec', '#90ed7d', '#f7a35c', '#8085e9','#f15c80', '#e4d354', '#8085e8', '#8d4653', '#91e8e1','#434348'))
      h2$addParams(dom = 'barplot')
      h2$tooltip(followPointer =T, hideDelay = 0, animation=T, shared=F)
      h2$xAxis(categories = levels(factor(bar2$SiteId)), title = list(style=list(fontSize="13px", color="#333333"), margin=6, text="Monitor Site"), type="category", labels=list(rotation=-58, align='right'))
      if(risk.is()>0) h2$yAxis(plotLines=list(list(zIndex=5,value=risk.1(), shadow=T, color="darkred", width=2, dashStyle="dash", shadow=T,label=list(align="top",verticalAlign="top", text="Health Standard", style=list(fontWeight="bold", color="#333333")))), min=0, max=1.0001*max(c(bar2$Conc,bar2$Boot_UCL),na.rm=T), title = list(style=list(fontSize="13px", color="#333333"), spacingTop=5, text = "Concentration (ug/m3)"))
      else h2$yAxis(min=0, max=1.0001*max(c(bar2$Conc,bar2$Boot_UCL),na.rm=T), title = list(style=list(fontSize="13px", color="#333333"), spacingTop=5, text = "Concentration (ug/m3)"))
      h2$chart(zoomType='x', height=450, spacingLeft=6, spacingRight=4, spacingTop=0, spacingBottom=10, marginBottom=145, marginRight=5)
      h2$legend(margin=10, redraw=F, symbolWidth=25, y=45, symbolPadding=5, align="center", verticalAlign="top", floating=F, borderWidth=0, padding=10)
      bar2[bar2==-1] <- NULL
      for(years in unique(bar2$year)) {h2$series(data = filter(bar2, year == years)$Conc, name =years, type="column") 
                                       if(input$time=="km_mean") h2$series(type="errorbar", color="grey", data =  lapply(1:nrow(filter(bar2,  year == years)), function(x) as.vector(unlist(filter(bar2,  year == years)[x,c("Conc", "Boot_UCL")]))), name=paste(years, "95% UCL"), tooltip=list(pointFormat=paste(years, " 95%UCL: ", "<b>{point.high}</b>", sep="")  )  )  
                                       #else h2$series(type="errorbar", visible=F, color="grey", data = lapply(1:nrow(filter(bar2,  year == years)), function(x) as.vector(unlist(filter(bar2,  year == years)[x,c("Conc", "Boot_UCL")]))), name=paste(years, "95% UCL"), tooltip=list(pointFormat=paste(years, " 95%UCL: ", "<b>{point.high}</b>", sep="")  )  )  
      }
      h2$title(margin=35, style= list(fontWeight="bold", color="black"), text = titlez()) 
      h2$subtitle(y=38, x=-1, style= list(color="darkred", fontSize="13.5px", fontWeight="bold"), text= paste("- - ", risk(), sep=""))
      h2$plotOptions(animation = F, series=list(shadow=F, groupPadding=.13))
      h2$exporting(width=1800, sourceWidth=900, buttons=list(contextButton=list(symbolStrokeWidth=2,text="Print")  ))
    }     
    else {df <- data.frame(km_mean = rep(NA,12), Year= seq(from=2002, to=2013))
          suppressWarnings(h1 <- hPlot(x="Monitor Site", y = "Average Concentration", type="column", data = df))
          h2$addParams(dom = 'barplot')
          h2$xAxis(title = list(style=list(fontSize="13px"), text="Year"), type="category", categories = df$Year, min =2002, max=2013)
          h2$yAxis(min = 0, ceiling = 10, title = list(style=list(fontSize="13px"), text = paste(isolate(input$pollutant), "Concentration (ug/m3)")))
          h2$legend(enabled=F)
          h2$chart(height=450, spacingLeft=5, marginBottom=45, marginRight=0, spacingRight=0, plotBackgroundColor= "#E8E8E8")
          h2$title(style= list(color="red"), text = "No data available for this selection.")
          h2$subtitle(text = "Try selecting additional years.")        
    }
    
    suppressWarnings(return(h2))
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
      trend2<- arrange(trend2,year)
      for(years in unique(trend2$year)) { for(site in unique(trend2$SiteId)) {if(nrow(filter(trend2,SiteId==site, year == years))<1) trend2<-rbind(trend2, c(years, filter(trend2,SiteId==site)$MPCAID[1], site, -1,-1)) }}
      trend2[trend2==-1] <- NA
      trend2$year <- as.numeric(trend2$year)
      trend2$Conc <- as.numeric(trend2$Conc)
      trend2$Conc <- signif(trend2$Conc, digits=3)
      trend2<- arrange(trend2,year)
      h1 <- Highcharts$new()
      #h1 <- hPlot(x="year", y = "Conc", type="line", data = trend2, group="SiteId")
      x<-1
      for(site in unique(trend2$SiteId)) {h1$series(data = filter(trend2, SiteId==site)$Conc, name=site, type="spline", connectNulls=T, dashStyle=c("line","shortdot","dot")[x%%4+1])
       x<-x+1}                         
      h1$addParams(dom = 'trends')
      if(input$time=="km_mean") h1$colors(c('#2f7ed8', '#8bbc21', '#EAC530', '#1aadce', '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a','#0d233a','grey'))
      else h1$colors(c('#7cb5ec', '#90ed7d', '#f7a35c', '#8085e9','#f15c80', '#e4d354', '#8085e8', '#8d4653', '#91e8e1','#434348'))
      h1$tooltip(followPointer =T, hideDelay = 0, animation=T, shared=F)
      h1$xAxis(categories = unique(trend2$year), title = list(style=list(fontSize="13px", color="#333333"), margin=15, text="Year"), type="category")
      if(risk.is()>0) h1$yAxis(plotLines=list(list(zIndex=5, visible = ifelse(risk.is()>0,T,F), value=risk.1(), color="darkred", width=2, dashStyle="dash", label=list(align="top",verticalAlign="top", text="Health Standard", style=list(fontWeight="bold", color="#333333")))), min = 0.99*min(trend2$Conc, na.rm=T), max = 1.01*max(trend2$Conc,na.rm=T), ceiling = 100, title = list(style=list(fontSize="13px", color="#333333"), marginBottom=5, text = "Concentration (ug/m3)"))
      else h1$yAxis(min = 0.99*min(trend2$Conc, na.rm=T), max = 1.01*max(trend2$Conc, na.rm=T), ceiling = 100, title = list(style=list(fontSize="13px", color="#333333"), marginBottom=5, text = "Concentration (ug/m3)"))
      h1$chart(zoomType='x', height=450, spacingLeft=5, spacingRight=4, spacingTop=0, marginBottom=46, marginRight=4)
      h1$legend(margin=10, redraw=F, symbolWidth=20, symbolPadding=5, x=40, y=42, align="center", verticalAlign="top", floating=F, borderWidth=0, paddingBottom=5, width=890)
      h1$title(margin=35, style= list(fontWeight="bold", color="black"), text = titlez()) 
      h1$subtitle(y=38,x=-1, style= list(color="darkred", fontSize="13.5px", fontWeight="bold"), text= paste("- - ", risk(), sep=""))
      h1$plotOptions(series=list(shadow=T), pointStart= 2002, pointInterval=1)
      h1$exporting(width=1800, sourceWidth=900, buttons=list(contextButton=list(symbolStrokeWidth=2,text="Print")  ))
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
  }, options= list(bLengthChange=T, aLengthMenu = c(5, 10, 25, 50), iDisplayLength = 10, bFilter=T))
  
  
  output$monitorMap <- renderMap({
    d2 <- toxics[0,c("year","MPCAID", "Pollutant", "lat","long", "SiteId")]
    toxics <- dataset2()
    # Reduce the table to a single row for each site
    for(site in unique(toxics$SiteId)) d2 <-rbind(d2, filter(toxics, SiteId==site)[1,c("year","MPCAID", "Pollutant", "lat","long", "SiteId")])

    # Create the popup info 
    # Paste all years together that a site was active
    # Paste all pollutants sampled at a site together
    for(i in 1:nrow(d2)) {d2$year[i] <- paste(levels(factor(filter(toxics, SiteId ==d2[i,"SiteId"])$year)),collapse=", ")
                    d2$Pollutant[i] <- paste(levels(factor(filter(toxics, SiteId ==d2[i,"SiteId"])$Pollutant)), collapse=", ") }

    # Format the popup for each site in the list
    d2 <- mutate(d2, popup = paste0("<b>", SiteId,
                "</b><br><hr style='height:3px; width:446px; margin:0; margin-bottom:5px; padding:0; background-color: #004F98; border-color: #004F98;'/>",
                "<b style='color:#5db0ff;'>",
                "Years Active: ",
                "</b> <code style='display:block;  width:443px; border:0; white-space:pre-wrap; background-color:black; color:white;'>",
                year,
                "</code><br> <b style='color:#5db0ff;'>",
                "Pollutants: ",
                "</b><code style='display:block; width:443px; border:0; white-space:pre-wrap; background-color: black; color: white;'>",
                Pollutant, "</code> <p><b style='color:#5db0ff;'>",
                "Coordinates: </b>",
                lat, ", ", long, "</p>") )

# Convert the data frame into a list, saving each row as a new item in the list
dat_list <- lapply(1:nrow(d2), function(x) as.list(d2[x,]))

mMap <- Leaflet$new()

# Set the background map to black and white from "Stamen Maps"
mMap$tileLayer(provider = "Stamen.TonerLite", maxZoom=16)

#onEachFeature = '#! function(feature, layer){layer.bindLabel(feature.properties.label, {maxWidth: 0} ) .addTo(map)} !#',

# Assign a marker to each (lat,long) and attach popup text
mMap$geoJson(toGeoJSON(dat_list, lat = 'lat', lon = 'long' ),
             onEachFeature = "#! function(feature, layer){layer.bindPopup(feature.properties.popup, {minWidth: 450, minHeight:300})} !#",
             pointToLayer = "#! function(feature, latlng){return L.marker(latlng, {
             opacity: 0.75, weight: 1, title: feature.properties.SiteId }) } !#" )

# Control the zoom level with a silly formula
mMap$setView(c(mean(range(d2$lat)), mean(range(d2$long))), zoom = max(6,13+round(-5*(max(c(.35+(max(d2$long)-min(d2$long)),max(d2$lat)-min(d2$lat)))^(1/2.5)))))
mMapsuppressWarnings(return( mMap))
    
  })
  
  
  
})
