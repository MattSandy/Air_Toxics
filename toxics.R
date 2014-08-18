# Air Toxics pre-processing for web app

# Remove unreliable methods
toxics <- filter(toxics, !Pollutant %in% c("Mercury","Copper","Acetone","2-Proponol","Aluminum"))
# Remove lead, shown in Criteria app
toxics <- filter(toxics, !Pollutant %in% c("Lead"))

# Create unique Site ID's
toxics$SiteId <- paste0(toxics$City, "(", toxics$MPCAID, ")")
toxics[toxics$SiteId== "Minneapolis-St Paul International Airport(964)", ]$SiteId <-  "MSP Intl. Airport(964)"
