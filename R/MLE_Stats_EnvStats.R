###--------------------------------------------------------------------------###
#     Censored Data Analysis
#
#     Purpose: Using the EnvStats and dplyr package, the function below 
#               completes a summary data analysis on data containing censored 
#               or (non-detect)measurements
#
#     Output: The summary includes the MLE mean, the
#               estimated hourly maximum, and the 95% Bootstrapped UCL. 
#
###--------------------------------------------------------------------------###

#==============================================================================#
#        
#    Create envStats.summary() function 
# 
#    Report MLE-mean, estimated 1-hour max, and bootstrapped 95% UCL
#==============================================================================#

envStats.summary <- function( data                  = data,
                              SITEID                = "AQS_ID",
                              Results               = "Concentration",
                              MDL                   = "MDL",
                              Year                  = "Year",
                              Date                  = "Date",
                              Pollutant             = "Pollutant",
                              Param_Code            = "Param_Code",    
                              POC                   = "POC",
                              Percent_Cutoff        = 10,
                              Minimum_Samples       = 45,
                              Minimum_Detects       = 6,
                              Minimum_Unique_Values = 3,
                              Low_Threshold_for_MLE = 0.75,
                              Boot_Repeats          = 3000,
                              seed                  = 27,
                              Hour_Max_Multiplier   = 10) {
  
  # Load packages
  library(EnvStats)
  library(dplyr)
  library(stringr)
  
  #==============================================================================#
  #    Filter data to relevant columns                                           #
  #==============================================================================#
  data <- data[ , c(SITEID, POC, Pollutant, "CAS", Param_Code, Results, MDL, Year, Date)]
  
  # Rename columns
  names(data) <- c("SITEID", "POC", "Pollutant", "CAS", "Param_Code",
                   "Concentration", "MDL", "Year", "Date")
  
  #Remove white-space
  data$Pollutant <- str_trim(data$Pollutant)
  
  #Remove NAs
  data <- data[!is.na(data$Concentration), ]
  
  
  #==============================================================================#
  #   Find average site concentrations when 
  #   multiple POCs or Param_Codes are present               
  #==============================================================================#
  data <- group_by(data, SITEID, Pollutant, CAS, MDL, Year, Date) %>%
          summarize(Param_Code    = Param_Code[1], 
                    Concentration = mean(Concentration, na.rm=T))
  
  #==============================================================================#
  #   Create groups based on SITEID, Pollutant and Year                        #
  #       ex:  270531007_Carbon Sulfide_2010                                    #
  #==============================================================================#
  data$GroupID <- paste(data$SITEID, data$Pollutant, data$Year, sep="_") 
  
  #==============================================================================#
  #   Calculate total detections and percent detect                             #
  #==============================================================================#
  
  # Create Censored and Detected column
  data$Censored <- data$Concentration <  data$MDL
  data$Detected <- data$Concentration >= data$MDL
  
  # Issue warning if values are == to MDL
  if(nrow(filter(data, Concentration == MDL)) > 0) {
    warning(paste(nrow(filter(data, Concentration == MDL)), 
                  "observations found equal to the MDL."))
  }
  
  # Censored summaries
  data <- group_by(data, GroupID) %>% 
          mutate(Count_Sampled  = n(), 
                 Count_Censored = sum(Censored),
                 Count_Detect   = Count_Sampled - Count_Censored,
                 Pct_Detect     = round(100 * Count_Detect/Count_Sampled))
  
  #==============================================================================#
  #   Estimate acute 1-hour max value 
  #   Default methods:
  #     * Multiply 2nd highest daily-mean by 10
  #     * Multiply 1st highest daily-mean by 10
  #==============================================================================#
  
  # Remove Censored values for 2nd-max
  data$Report_Value <- ifelse(data$Censored == 1, NA, data$Concentration)
  
  # For each group multiply 2nd daily max by 10 to estimate hourly maximum
  data <- group_by(data, GroupID) %>% 
          mutate(Day_Max     = sort(Report_Value, decreasing=T)[1], 
                 Day_2ndHigh = sort(Report_Value, decreasing=T)[2], 
                 Hour_Max_byMaxDay  = Day_Max * Hour_Max_Multiplier, 
                 Hour_Max_by2ndHigh = Day_2ndHigh * Hour_Max_Multiplier)
  
  data$Report_Value <- NULL
  
  #==============================================================================#
  #   For annual summaries produce NA for records with:                                            
  #          * % detected        < Percent_Cutoff                                
  #          * count of detects  < Minimum_Detects
  #          * number of samples < Minimum_Samples
  #          * Less than minimum required unique values
  #==============================================================================#
  
  # Save raw values
  data$Raw_Values <- data$Concentration
  
  # Replace non-detected machine values with detection limit
  data[data$Censored, ]$Concentration <- data[data$Censored, ]$MDL
  
  # Count unique detected values, and number of identical values
  data <- group_by(data, GroupID) %>% mutate(uniQ = length(unique(Concentration[!Censored])))
  
  data <- group_by(data, GroupID) %>% mutate(sameValues = sum(duplicated(Concentration[!Censored])))
  
  # Create filter based on data quality checks
  ND_filter <- (data$Count_Sampled < Minimum_Samples) | 
               (data$Count_Detect  < Minimum_Detects) | 
               (data$Pct_Detect    < Percent_Cutoff)  | 
               (data$uniQ          < Minimum_Unique_Values)
  
  #==============================================================================#
  #   Calculate annual averages:
  #       Raw_Value_Mean = using all machine values as reported
  #       SubDL_Mean     = replace all censored values with MDL
  #       Mean           =  MLE mean using EnvStats package                                                           
  #==============================================================================#
  
  # Kaplan-Meier option
  #data_annual <- group_by(data_annual, GroupID) %>% 
  #    mutate(KM_Mean= if (Count_Censored[1] < 1) mean(Concentration)
  #        else enparCensored(Concentration, as.logical(Censored))$parameters[[1]])
  
  # Calculate alternative mean methods
  data <- group_by(data, GroupID) %>% 
          mutate(Raw_Value_Mean = mean(Raw_Values), 
                 SubDL_Mean = mean(Concentration))
  
  # Set random seed for bootstrap sampling
  set.seed(seed)
  
  # Run boot function to get UCL95 for alternative mean methods
  data <- group_by(data, GroupID) %>% 
          mutate(Raw_UCL95 = sort(replicate(Boot_Repeats, mean(sample(Raw_Values, replace=T))))[Boot_Repeats*.95+1],
                 SubDL_UCL95 = sort(replicate(Boot_Repeats, mean(sample(Concentration, replace=T))))[Boot_Repeats*.95+1])
  
  # Filter data that don't meet annual summary reporting requirements
  data_annual <- data[!ND_filter, ]
  
  # Calculate reported MLE mean
  data_annual <- group_by(data_annual, GroupID) %>% 
                 mutate(Mean = if(Count_Censored[1] < 1) mean(Concentration)
                               else enormCensored(Concentration, as.logical(Censored), method="impute.w.mle", ci=F, lb.impute=MDL[1] * Low_Threshold_for_MLE)$parameters[[1]])
  
  #==============================================================================#
  #   Bootstrap MLE 95% UCL                                                      #
  #==============================================================================#
  
  # Load Bootstrapping MLE function
  source("bootEnvStats.R")
  
  # Get group details for bootstrap progress updates
  N_of_Groups <- length(unique(data_annual$GroupID))
  
  data_annual <- group_by(data_annual, GroupID) %>%
                 mutate(Group_Num = grep(GroupID[1], unique(data_annual$GroupID)),
                        Status    = round(100 * Group_Num[1]/N_of_Groups, 1))
  
  # Run boot function for each group
  data_annual <- group_by(data_annual, GroupID) %>% 
                 mutate(UCL95 = ifelse(Count_Censored[1] < 1, 
                                       sort(replicate(Boot_Repeats, mean(sample(Concentration, replace=T))))[Boot_Repeats*.95+1], 
                                       boot_MLE75(GroupID[1], 
                                                  Concentration, 
                                                  Censored, 
                                                  MDL[1], 
                                                  Boot_Repeats,
                                                  Status[1],
                                                  Low_Threshold_for_MLE)))
  
  
  # Option to use EnvStats built in Conf. Interval function
  # data2 <- group_by(data2, GroupID) %>% mutate(UCL95=if (Count_Censored[1] < 1) sort(replicate(Boot_Repeats, mean(sample(Concentration, replace=T))))[Boot_Repeats*.95+1] else enormCensored(Concentration, Censored, method="impute.w.mle", ci=T, lb.impute=MDL[1]*.75, ci.method="bootstrap", n.bootstraps=Boot_Repeats, seed=27, conf.level=.90)$interval$limits[[2]])
  
  #==============================================================================#
  #   Remove daily results and join tables                                       #                   
  #==============================================================================#   
  data <- data[!duplicated(data$GroupID), ]
  
  data_annual <- data_annual[!duplicated(data_annual$GroupID), ]
  
  data <- left_join(data, data_annual[ , c("GroupID", "Mean", "UCL95")])
  data$Concentration    <- NULL
  data$Raw_Values       <- NULL
  data$Censored         <- NULL
  data$Detected         <- NULL
  #data$Date <- NULL
  
  options(warn = 1)
  data$Mean_Method  <- paste0("Impute MLE with normal approximation; Lowerbound = ",
                               100 * Low_Threshold_for_MLE, "% of MDL.")
  data$UCL95_Method <- "95th percentile of the bootstrapped MLE means."
  
  row.names(data) <- NULL
  
  return(data)
} 


#======# 
