######################################
# 
#   Bootstrap MLE UCL95
#
######################################

# Define boot_MLE75
boot_MLE75 <- function(group        = GroupID,     
                       results      = Results, 
                       censored     = Censored,
                       mdl          = MDL[1],
                       repeats      = 3000,
                       status       = Status[1],
                       low_threshold_for_mle = 0.75) {

# Suppress repeated warnings
options(warn = -1)    

# Print progress  
cat(paste0("\n", status, "%", "   ", group))
    
# Set censored as logical (True/False)
censored <- as.logical(censored)
    
# Define function to record MLE mean of re-sampled table
getMLE <- function(n = length(censored)){
  
    random.rows <- sample(1:n, replace=T)
    
    while ((sum(censored[random.rows]) > n-2) | (length(unique(results[random.rows])) < 3) ) {
      random.rows <- sample(1:n, replace=T) 
      }
        
    if (sum(censored[random.rows]) < 1) return(mean(results[random.rows]))
    
    enormCensored(results[random.rows], 
                  censored[random.rows], 
                  method = "impute.w.mle", 
                  ci = F, 
                  lb.impute = mdl * low_threshold_for_mle)$parameters[[1]]
}

# Use lapply to repeat EnvStats MLE mean 
bootedMeans <- lapply(1:repeats, FUN=function(x) getMLE())
    
# Summarize the booted MLE means: UCL (options include Mean and Std. Dev)
#boot_UCL <- quantile(unlist(bootedMeans), probs=percentile, na.rm=T)
boot_UCL <- sort(unlist(bootedMeans))[repeats*.95 + 1]

options(warn=1)
    
return(boot_UCL[[1]])

}

#-----#
