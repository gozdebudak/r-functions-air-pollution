corr <- function(directory, threshold = 0){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the 
  ## number of completely observed observations (on all 
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Getting the complete row counts from complete function and
  ## converting to a data frame
  completeones <- as.data.frame(complete(directory))
  
  ## Checking data with threshold value and getting the monitor ids
  abovethreshold <-   completeones$completes > threshold
  aboveids <- completeones[abovethreshold,]$id

  ## Creating a numeric vector for keeping correlation values of monitors
  corrs <- numeric(length(aboveids))
  
  ## If there is no monitor above the threshold, function returns 
  ## a numeric vector of length 0.
  if(length(aboveids) == 0){
    numeric(0)
  } else{
    ## Setting working directory to 'directory' variable for reading files
    setwd(directory)
    
    for(i in 1:length(aboveids)){
      ## Creating file name by pasting together the id of monitor and ".csv"
      filename <- paste(formatC(aboveids[i], width=3, flag="0"), ".csv", sep = "")
      ## Reading file and creating "pollutantdata" data frame
      pollutantdata <- read.csv(file = filename)
      ## The data with only non NA values
      completedata <- pollutantdata[complete.cases(pollutantdata),]
      ## The correlation value of monitor is assigned to numeric vector
      corrs[i] <- cor(completedata$sulfate, completedata$nitrate, method = "pearson")
    }
    ## Setting working directory back
    setwd("..")
    ## Returning the vector of correlations
    corrs
  }
}