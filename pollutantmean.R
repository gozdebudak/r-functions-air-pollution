pollutantmean <- function(directory, pollutant, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating 
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Setting working directory to given directory parameter
  setwd(directory)
  ## Creating a numeric vector for keeping the monitors pollutant values
  allmonitors <- numeric(0)
  
  ## for loop for calculating the mean of the each monitor 
  ## from id vector
  for(i in 1:length(id)){
    ## Creating file name by pasting together the directory, 
    ## id of monitor and ".csv"
    filename <- paste(formatC(id[i], width=3, flag="0"), ".csv", sep = "")
    ## Reading file and creating "pollutantdata" data frame
    pollutantdata <- read.csv(filename)
    ## Adding all values too 'allmonitors' vector
    allmonitors <- c(allmonitors, pollutantdata[,pollutant])
  }
  setwd("..")
  ## Calculating the mean of the all monitors
  mean(allmonitors, na.rm=TRUE)
}