complete <- function(directory, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Setting working directory to given directory parameter
  setwd(directory)
  
  ## The object that contains monitor's complete row count
  completes <- integer(length = length(id))
  for(i in 1:length(id)){
    ## Creating file name by pasting together the id of monitor and ".csv"
    filename <- paste(formatC(id[i], width=3, flag="0"), ".csv", sep = "")
    ## Reading file and creating "pollutantdata" data frame
    pollutantdata <- read.csv(file = filename)
    
    ## Count of the complete rows to an integer vector
    completes[i] <- nrow(pollutantdata[complete.cases(pollutantdata),])
  }
  setwd("..")
  ## Creating the data frame for monitors and complete rows count
  cbind(id, completes)
}