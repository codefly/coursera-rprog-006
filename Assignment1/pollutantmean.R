
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  if(!file.exists(directory))
    stop(paste("Directory ", directory , " does not exist!"));
  
  directory <- gsub("([^/])$", "\\1/", directory)
  vec <- numeric(0)
  
  for (i in id){
    file <- paste(directory, sprintf("%03d", i), ".csv", sep="")
    if(file.exists(file)){
      vec <- c(vec, file)
    }
  }
  
 allframes = lapply(vec, read.csv, header=TRUE )
 merged = do.call(rbind, allframes);
 mean(merged[,pollutant], na.rm=TRUE)
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
}


