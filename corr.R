corr <- function(directory, threshold = 0) {
  readFile <- function(file) {
    read.csv(paste(directory, "/", file, sep=""))
  }
  
  totallyNA <- function(contents) {
    contents[!is.na(contents$sulfate) & !is.na(contents$nitrate),]
  } 
  
  correlation <- function(naContents) {
    cor(x=naContents$sulfate, y =naContents$nitrate)
  }
    
  #sapply(list.files(directory), function(x) nrow(totallyNA(readFile("specdata", x))))
  allTheFiles <- list.files(directory)
  relevantFiles <- Filter(function(file) nrow(totallyNA(readFile(file))) > threshold, allTheFiles)
  
  if(length(relevantFiles) == 0)
    0
  else
    sapply(relevantFiles, function(file) correlation(totallyNA(readFile(file))))
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
}

# Files with >150 completely observed observations
#  length(Filter(function(file) nrow(totallyNA(readFile("specdata", file))) > 150, allTheFiles))

