complete <- function(directory, id = 1:332) {  
  readFile <- function(id) {
    read.csv(paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv", sep=""))  
  }
  
  nobs <- apply(as.array(id),1, function(x) length(Filter(function(x) !is.na(x), readFile(x)$sulfate)))
  data.frame(id=id, nobs=nobs)
}

complete <- function(directory, id = 1:332) {  
  readFile <- function(id) {
    read.csv(paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv", sep=""))  
  }
  
  totallyNA <- function(id) {
    contents <- readFile(id)
    contents[!is.na(contents$sulfate) & !is.na(contents$nitrate),]
  }
  
  nobs <- apply(as.array(id),1, function(x) nrow(totallyNA(x)))
  data.frame(id=id, nobs=nobs)
}

# blah[!is.na(blah$sulfate) & !is.na(blah$nitrate),]