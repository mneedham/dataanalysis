rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(!state %in% unique(data$State)) {
    stop("invalid state")
  }
  
  if(!outcome %in% c("heart attack", "pneumonia", "heart failure")) {
    stop("invalid outcome")  
  }
  
  if(outcome == "heart attack") {
    sortColumn <- 11
  } else if(outcome == "heart failure") {
    sortColumn <- 17
  } else {
    sortColumn <- 23
  }     
  
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  filteredByStates <- data[data$State == state,]
  sorted <- filteredByStates[order(filteredByStates[,sortColumn], filteredByStates[,2], na.last=TRUE),]
  
  if(is.numeric(num)) {
    position <- num
  } else if(num == "best") {
    position <- 0
  } else {
    position <- nrow(sorted[!is.na(sorted[sortColumn]),])
  }
  
  sorted$Hospital.Name[position]
}