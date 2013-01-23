best <- function(state, problem) {
  if(!problem %in% c("heart attack", "pneumonia", "heart failure")) {
    stop("invalid outcome")  
  }
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(!state %in% unique(outcome$State)) {
    stop("invalid state")
  }
  
  if(problem == "heart attack") {
    sortColumn <- 11
  } else if(problem == "heart failure") {
    sortColumn <- 17
  } else {
    sortColumn <- 23
  }   
    
  outcome[, 11] <- as.numeric(outcome[, 11])
  outcome[, 17] <- as.numeric(outcome[, 17])
  outcome[, 23] <- as.numeric(outcome[, 23])
  
  filteredByStates <- outcome[outcome$State == state,]
  sorted <- filteredByStates[order(filteredByStates[,sortColumn], filteredByStates[,2], na.last=TRUE),]
  sorted$Hospital.Name[1]
}