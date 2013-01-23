rankall <- function(outcome, num = "best") {
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
    
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  states <- unique(data$State)
  states <- states[order(states)]
  
  findHospital <- function(state) {
    filteredByStates <- data[data$State == state,]
    sorted <- filteredByStates[order(filteredByStates[,sortColumn], filteredByStates[,2], na.last=TRUE),]
    
    if(is.numeric(num)) {
      position <- num
    } else if(num == "best") {
      position <- 1
    } else {
      position <- nrow(sorted[!is.na(sorted[sortColumn]),])
    }      
    
    sorted$Hospital.Name[position]
  }
  
  hospitals <- apply(as.array(states), 1, function(x) findHospital(x))
  print(hospitals)
  data.frame(hospital = hospitals, state = states)
}