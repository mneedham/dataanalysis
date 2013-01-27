count <- function(cause = NULL) {
  allowedCauses = c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
    
  if(is.null(cause) || !cause %in% allowedCauses) {
    stop("invalid cause")
  }
  
  homicides <- readLines("homicides.txt")
  
  r <- regexec("<dd>[Cc]ause: (.*?)</dd>", homicides)
  m <- regmatches(homicides, r)
  actualCauses <- sapply(m, function(x) x[2])  
    
  matchingCauses <- actualCauses[tolower(actualCauses) == cause]
  matchingCauses <- matchingCauses[!is.na(matchingCauses)]
  length(matchingCauses)
    
  ## Extract causes of death
  ## Return integer containing count of homicides for that cause
}