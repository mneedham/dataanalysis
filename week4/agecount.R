agecount <- function(age = NULL) {
  if(is.null(cause)) {
    stop("invalid age")
  }  
  
  homicides <- readLines("homicides.txt")
  
  r <- regexec("<dd>[^,]+. (.*?) years old</dd>", homicides)
  m <- regmatches(homicides, r)
  actualAges <- sapply(m, function(x) x[2])
  actualAges <- actualAges[!is.na(actualAges)]
  actualAges <- as.integer(actualAges)
  
  numberOfDeaths <- actualAges[actualAges == age]
  length(numberOfDeaths)
}
