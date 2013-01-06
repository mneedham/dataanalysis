data <- read.table("foo.txt")

initial <- read.table("datatable.txt", nrows=1000)
classes <- sapply(initial, class)
tabAll <- read.table("datatable.txt", colClasses = classes)

y <- data.frame(a=1, b="a")
dput(y)

dput(y, file="y.R")
new.y <- dget("y.R")
dump(c("x", "y"), file="data.R")

myfunction <- function(x) {
  y <- rnorm(100)
  mean(y)
}

str(str)

