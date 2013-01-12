make.NegLogLik <- function(data, fixed=c(FALSE, FALSE)) {
  params <- fixed
  function(p) {
    params[!fixed] <- p
    mu <- params[1]
    sigma <- params[2]
    a <- -0.5*length(data)*log(2*pi*sigma^2)
    b <- -0.5*sum((data-mu)^2) / (sigma^2)
    -(a + b)
  }
}

normals <- rnorm(100,1,2)
nLL <- make.NegLogLik(normals, c(1, FALSE))
x <- seq(1.7, 1.9, len=100)
y <- sapply(x, nLL)
plot(x, exp(-(y - min(y))), type ="l")

x <- list(a=1.5, b=rnorm(10))
lapply(x, mean)

x <- 1:4
lapply(x, runif)

x <- list(a = matrix(1:4,2,2), b=matrix(1:6,3,2))
lapply(x, function(elt) elt[,1])

x <- list(a=1:4, b = rnorm(10), c=rnorm(20,1), d=rnorm(100,5))
lapply(x, mean)
sapply(x,mean) # gives us result as one list

str(apply)

x <- matrix(rnorm(200), 20, 10)
apply(x, 2, mean)
apply(x, 1, sum)

rowSums <- apply(x,1,sum)
rowMeans <- apply(x,1,mean)
colSums <- apply(x,2,sum)
colMeans <- apply(x,2,mean)

x <- matrix(rnorm(200), 20, 10)
apply(x, 1, quantile, probs = c(0.25,0.75))

a <- array(rnorm(2 * 2 * 10), c(2, 2, 10))
apply(a, c(1,2), mean)

x <- c(rnorm(10), runif(10), rnorm(10,1))
f <- gl(3,10)
split(x,f)
help(gl)

library(datasets)
head(airquality)

lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))

x <- rnorm(10)
f1 <- gl(2,5)
f2 <- gl(5,2)
f1
f2
interaction(f1,f2)
str(split(x, list(f1,f2)), drop=TRUE)

str(mapply) # pass different lists to it

# these are equivalent
list(rep(1,4), rep(2,3), rep(3,2), rep(4,1))
mapply(rep, 1:4, 4:1)

noise <- function(n, mean, sd) {
  rnorm(n, mean, sd)
}

noise(5, 1, 2)
noise(1:5, 1:5, 2)

mapply(noise, 1:5, 1:5, 2)
list(noise(1,1,2), noise(2,2,2), noise(3,3,2), noise(4,4,2), noise(5,5,2))

log(-1) # warning
printmessage <- function(x) {
  if(is.na(x))
    print("na")
  else if(x > 0) 
    print("greater 0")
  else
    print("less than 0")
  invisible(x)
}

#debug functions
mean(x2)
traceback()

lm(y2 ~ x2)
debug(lm)
lm(y2 ~ x2)

options(error = recover)

pow <- function(x = 4, n = 3) {
  x^n
}

x <- 1:10
if(x > 5) {
  x <- 0
}`
read.csv("nofile")

library(datasets)
data(iris)

mean(subset(iris, Species == "virginica", select=Sepal.Length))

# get the first 4 columns
iris[, 1:4]

# 2 means get the means by column
apply(iris[, 1:4], 2, mean)

library(datasets)
data(mtcars)

# split the data set up into list by their cylinder value
split(mtcars, mtcars$cyl)

# get the mpg mean by cylinders in the car
tapply(mtcars$mpg, mtcars$cyl, mean)

horseyp <- tapply(mtcars$hp, mtcars$cyl, mean)