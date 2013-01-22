x <- rnorm(5)
summary(x)
# 10 variables, mean 20, standard deviation 10
x <- rnorm(10, 20, 10)

set.seed(1)
rnorm(5)
rnorm(5)

# if we reset the seed we'll get the same numbers as we did above
set.seed(1)
rnorm(5)
rnorm(5)

set.seed(20)
x <- rnorm(100)
e <- rnorm(100,0,2)
y <- 0.5 + 2 * x + e
summary(y)
plot(x,y)

# binary random variable instead of normal
set.seed(10)
x <- rbinom(100,1,0.5)
e <- rnorm(100,0,2)
y <- 0.5 + 2 * x + e
summary(y)
plot(x,y)

# poisson 
set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 + 0.3 + x
y <- rpois(100, exp(log.mu))
summary(y)
plot(x,y)

# sample - arbritrary distributions
set.seed(1)
# sampling without replacement
sample(1:10,4)
sample(1:10,4)

sample(letters, 5)
sample(1:10) # permutation

sample(1:10, replace = TRUE)

#plotting
# base graphics you build up piece by piece
# lattice is in one function call


x <- rnorm(100)
y <- x + rnorm(100,sd=0.5)
f <- gl(2,50, labels=c("group 1", "group 2"))
# plot with regression line
xyplot(y ~ x | f,
       panel= function(x, y, ...) {
         panel.xyplot(x,y,...)
         panel.abline(x,y, col=2)
         })

data(environmental)
xyplot(ozone ~ radiation, data ~ environmental)

plot(0,0, main=expression(theta == 0),
     ylab = expression(hat(gamma) == 0),
     xlab = expression(sum(x[i] * y[i], i==1,n)))
