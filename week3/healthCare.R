outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], 
     main="Heart Attack 30−day Death Rate",
     xlab = "30-day Death Rate")

# get all the titles
names(outcome)

# 1 row, 3 columns
par(mfrow = c(1, 3))

# 3 row, 1 column
#par(mfrow = c(3, 1))

# Heart Attacks 30 day death rate
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], 
     main=paste("Heart Attack", " (", sprintf("%.3f",mean(outcome[,11], na.rm=TRUE)), ")", sep=""),
     xlab = "30-day Death Rate",
     xlim=c(6.7,21.9))

abline(v=median(outcome[,11], na.rm=TRUE), col=4, lty=2)

# Heart Failure 30 day death rate
outcome[, 17] <- as.numeric(outcome[, 17])
hist(outcome[, 17], 
     main=paste("Heart Failure", " (", sprintf("%.3f",mean(outcome[,17], na.rm=TRUE)), ")", sep=""),
     xlab = "30-day Death Rate",
     xlim=c(6.7,21.9))

abline(v=median(outcome[,17], na.rm=TRUE), col=4, lty=2)

# Pneumonia 30 day death rate
outcome[, 23] <- as.numeric(outcome[, 23])
blah <- mean(outcome[, 23], na.rm=TRUE)
hist(outcome[, 23], 
     main=paste("Heart Failure", " (", sprintf("%.3f",mean(outcome[,17], na.rm=TRUE)), ")", sep=""),     
     xlab = "30-day Death Rate",
     xlim=c(6.7,21.9))

# Work out the ranges of values ignoring N/A values
range(outcome[,23], na.rm=TRUE)

# draw a line at the median
abline(v=median(outcome[,23], na.rm=TRUE), col=4, lty=2)

statesCount <- table(outcome$State)

# r filter table based on count
# http://stackoverflow.com/questions/6796569/how-to-filter-a-dataframe-based-on-category-counts
# find all the outcomes that belong in states with more than 20 entries
outcome2 <- outcome[outcome$State %in% names(table(outcome$State)[table(outcome$State) > 20]),]

death <- outcome2[, 11]
state <- outcome2$State

par(mfrow = c(1, 1))
par(las=2)
boxplot(death ~ state, 
        ylab = "30-day Death Rate",
        main = "Heart Attack 30-day Death Rate by State")
