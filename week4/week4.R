# all the available colours
colors()

pal <- colorRamp(c("red", "blue"))
pal(0)
pal(1)
pal(0.5)

pal(seq(0, 1, len = 10))

pal <- colorRampPalette(c("red", "yellow"))
pal(2)
pal(10)

library(RColorBrewer)
cols <- brewer.pal(3, "BuGn")
cols

pal <- colorRampPalette(cols)
image(volcano, col=pal(20))

x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x,y)

# Regular expressions
homicides <- readLines("homicides.txt")

length(grep("iconHomicideShooting", homicides))
length(grep("iconHomicideShooting|icon_homicide_shooting", homicides))
length(grep("Cause: [Ss]hooting", homicides))

i <- grep("[cC]ause: [Ss]hooting", homicides)
j <- grep("[Ss]hooting", homicides)

str(i)
str(j)

setdiff(i, j)
setdiff(j, i)

grep("^New", state.name)
grep("^New", state.name, value=TRUE)
grepl("^New", state.name)

regexpr("<dd>[Ff]ound(.*)</dd>", homicides[1:10])
substr(homicides[1], 177, 177 + 93 -1)

r <- regexpr("<dd>[Ff]ound(.*?)</dd>", homicides[1:5])
x <- substr(homicides[1], 177, 177 + 33 -1)
m <- regmatches(homicides[1:5], r)

# Get rid of the dd tags
sub("<dd>[Ff]ound on |</dd>", "", x)
gsub("<dd>[Ff]ound on |</dd>", "", x)

d <- gsub("<dd>[Ff]ound on |</dd>", "", m)
as.Date(d, "%B %d, %Y")

# allows us to capture a sub-expression
regexec("<dd>[Ff]ound on (.*?)</dd>", homicides[1])

substr(homicides[1], 190, 190 + 15 - 1)

r <- regexec("<dd>[Ff]ound on (.*?)</dd>", homicides[1:2])

m <- regmatches(homicides[1:2], r)

sapply(m, function(x) x[2])

r <- regexec("<dd>[Ff]ound on (.*?)</dd>", homicides)
m <- regmatches(homicides, r)
dates <- sapply(m, function(x) x[2])
dates <- as.Date(dates, "%B %d, %Y")
hist(dates, "month", freq=TRUE)