initial <- read.csv("train.csv", header = TRUE)
initial <- read.csv("train.csv", nrows=10000, header = TRUE)
classes <- sapply(initial, class)

# get first row
initial[1:1,]

# take a sample of 5 rows of the input 
sampleSet <- initial[sample(1:nrow(initial), 10000), ]

# get all the labels
sampleSet.labels <- as.factor(sampleSet$label)

# show how many times each label appears
table(sampleSet.labels)

# various statistics about the data set
summary(sampleSet)

# stem cell showing the spread of the values for pixlel 189
stem(sampleSet$pixel190)

# correlation between pixel189 and the label
cor(pixel189, labels)

# get the variance for every feature
apply(subset(sampleSet, select = -label), 2, var)

# get data set excluding label
# http://stackoverflow.com/questions/6286313/remove-an-entire-column-from-a-data-frame-in-r
subset(sampleSet, select = -label)

# show all the features which don't have any variance - all have the same value
excludingLabel <- subset( sampleSet, select = -label)
variances <- apply(excludingLabel, 2, var)
names(excludingLabel[variances == 0][1,])

# get the names of the labels which have no variance
pointlessFeatures <- names(excludingLabel[variances == 0][1,])
write(file="pointless-features.txt", pointlessFeatures)

# count how many labels have no variance
length(names(excludingLabel[apply(excludingLabel, 2, var) == 0][1,]))

tiny <- sampleSet[1:5, 200:210]
apply(tiny, 1, mean)

countZeros <- function(entries) {
  length(Filter(function (x) x == 0, entries))
}

apply(tiny, 1, countZeros)
apply(tiny, 1, function(entries) length(Filter(function (x) x != 0, entries)))

tiny$nonZeros <- apply(tiny, 1, function(entries) length(Filter(function (x) x != 0, entries)))

# put the non zeros into the data frame
initial$nonZeros <- apply(initial, 1, function(entries) length(Filter(function (x) x != 0, entries)))
initial$fullHouses <- apply(initial, 1, function(entries) length(Filter(function (x) x == 255, entries)))
initial$meanPixels <- apply(initial, 1, mean)

# print out the labels and non zero counts
subset(initial, select=c(label, nonZeros))

# scatter plot showing labels vs non zero value
smoothScatter(initial$label, initial$nonZeros)

initial.pca <- prcomp(~ ., data = initial, cor = TRUE)

newFeatures <- subset(initial[1:10,], select=c(label, nonZeros, meanPixels, fullHouses))
newFeatures <- subset(initial, select=c(label, nonZeros, meanPixels, fullHouses))
write.table(file="feature-extraction.txt", newFeatures, row.names=FALSE, sep=",")

write.table(file="train-plus-features.csv", initial, row.names=FALSE, sep=",")