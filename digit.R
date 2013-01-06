initial <- read.csv("train.csv", nrows=10000, header = TRUE)
classes <- sapply(initial, class)

# get first row
initial[1:1,]

# take a sample of 5 rows of the input 
sampleSet <- initial[sample(1:nrow(initial), 10000), ]

# get all the labels
sampleSet.labels <- as.factor(sampleSet$label)

# show how many times each label appears
table(as.factor(sampleSet$label))

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