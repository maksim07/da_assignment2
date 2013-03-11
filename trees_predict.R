# prediction by using trees

library(tree)
library(BMA)

# load data
load("/tmp/samsungData.rda")

source("utils.R")

# training with leave one out cross validation
trainData <- cleanDataStructure(getSubjects(samsungData, c(1, 3, 5, 6)))  
testData <- cleanDataStructure(getSubjects(samsungData, c(27, 28, 29, 30)))

trainResiduals <- data.frame(prediction = c(), actual = c())
for (i in 1:nrow(trainData))
{
  localTrainData <- trainData[c(-i),]
  localTestData <- trainData[c(i),]
  
  localTree <- tree(activity ~ ., data = localTrainData)
  pred <- predict(localTree, localTestData, type="class")
  trainResiduals[i, "prediction"] <- pred
  trainResiduals[i, "actual"] <- localTestData[1, "activity"]
  print(paste0(i, " ", trainResiduals[i, "prediction"], " ", trainResiduals[i, "actual"],
               " ", trainResiduals[i, "prediction"] == trainResiduals[i, "actual"]))
}

# calculating errors during cross validation
trainErrors <- trainResiduals[trainResiduals$prediction != trainResiduals$actual, ]
trainErrorsTable <- table(trainErrors)

sum(trainResiduals$prediction != trainResiduals$actual) / length(trainResiduals$actual)


# testing
t <- tree(activity ~ ., data=trainData)
plot(t)
text(t, ps = 0.4, cex=0.7)
pr <- predict(t, testData, type="class")
pr <- data.frame(pr = pr, ac = testData$activity)

sum(pr$pr != pr$ac)
print(paste0("Error rate is ", sum(pr$pr != pr$ac) / nrow(pr)))

table(pr[pr$pr != pr$ac,]$pr, pr[pr$pr != pr$ac,]$ac)

