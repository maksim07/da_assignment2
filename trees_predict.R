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
trainErrorsTable
trainErrorsTable / sum(trainErrorsTable) >= 0


# testing
t <- tree(activity ~ ., data=trainData)
plot(t)
text(t)
pr <- predict(t, testData, type="class")
pr <- data.frame(pr = pr, ac = testData$activity)

print(paste0("Error rate is ", sum(pr$pr != pr$ac) / nrow(pr)))



