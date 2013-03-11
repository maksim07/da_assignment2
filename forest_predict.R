# prediction by using random forrest
library(randomForest)

source("utils.R")

# load data
load("/tmp/samsungData.rda")
# training and testing data
trainData <- cleanDataStructure(getSubjects(samsungData, c(1, 3, 5, 6)))  
testData <- cleanDataStructure(getSubjects(samsungData, c(27, 28, 29, 30)))
validationData <- cleanDataStructure(getSubjects(samsungData, c(2, 7, 8, 9:26)))



# preparing random forest
set.seed(717171)
forestTrain <- randomForest(activity ~ ., data=trainData, prox=TRUE, ntree=500)
forestTrain

# cross validation
predictErrorRates <- c()
for (i in c(1:50))
{
  set.seed(1000 + i)
  samp <- sample(c(1:nrow(trainData)), size=200)
  localTrain <- trainData[-samp,]
  localTest <- trainData[samp,]
  localForest <- randomForest(activity ~ ., data=localTrain, prox=TRUE, ntree=500)
  localPredict <- predict(localForest, localTest)
  predictErrorRates[[i]] <- sum(localPredict != localTest$activity) / length(localTest$activity)
  print(paste0("Error rate for sample ", i, " is ", predictErrorRates[i]))
}

mean(predictErrorRates)

hist(predictErrorRates, 7, col="lightgray", main="Cros validation error rates", xlab="Error rate")

# error rate on train data
predictTrain<-predict(forestTrain, newdata=trainData)
sum(predictTrain != trainData$activity) / length(trainData$activity)


# predicting test data
predictTest <- predict(forestTrain, testData)
sum(predictTest != testData$activity) / length(testData$activity)
sum(predictTest != testData$activity)

testData$predicted <- predictTest

testDataErrors <- testData$activity != testData$predicted
table(testData[testDataErrors,]$activity, testData[testDataErrors,]$predicted)


# validating data
predictValid <- predict(forestTrain, validationData)
sum(predictValid != validationData$activity) / length(validationData$activity)
sum(predictValid != validationData$activity)

