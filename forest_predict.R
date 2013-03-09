# prediction by using random forrest
library(randomForest)

source("utils.R")

# load data
load("/tmp/samsungData.rda")
# training and testing data
trainData <- cleanDataStructure(getSubjects(samsungData, c(1, 3, 5, 6)))  
testData <- cleanDataStructure(getSubjects(samsungData, c(27, 28, 29, 30)))


# preparing random forest
set.seed(717171)
forestTrain <- randomForest(activity ~ ., data=trainData, prox=TRUE, ntree=500)
forestTrain

# cross validation
predictErrorRates <- c()
for (i in c(1:20))
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

hist(predictErrorRates, 7, col="lightgray")

# error rate on train data
predictTrain<-predict(forestTrain, newdata=trainData)
sum(predictTrain != trainData$activity) / length(trainData$activity)


# predicting test data
predictTrain <- predict(forestTrain, testData)
sum(predictTrain != testData$activity) / length(testData$activity)
sum(predictTrain != testData$activity)

testData$predicted <- predictTrain

table(testData$activity, testData$predicted)



