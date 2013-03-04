library(tree)
library(BMA)


load("/tmp/samsungData.rda")

getSubjects <- function(data, subs)
{
  data[data$subject %in% subs, c(-562)]
}

cleanDataStructure <- function(raw)
{
  colnames(raw) <- gsub("\\(\\)", "_", colnames(raw))
  colnames(raw) <- gsub("\\(", "_", colnames(raw))
  colnames(raw) <- gsub("\\)", "_", colnames(raw))
  colnames(raw) <- gsub("\\,", "_", colnames(raw))
  colnames(raw) <- gsub("\\.", "_", colnames(raw))
  colnames(raw) <- gsub("-", "_", colnames(raw))
  raw$activity <- as.factor(raw$activity)  

  raw
}

trainData <- cleanDataStructure(getSubjects(samsungData, c(1, 3, 5, 6)))  
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

trainErrors <- trainResiduals[trainResiduals$prediction != trainResiduals$actual, ]
trainErrorsTable <- table(trainErrors)
trainErrorsTable
trainErrorsTable / sum(trainErrorsTable) >= 0.1

trainData$predicted <- as.integer(trainResiduals$prediction == trainResiduals$actual)

walkSamples <- trainResiduals[trainResiduals$prediction == "walkdown" | trainResiduals$prediction == "walkup",]
walkSamples <- trainData[trainData$activity == "walkdown" | trainData$activity == "walkup", ]
walkSamples <- walkSamples[,-c(563)]

walkSamples$activity <- as.integer(walkSamples$activity)
walkLm <- bic.glm(activity ~ ., data = walkSamples, glm.family="poisson")
summary(walkLm)
walkLmStep <- step(walkLm)


testData <- cleanDataStructure(getSubjects(samsungData, c(27, 28, 29, 30)))

t <- tree(activity ~ ., data=trainData)
pr <- predict(t, testData, type="class")
pr <- data.frame(pr = pr, ac = testData$activity)
head(pr, 300)

sum(pr$pr == pr$ac) / nrow(pr)






