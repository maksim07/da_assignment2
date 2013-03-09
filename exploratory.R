library(BMA)
load("/tmp/samsungData.rda")

source("utils.R")

colnames(samsungData)

samsungData$actionNum <- as.numeric(as.factor(samsungData$activity))

samsungDataNew <- samsungData[samsungData$subject %in% c(1, 3, 5, 6), c(1:100, 564)]
samsungDataNew <- samsungDataNew[sub("tGravityAccMag", "", names(samsungDataNew)) != names(samsungDataNew),]

samsungDataTest <- samsungData[samsungData$subject %in% c(27, 28, 29, 30), c(1:100, 564)]

samsungDataTest <- samsungDataNew

any(is.na(samsungData))

sum(table(colnames(samsungData)) > 1)
samsungDataClean <- cleanDataStructure(samsungData)
sum(table(colnames(samsungDataClean)) > 1)


colnames(samsungDataClean)

any(is.na(samsungDataClean))
colnames(samsungDataClean)
samsungDataClean <- samsungDataClean[,-c(480)]
colnames(cleanDataStructure(samsungData))

bglm <- bic.glm(activity ~ ., data=samsungDataClean, glm.family="poisson")


