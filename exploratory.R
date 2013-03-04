library(BMA)
load("/tmp/samsungData.rda")

colnames(samsungData)


samsungData$actionNum <- as.numeric(as.factor(samsungData$activity))

samsungDataNew <- samsungData[samsungData$subject %in% c(1, 3, 5, 6), c(1:100, 564)]
samsungDataNew <- samsungDataNew[sub("tGravityAccMag", "", names(samsungDataNew)) != names(samsungDataNew),]
lmodel <- glm(actionNum ~ ., data = samsungDataNew, family="poisson")
lmodel <- bic.glm(actionNum ~ ., data = samsungDataNew, glm.family="poisson")
summary(lmodel)
st <- step(lmodel)

summary(st)

samsungDataTest <- samsungData[samsungData$subject %in% c(27, 28, 29, 30), c(1:100, 564)]

samsungDataTest <- samsungDataNew

pred <- predict.glm(st, samsungDataTest)
pred <- predict(lmodel, samsungDataTest)

res <- data.frame(test = samsungDataTest$actionNum, pred = exp(pred))
res$result <- abs(res$pred - res$test) < 0.5
nrow(res)
sum(res$result)

sum(res$result) / nrow(res)


summary(abs(res$test - res$pred))

