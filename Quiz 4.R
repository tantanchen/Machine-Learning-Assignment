library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(e1071)

set.seed(125)

training <- segmentationOriginal[segmentationOriginal$Case == "Train",]
test <- segmentationOriginal[segmentationOriginal$Case == "Test",] 

fit <- train(Class~., data = training, method = "rpart")


library(pgmm)
data(olive)
olive = olive[,-1]

fit <- train(Area~., data = olive, method = "rpart")
newdata = as.data.frame(t(colMeans(olive)))
prediction <- predict(fit, newdata = newdata)


library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
fit <- train(chd~age+alcohol+obesity+tobacco+typea+ldl, data = trainSA, method = "glm", family = "binomial")
missClass <- function(values,prediction)
{
    sum(((prediction > 0.5)*1) != values)/length(values)
}
missClass(testSA$chd,predict(fit, newdata = testSA))
missClass(trainSA$chd,predict(fit, newdata = trainSA))

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

set.seed(33833)

fitRF <- train(y~., data = vowel.train, method = "rf")
fitGBM <- train(y~., data = vowel.train, method = "gbm", verbose = FALSE)

predRF <- predict(fitRF, newdata = vowel.test)
predGBM <- predict(fitGBM, newdata = vowel.test)

AccuracyRF <- confusionMatrix(predRF, vowel.test$y)
AccuracyGBM <- confusionMatrix(predGBM, vowel.test$y)

indexOfAgreed <- (predRF == predGBM)
AgreementAccuracy <- confusionMatrix(predRF[indexOfAgreed], vowel.test$y[indexOfAgreed])


set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
fitRF <- train(diagnosis~., data = training, method = "rf")
fitGBM <- train(diagnosis~., data = training, method = "gbm", verbose = FALSE)
fitLDA <- train(diagnosis~., data = training, method = "lda")

predRF <- predict(fitRF, newdata = testing)
predGBM <- predict(fitGBM, newdata = testing)
predLDA <- predict(fitLDA, newdata = testing)

dfStacked <- data.frame(predRF, predGBM, predLDA, diagnosis =testing$diagnosis)
fitStacked <- train(diagnosis~., data = dfStacked, method = "rf")
predStacked <- predict(fitStacked, newdata = dfStacked)

confusionMatrix(predRF, testing$diagnosis)$overall['Accuracy']
confusionMatrix(predGBM, testing$diagnosis)$overall['Accuracy']
confusionMatrix(predLDA, testing$diagnosis)$overall['Accuracy']
confusionMatrix(predStacked, testing$diagnosis)$overall['Accuracy']


set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)

fitLasso <- train(CompressiveStrength~., data = training, method = "lasso")
plot.enet(fitLasso$finalModel, xvar="penalty", use.color=TRUE)

set.seed(325)
svm <- svm(CompressiveStrength~., data = training)
predSVM <- predict(svm, newdata = testing)
sqrt(sum((predSVM - testing$CompressiveStrength)^2))


library(lubridate) # For year() function below
library(forecast)
library(readr)
dat = read_csv("~/Downloads/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

forecastModel <- bats(tstrain, h = 235, level = .95)
predForecast <- forecast(tstrain, h = 235, level = .95)
forecasting <- predict(forecastModel, newdata = testing$visitsTumblr)
df <- data.frame(visitsTumblr = testing$visitsTumblr, predForecast$lower, predForecast$upper)
df$forecasting.lower <= df$visitsTumblr & df$visitsTumblr <= df$forecasting.upper
accuracy(predForecast, testing$visitsTumblr)
plot(predForecast)
