library(dplyr)
library(ggplot2)
library(caTools)
library(corrgram)
library(randomForest)


data <- read.csv("~/MLprog/data.csv")
head(data)

any(is.na(data))

df = subset(data, select = -c(date, waterfront, view, yr_built, yr_renovated, street, city, statezip, country, sqft_basement))
head(df)
corrgram(df, lower.panel = panel.shade, upper.panel = panel.cor)

set.seed(38)

sampleSplit <- sample.split(Y=df$price, SplitRatio=0.8)
trainset <- subset(x=df, sampleSplit==TRUE)
testSet <- subset(x=df, sampleSplit==FALSE)

model <- lm(formula = price ~ ., data = trainset)
rf.forest <- randomForest(price ~ .,data = trainset,mtry = 3, importance=TRUE )

summary(model)

prediction <- predict(model, testSet)
result <- data.frame(testing$price, predict(rf.forest,testing,type = "response"))

modelEval <- cbind(testSet$price, prediction)
colnames(modelEval) <- c('Actual', 'Predicted')
modelEval <- as.data.frame(modelEval)

colnames(result) <- c('Actual', 'Predicted')
result <- as.data.frame(result)

plot(modelEval)
plot(result)

mse_reg <- mean((modelEval$Actual - modelEval$Predicted)^2)
rmse_reg <- sqrt(mse_reg)

mse_rf <- mean((result$Actual - result$Predicted)^2)
rmse_rf <- sqrt(mse_rf)

rmse_reg
rmse_rf
