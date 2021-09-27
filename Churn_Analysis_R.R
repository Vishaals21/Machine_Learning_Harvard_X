library(tidyverse)
install.packages("HistData")
library(HistData)
library(xlsx)
library(caret)
getwd()
setwd("/Users/vishalsharma/Downloads/Data Analytics/Excel Files for R")
data <- read.xlsx("Churn.xlsx",sheetIndex =1)
y <- data$Churn

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- data %>% slice(-test_index)
test_set <- data %>% slice(test_index)
y <- train_set$Churn
z <- as.factor(y)

knn_fit <- knn3(z ~ Sales+Region, data = train_set, k =5)

c <- as.factor(test_set$Churn)
c

y_hat_knn <- predict(knn_fit, test_set, type = "class")
confusionMatrix(data = y_hat_knn, reference = c)$overall["Accuracy"]

train_set
str(y)
z <- as.factor(y)
str(z)
fit_glm <- glm(z~Sales+Region, data=train_set, family="binomial")
p_hat_logistic <- predict(fit_glm, test_set)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, "Yes", "No"))
confusionMatrix(data = y_hat_logistic, reference = c)$overall[1]
