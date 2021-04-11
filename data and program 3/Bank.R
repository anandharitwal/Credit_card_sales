# Libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(rpart)
library(rpart.plot)
library(caret)
library(glmnet)
library(mlbench)
library(rAverage)
library(tidyverse)
library(scales)
library(lubridate)
library(ggridges)
library(readr)
library(rms)
# Data
data <- read_csv("D:/data mininig/dataset/10000 rows and 10 columns/bank dataset/bank/bank-full.csv")
str(data)
View(data)
data<- data[-c(5,9,10,15,16)]
# Visualization

data %>%
         ggplot(aes(x=y, y=age, fill = y)) +
         geom_boxplot() +
         ggtitle("Box Plot")

data %>% ggplot(aes(x=age, fill = y)) +
         geom_density(alpha=0.8, color= 'black') +
         ggtitle("Density Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Naive Bayes Model
model <- naive_bayes(y ~ ., data = train, usekernel = T)
model
summary(model)
require(rms)
model1b<-lrm(y~., data= train)
model1b
plot(model)

# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$y))
1 - sum(diag(tab1)) / sum(tab1)
confusionMatrix(tab1)
# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$y))
1 - sum(diag(tab2)) / sum(tab2)
confusionMatrix(tab2)


# applying Logistic regression
mymodel <- multinom(y~., data = train)
summary(mymodel)

# Confusion Matrix & Misclassification Error - training
p2 <- predict(mymodel, train)
tab2 <- table(p2, train$y)
tab2

confusionMatrix(tab2)
# Confusion Matrix & Misclassification Error - testing
p3 <- predict(mymodel, test)
tab3 <- table(p3, test$y)
tab3

confusionMatrix(tab3)

#Building desicion tree 
decision_tree <- rpart(y~., data = train, method = 'class')
summary(decision_tree)
View(summary(decision_tree))
rpart.plot(decision_tree)
predict_injury<-predict(decision_tree,test,type='class')
table_mat <- table(test$y, predict_injury)
table_mat
confusionMatrix(table_mat)

#postpruning for train  data
printcp(decision_tree)
plotcp(decision_tree)
pruned.tree <- prune(decision_tree, cp = 0.01)
rpart.plot(pruned.tree)
View(summary(pruned.tree))
predict_injury_pruned<-predict(pruned.tree,train,type='class')
table_mat5 <- table(train$y, predict_injury_pruned)
table_mat5
confusionMatrix(table_mat5)
