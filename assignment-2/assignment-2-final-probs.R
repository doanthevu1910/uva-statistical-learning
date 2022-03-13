rm(list = ls()); cat("\014"); dev.off()

setwd("D:/UvA/BSc Econometrics/Year 2/Block 3/Statistical Learning/Assignment 2")

library(MASS); library(randomForest); set.seed(2022)

dataset1 <- read.csv("dataset1.csv")
dataset2 <- read.csv("dataset2.csv")
dataset3 <- read.csv("dataset3.csv")
dataset4 <- read.csv("dataset4.csv")
dataset5 <- read.csv("dataset5.csv")

#Logistic Regression, or
#Quadratic Discriminant Analysis, or
#A random forest with 500 trees using sqrt(p) variables and the seed number 2022 in R. 

#Dataset 1
data <- dataset1; attach(data)

##Logistic Regression

###over all features

model <- glm(target ~., data = data, 
             family = binomial)

summary(model)

p_hat <- predict(model, newdata = data, 
                 type = "response")

y1 <- p_hat

#Dataset 2
data <- dataset2; attach(data); set.seed(2022)

target <- as.factor(data$target)

##Random forest

model.rf <- randomForest(target ~., data = data, mtry = round(sqrt(ncol(data) - 1)), 
                         ntree = 500, importance = TRUE)

#p_hat <- predict(model.rf, newdata = data, type = "response")

p_hat <- predict(model.rf, type = "prob")[, 2]

y2 <- p_hat

y.test <- rep(0, 100); y.test[p_hat >= .5] <- 1

table(y.test, dataset2$target)

#Test

# Data set 2
set.seed(2022)
dataset2 <- read.csv("dataset2.csv")
summary(dataset2)
library(randomForest)
dataset2[,1] <- as.factor(dataset2$target)

n.p2 <- ncol(dataset2) - 1

m.ds2 <- randomForest(target ~., data = dataset2,
                      mtry = round(sqrt(n.p2)),
                      ntree = 500, importance = TRUE)
m.ds2
yhat.2 <- predict(m.ds2, type = "response")
prob.2.df <- predict(m.ds2, type = "prob")
prob.2 <- prob.2.df[,2]
sum(yhat.2 == dataset2$target)

yhat.prob2 <- rep(0, 100)
yhat.prob2[round(prob.2, digit = 5) >= .5] <- 1
table(yhat.prob2, yhat.2)

y2 <- prob.2

#Dataset 3
data <- dataset3; attach(data)

##Logistic Regression

###over all features

model <- glm(target ~., data = data, 
             family = binomial)

summary(model)

p_hat <- predict(model, newdata = data, 
                 type = "response")

y3 <- p_hat

#Dataset 4
data <- dataset4; attach(data)

##Logistic Regression

###over all features

model <- glm(target ~., data = data, 
             family = binomial)

summary(model)

p_hat <- predict(model, newdata = data, 
                 type = "response")

y4 <- p_hat

#Dataset 5
data <- dataset5; attach(data)

##QDA
qda.fit <- qda(target ~ ., data = data)

qda.predict <- predict(qda.fit, newdata = data, type = "response")

p_hat <- qda.predict$posterior[, 2]

y5 <- p_hat

#Results
result <- read.csv("Answer_Sheet.csv")

result$Data.Set.1 <- y1
result$Data.Set.2 <- y2
result$Data.Set.3 <- y3
result$Data.Set.4 <- y4
result$Data.Set.5 <- y5

write.csv(result, "Answer_Sheet.csv", row.names = FALSE)

View(round(result, 5))