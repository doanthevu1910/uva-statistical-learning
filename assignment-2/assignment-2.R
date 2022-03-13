rm(list = ls()); cat("\014"); dev.off()

setwd("D:/UvA/BSc Econometrics/Year 2/Block 3/Statistical Learning/Assignment 2")

library(MASS); library(randomForest); set.seed(2022)

dataset1 <- read.csv("dataset1.csv")
dataset2 <- read.csv("dataset2.csv")
dataset3 <- read.csv("dataset3.csv")
dataset4 <- read.csv("dataset4.csv")
dataset5 <- read.csv("dataset5.csv")

#Logistic Regression
#Quadratic Discriminant Analysis
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

y_hat <- rep(0, length(p_hat))
y_hat[p_hat > 0.5] <- 1

CM <- table(predicted = y_hat, truth = data$target); CM

print(sprintf("LR (all features): overall fraction correct= %10.6f", 
              (CM[1,1] + CM[2,2])/sum(CM)))

###over features_2, features_4

model_24 <- glm(target ~ features_2 + features_4, 
                data = data, family = binomial)

summary(model_24)

p_hat <- predict(model_24, newdata = data, 
                 type = "response")

y_hat <- rep(0, length(p_hat)); y_hat[p_hat > 0.5] <- 1

CM <- table(predicted = y_hat, truth = data$target); CM

print(sprintf("LR (features_2 + features_4): overall fraction correct= %10.6f", (CM[1,1] + CM[2,2])/sum(CM)))

##QDA
qda.fit <- qda(target ~ features_2 + features_4, 
               data = data)

qda.predict <- predict(qda.fit, newdata = data,  
                       type = "response")

CM <- table(predicted = qda.predict$class, truth = data$target); CM

print(sprintf("QDA (features_2 + features_4): overall fraction correct= %10.6f", (CM[1,1] + CM[2,2])/sum(CM)))

#Dataset 2
##Logistic Regression

###over all features

data <- dataset2; attach(data)

model <- glm(target ~., data = data, 
                  family = binomial)

summary(model)

p_hat <- predict(model, newdata = data, 
                 type = "response")

y_hat <- rep(0, length(p_hat)); y_hat[p_hat > 0.5] <- 1

CM <- table(predicted = y_hat, truth = data$target); CM

print(sprintf("LR (all features): overall fraction correct= %10.6f", (CM[1,1] + CM[2,2])/sum(CM)))

##Random forest

data <- dataset2; attach(data)

model.rf <- randomForest(target ~., data = data, mtry = sqrt(ncol(data) - 1), 
                         ntree = 500, importance = TRUE)

# p_hat <- predict(model.rf, newdata = data, type = "response")

p_hat <- predict(model.rf, type = "response")

y_hat <- rep(0, length(p_hat)); y_hat[p_hat > 0.5] <- 1

CM <- table(predicted = y_hat, truth = data$target); CM

print(sprintf("RF: overall fraction correct= %10.6f", (CM[1,1] + CM[2,2])/sum(CM)))

#Dataset 3
##Logistic Regression

###over all features

data <- dataset3; attach(data)

model <- glm(target ~., data = data, 
             family = binomial)

summary(model)

p_hat <- predict(model, newdata = data, 
                 type = "response")

y_hat <- rep(0, length(p_hat))
y_hat[p_hat > 0.5] <- 1

CM <- table(predicted = y_hat, truth = data$target); CM

print(sprintf("LR (all features): overall fraction correct= %10.6f", (CM[1,1] + CM[2,2])/sum(CM)))

#Dataset 4

##Logistic Regression 

###over all features

data <- dataset4; attach(data)

model <- glm(target ~., data = data, 
             family = binomial)

summary(model)

p_hat <- predict(model, newdata = data, 
                 type = "response")

y_hat <- rep(0, length(p_hat))
y_hat[p_hat > 0.5] <- 1

CM <- table(predicted = y_hat, truth = data$target); CM

print(sprintf("LR (all features): overall fraction correct= %10.6f", (CM[1,1] + CM[2,2])/sum(CM)))

##Random forest

data <- dataset4; attach(data)

model.rf <- randomForest(target ~., data = data, mtry = sqrt(ncol(data) - 1), 
                         ntree = 500, importance = TRUE)

#p_hat <- predict(model.rf, newdata = data, type = "response")

p_hat <- predict(model.rf, type = "response")

y_hat <- rep(0, length(p_hat)); y_hat[p_hat > 0.5] <- 1

CM <- table(predicted = y_hat, truth = data$target); CM

print(sprintf("RF: overall fraction correct= %10.6f", (CM[1,1] + CM[2,2])/sum(CM)))

#Dataset 5
data <- dataset5; attach(data)

##QDA
qda.fit <- qda(target ~ ., data = data)

qda.predict <- predict(qda.fit, newdata = data,  
                       type = "response")

CM <- table(predicted = qda.predict$class, truth = data$target); CM

print(sprintf("QDA (features_2 + features_4): overall fraction correct= %10.6f", (CM[1,1] + CM[2,2])/sum(CM)))