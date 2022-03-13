rm(list = ls())

setwd("D:/UvA/BSc Actuarial Science/Year 2/Block 3/Statistical Learning/Assignment 1")
getwd()

library(leaps); library(glmnet); library(pls)
par(mfrow = c(2,2))

predict.regsubsets <- function(object, newdata, id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars] %*% coefi
}

bestpara <- read.csv("bestpara.csv"); View(bestpara)

#1 
dataset1 <- read.csv("dataset1.csv"); data <- dataset1; attach(data)

##Forward Stepwise Selection
step.model <- regsubsets(target ~., data = data, nvmax = 3, method = "forward")
mod.summary <- summary(step.model)
coef(step.model, 3) #3
#y1 <- -0.03070304 + 1.46052406*features_2 + 2.04519819*features_8 + 1.79561847*features_14 

y1 <- predict.regsubsets(step.model, data, id = 3)

MSE <- mean((target - y1)^2); MSE

View(y1)

#2 
dataset2 <- read.csv("dataset2.csv"); data <- dataset2; attach(data)

##Ridge Regression
full.mat <- model.matrix(target ~., data = data)
mod.ridge <- glmnet(full.mat, data$target, alpha=0, lambda=10.7226722201032)
y2 <- predict(mod.ridge, newx = full.mat, s = 10.7226722201032)
MSE <- mean((data$target - y2)^2); MSE

View(y2)

#3
dataset3 <- read.csv("dataset3.csv"); data <- dataset3; attach(data)

##LASSO Regression
full.mat <- model.matrix(target ~., data = data)
mod.lasso <- glmnet(full.mat, data$target, alpha=1, lambda=0.0132194114846603)
y3 <- predict(mod.lasso, newx = full.mat, s = 0.0132194114846603)
MSE <- mean((data$target - y3)^2); MSE

View(y3)

#4
dataset4 <- read.csv("dataset4.csv"); data <- dataset4; attach(data)

##PCR
pcr.fit <- pcr(target ~., data = data, scale = TRUE, ncomp = 3, validation="CV")
summary (pcr.fit)
coef(pcr.fit)
y4 <- predict(pcr.fit, data, ncomp = 3)
MSE <- mean((data$target - y4)^2); MSE 

View(y4)

#5
dataset5 <- read.csv("dataset5.csv"); data <- dataset5; attach(data)

##Forward Stepwise Selection
step.model <- regsubsets(target ~., data = data, nvmax = 2, method = "forward")
mod.summary <- summary(step.model)
coef(step.model, 2) 
#y5 <- 0.4280091 + 3.8586647*features_1 + 1.0161602*features_2
MSE <- mean((target - y5)^2); MSE

y5 <- predict.regsubsets(step.model, data, id = 2)

View(y5)

result <- read.csv("Answer_Sheet.csv")

result$Data.Set.1 <- y1
result$Data.Set.2 <- y2
result$Data.Set.3 <- y3
result$Data.Set.4 <- y4
result$Data.Set.5 <- y5

write.csv(result, "Answer_Sheet.csv", row.names = FALSE)

View(result)