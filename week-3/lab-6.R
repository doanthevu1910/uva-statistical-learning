rm(list = ls()); cat("\014"); dev.off()

setwd("D:/UvA/BSc Econometrics/Year 2/Block 3/Statistical Learning/Session 6 - Tree-based methods/Lab 6")

getwd()

install.packages("tree")
install.packages("gbm")
install.packages("randomForest")

library(tree)
library(ISLR)

data (Carseats)
attach(Carseats)

High <- ifelse(Sales<=8,"No","Yes")

Carseats <- data.frame(Carseats, High)

tree.carseats <- tree(High~.-Sales, Carseats)
#Numeric variables are divided into X<a and X>a
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)

library (ISLR2)
attach (Carseats)
High <- factor(ifelse(Sales <= 8, "No", "Yes"))

Carseats <- data.frame(Carseats, High)

tree.carseats <- tree(High ~.-Sales, Carseats)

library(tree)
library(ISLR)

data (Carseats)
attach(Carseats)

High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
#Numeric variables are divided into X<a and X>a
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)

