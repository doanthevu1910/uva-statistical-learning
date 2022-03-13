rm(list = ls())

setwd("D:/UvA/BSc Actuarial Science/Year 2/Block 3/Statistical Learning/Session 2 - High dimensional linear regression/Lab 2")

getwd()

#1 Replicate the results in sub sections 3.6.1-3.6.3

library (MASS)
library (ISLR2)

head(Boston)

attach(Boston)

lm.fit <- lm(medv ~ lstat, data = Boston)

summary(lm.fit)

names(lm.fit)

coef(lm.fit)

confint (lm.fit)

predict(lm.fit , data.frame(lstat = (c(5, 10, 15))),
           interval = "confidence")

predict(lm.fit , data.frame(lstat = (c(5, 10, 15))),
        interval = "prediction")

plot (lstat , medv)
abline (lm.fit)

abline (lm.fit , lwd = 3)
abline (lm.fit , lwd = 3, col = " red ")
plot (lstat , medv , col = " red ")
plot (lstat , medv , pch = 20)
plot (lstat , medv , pch = "+")
plot (1:20, 1:20, pch = 1:20)

par (mfrow = c(2, 2))
plot (lm.fit)

plot ( predict (lm.fit), residuals (lm.fit))
plot ( predict (lm.fit), rstudent (lm.fit))

plot ( hatvalues (lm.fit))
which.max ( hatvalues (lm.fit))

lm.fit <- lm(medv ~ lstat + age , data = Boston)
summary (lm.fit)

lm.fit <- lm(medv ~ ., data = Boston)
summary (lm.fit)

library (car)
vif(lm.fit)

lm.fit1 <- lm(medv ~ . - age , data = Boston)
summary(lm.fit1)

lm.fit1 <- update (lm.fit , ~ . - age)

#2 Replicate the results in the section 6.5, 
#only for the best subset selection 
#and the forward stepwise regression.

##Best Subset Selection
library (ISLR2)
View (Hitters)
names (Hitters)

dim (Hitters)
sum (is.na(Hitters$Salary))

Hitters <- na.omit (Hitters)
dim (Hitters)
sum (is.na(Hitters))

install.packages("leaps")
library (leaps)
regfit.full <- regsubsets (Salary ~ ., Hitters)
summary (regfit.full)

regfit.full <- regsubsets (Salary ~ ., data = Hitters ,
                           nvmax = 19)
reg.summary <- summary (regfit.full)

names(reg.summary)

reg.summary$rsq

par (mfrow = c(2, 2))
plot (reg.summary$rss , xlab = " Number of Variables ",
        ylab = " RSS ", type = "l")
plot (reg.summary$adjr2 , xlab = " Number of Variables ",
        ylab = " Adjusted RSq ", type = "l")

which.max(reg.summary$adjr2)
points (11, reg.summary$adjr2[11], col = " red ", cex = 2,
          pch = 20)

plot (reg.summary$cp, xlab = " Number of Variables ",
      ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points (10, reg.summary$cp[10], col = " red ", cex = 2,
        pch = 20)

which.min (reg.summary$bic)
plot (reg.summary$bic , xlab = " Number of Variables ",
      ylab = " BIC ", type = "l")
points (6, reg.summary$bic[6], col = " red ", cex = 2,
        pch = 20)

plot (regfit.full , scale = "r2")
plot (regfit.full , scale = "adjr2")
plot (regfit.full , scale = "Cp")
plot (regfit.full , scale = "bic")

coef (regfit.full , 6)

##Forward Stepwise Selection

regfit.fwd <- regsubsets (Salary ~ ., data = Hitters ,
                            nvmax = 19, method = "forward")
summary (regfit.fwd)

coef (regfit.full , 7)
coef (regfit.fwd , 7)

#3 Applied exercise 6.8.8, part (a) - (d). 
#Also use 5-fold cross validation 
#to choose the best model. 

set.seed(1)
X <- rnorm(100)
epsilon <- rnorm(100)

Y <- 0 + 0.1*X + 0.2*X^2 + 0.3*X^3 + epsilon 

library(leaps)
data <- data.frame(y = Y, x = X)
mod.full <- regsubsets(y ~ poly(x, 10, raw = T), data = data, nvmax = 10)
mod.summary <- summary(mod.full)

which.min(mod.summary$cp)

which.min(mod.summary$bic)

which.max(mod.summary$adjr2)

par(mfrow = c(2, 2))
plot(mod.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(2, mod.summary$cp[2], pch = 20, col = "red", lwd = 3)

plot(mod.summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l")
points(1, mod.summary$bic[1], pch = 20, col = "red", lwd = 3)

plot(mod.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, type = "l")
points(2, mod.summary$adjr2[2], pch = 20, col = "red", lwd = 3)

coefficients(mod.full, id = 2)

mod.fwd = regsubsets(y ~ poly(x, 10, raw = T), data = data, 
                     nvmax = 10, method = "forward")
fwd.summary <- summary(mod.fwd)
which.min(fwd.summary$cp)

par(mfrow = c(2, 2))
plot(fwd.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(2, fwd.summary$cp[2], pch = 20, col = "red", lwd = 3)

plot(fwd.summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l")
points(1, fwd.summary$bic[1], pch = 20, col = "red", lwd = 3)

plot(fwd.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, type = "l")
points(2, fwd.summary$adjr2[2], pch = 20, col = "red", lwd = 3)

coefficients(mod.fwd, id = 2)