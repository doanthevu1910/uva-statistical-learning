rm(list = ls())

setwd("D:/UvA/BSc Actuarial Science/Year 2/Block 3/Statistical Learning/Session 1 - Bias-variance tradeoff/Computer Lab 1")

getwd()

#1 Replicate the results in Section 2.3.1

x <- rnorm (50)
y <- x + rnorm (50, mean = 50, sd = .1)
cor (x, y)

set.seed(1303)
rnorm(50)

set.seed (3)
y <- rnorm (100)
mean (y)
var (y)
sqrt ( var (y))
sd(y)

#2 Applied exercise 2.4.8, part i to part v.

college <- read.csv("College.csv")

View(college)

rownames(college) <- college[, 1]
college <- college[, -1]

# fix(college)
summary(college)

pairs(college[, 2:10])

attach(college)

plot(Private, Outstate)
boxplot(x = Outstate, y = Private)

Elite <- rep ("No", nrow (college))
Elite[college$Top10perc > 50] <- " Yes "
Elite <- as.factor (Elite)
college <- data.frame (college , Elite)

summary(college)
boxplot(x = Outstate, y = Private)

par(mfrow = c(2, 2))

hist(Outstate)
hist(Apps)
hist(Accept)
hist(Enroll)

#3 Replicate the results in Section 5.3

install.packages("ISLR2")
library(ISLR2)
set.seed (1)
train <- sample (392, 196)

?sample

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 <- lm(mpg ~ poly (horsepower , 2), 
              data = Auto, subset = train)
mean((mpg - predict (lm.fit2 , Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly (horsepower , 3), 
              data = Auto, subset = train)
mean ((mpg - predict (lm.fit3 , Auto))[-train]^2)

set.seed (2)
train <- sample (392, 196)
lm.fit <- lm(mpg ~ horsepower , subset = train)
mean ((mpg - predict (lm.fit , Auto))[-train ]^2)
lm.fit2 <- lm(mpg ~ poly (horsepower , 2), data = Auto ,
              subset = train)
mean ((mpg - predict (lm.fit2 , Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly (horsepower , 3), data = Auto ,
              subset = train)
mean ((mpg - predict (lm.fit3 , Auto))[-train]^2)

glm.fit <- glm (mpg ~ horsepower , data = Auto)
coef (glm.fit)

lm.fit <- lm (mpg ~ horsepower , data = Auto)
coef (lm.fit)

library (boot)
glm.fit <- glm (mpg ~ horsepower , data = Auto)
cv.err <- cv.glm (Auto, glm.fit)
cv.err$delta

cv.error <- rep (0, 10)
for (i in 1:10) {
  glm.fit <- glm (mpg ~ poly (horsepower , i), data = Auto)
  cv.error[i] <- cv.glm (Auto , glm.fit)$delta[1]
}
cv.error

set.seed(17)
cv.error.10 <- rep (0, 10)
for (i in 1:10) {
  glm.fit <- glm (mpg ~ poly (horsepower , i), data = Auto)
  cv.error.10[i] <- cv.glm (Auto , glm.fit, K=10)$delta[1]
}
cv.error.10

alpha.fn <- function (data , index) {
  X <- data$X[index]
  Y <- data$Y[index]
  ( var (Y) - cov (X, Y)) / ( var (X) + var (Y) - 2 * cov (X, Y))
}

alpha.fn(Portfolio , 1:100)

set.seed (7)
alpha.fn(Portfolio , sample (100, 100, replace = T))

boot (Portfolio , alpha.fn, R = 1000)

boot.fn <- function (data , index) {
  coef (lm(mpg ~ horsepower , data = data , subset = index)) 
}
boot.fn(Auto, 1:392)

set.seed (1)
boot.fn(Auto , sample (392, 392, replace = T))

boot.fn(Auto , sample (392, 392, replace = T))

boot(Auto, boot.fn, 1000)

summary (lm(mpg ~ horsepower , data = Auto))$coef

boot.fn <- function (data , index) {
  coef (lm(mpg ~ horsepower + I(horsepower^2),
           data = data , subset = index))
}
set.seed (1)
boot (Auto , boot.fn, 1000)

summary (lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef

#4 Applied exercise 5.4.8, part (a)-(e)

set.seed (1)
x <- rnorm (100)
y <- x - 2 * x^2 + rnorm (100)

## n = 100, p = 2
par(mfrow = c(1, 1))
plot(x, y)

?glm

library(boot)
Data = data.frame(x, y)
set.seed(1)
#i
glm.fit = glm(y ~ x)
cv.glm(Data, glm.fit)$delta

# ii.
glm.fit = glm(y ~ poly(x, 2))
cv.glm(Data, glm.fit)$delta

# iii.
glm.fit = glm(y ~ poly(x, 3))
cv.glm(Data, glm.fit)$delta

# iv.
glm.fit = glm(y ~ poly(x, 4))
cv.glm(Data, glm.fit)$delta

library(boot)
Data = data.frame(x, y)
set.seed(7)
#i
glm.fit = glm(y ~ x)
cv.glm(Data, glm.fit)$delta

# ii.
glm.fit = glm(y ~ poly(x, 2))
cv.glm(Data, glm.fit)$delta

# iii.
glm.fit = glm(y ~ poly(x, 3))
cv.glm(Data, glm.fit)$delta

# iv.
glm.fit = glm(y ~ poly(x, 4))
cv.glm(Data, glm.fit)$delta

## exactly same result regardless of the seed

##quadratic form (ii.) has the lowest error, since Y ~ X is quadratic
