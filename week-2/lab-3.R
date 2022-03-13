rm(list = ls())

setwd("D:/UvA/BSc Actuarial Science/Year 2/Block 3/Statistical Learning/Session 3 - Shrinkage and dimension reduction/Lab 3")

getwd()

#1 Replicate the results in Section 6.6. (6.5?)

library(glmnet)

library (ISLR2)
names (Hitters)

dim (Hitters)
sum (is.na(Hitters$Salary))

Hitters <- na.omit (Hitters)
dim (Hitters)
sum (is.na(Hitters))

#Ridge Regression

x <- model.matrix(Salary ~ ., Hitters)[, -1]

y <- Hitters$Salary

grid <- 10^seq(10, -2, length = 100)

ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

round(predict(ridge.mod , s = 50, type = "coefficients")[1:20, ], 3)

set.seed (1)
train <- sample (1: nrow (x), nrow (x)/2)
test <- (-train)
y.test <- y[test]

ridge.mod <- glmnet (x[train, ], y[train], alpha = 0,
                     lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod , s = 4, newx = x[test , ])
mean((ridge.pred - y.test)^2)

mean((mean (y[train]) - y.test)^2)

ridge.pred <- predict (ridge.mod, s = 1e10, newx = x[test , ])
mean ((ridge.pred - y.test)^2)

ridge.pred <- predict (ridge.mod , s = 0, newx = x[test , ],
                       exact = T, x = x[train , ], y = y[train])
mean ((ridge.pred - y.test)^2)

lm(y ~ x, subset = train)
predict (ridge.mod , s = 0, exact = T, type = "coefficients",
           x = x[train , ], y = y[train])[1:20, ]

set.seed (1)
cv.out <- cv.glmnet (x[train , ], y[train], alpha = 0)
plot (cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict (ridge.mod , s = bestlam, newx = x[test , ])
mean ((ridge.pred - y.test)^2)

out <- glmnet (x, y, alpha = 0)
predict (out , type = "coefficients", s = bestlam)[1:20, ]

#LASSO Regression

lasso.mod <- glmnet (x[train , ], y[train], alpha = 1,
                     lambda = grid)
plot (lasso.mod)

set.seed (1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot (cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict (lasso.mod , s = bestlam,
                         newx = x[test , ])
mean ((lasso.pred - y.test)^2)

out <- glmnet (x, y, alpha = 1, lambda = grid)
lasso.coef <- predict (out , type = "coefficients",
                         s = bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef != 0]

#2 Replicate the results in the sub section 6.7.1. *6.5.3??)

library (pls)
set.seed (2)
pcr.fit <- pcr (Salary ~ ., data = Hitters, 
                scale = TRUE, validation = "CV")
summary(pcr.fit)

validationplot (pcr.fit , val.type = "MSEP")

set.seed (1)
pcr.fit <- pcr (Salary ~ ., data = Hitters , subset = train ,
                  scale = TRUE , validation = "CV")
validationplot(pcr.fit , val.type = "MSEP")

pcr.pred <- predict (pcr.fit , x[test , ], ncomp = 5)
mean ((pcr.pred - y.test)^2)

pcr.fit <- pcr (y ~ x, scale = TRUE, ncomp = 5)
summary (pcr.fit)

#3 Applied exercise 6.8.9 part (a)-(e), but using 10 fold cross validation.

