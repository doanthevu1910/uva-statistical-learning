rm(list = ls())

setwd("D:/UvA/BSc Econometrics/Year 2/Block 3/Statistical Learning/Session 4 - Classification/Lab 4")

getwd()

#1. Replicate the results in sub sections 4.6.1-4.6.4.

library(ISLR)
library(MASS)

names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)

cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

glm.probs <- predict(glm.fits,type="response")
glm.probs[1:10]
contrasts(Direction)

glm.pred <- rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred==Direction)

train <- (Year < 2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                data=Smarket, family=binomial, subset=train)
glm.probs <- predict(glm.fits, Smarket.2005, type="response")

glm.pred <- rep("Down", 252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

glm.fits <- glm(Direction ~ Lag1 + Lag2, 
                data=Smarket, family=binomial, subset=train)
glm.probs <- predict(glm.fits, Smarket.2005, type="response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)

predict(glm.fits, 
        newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)),
        type="response")

library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, 
               data = Smarket, subset = train)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

sum(lda.pred$posterior[,1]>.9)

qda.fit <- qda(Direction ~ Lag1 + Lag2,
               data=Smarket, subset=train)
qda.fit

qda.class <- predict(qda.fit,Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

#2. Applied exercise 4.7.13, parts a-f.

set.seed(0)
data(Weekly)
View(Weekly)

##a

Direction = Weekly$Direction
Weekly$Direction = NULL 
Weekly$NumericDirection = as.numeric( Direction ) # Maps Down=>1 and Up=>2
Weekly$NumericDirection[ Weekly$NumericDirection==1 ] = -1 # Maps Down=>-1 and Up=>2
Weekly$NumericDirection[ Weekly$NumericDirection==2 ] = +1 # Maps Down=>-1 and Up=>+1

Weekly.cor <- cor(Weekly); Weekly.cor

##b
Weekly$NumericDirection <- NULL 
Weekly$Direction <- Direction

five_lag_model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly, family=binomial)
print(summary(five_lag_model))

print(contrasts(Weekly$Direction))

##c
p_hat <- predict(five_lag_model, 
                 newdata=Weekly, 
                 type="response")
y_hat <- rep("Down", length(p_hat))
y_hat[p_hat > 0.5] <- "Up"

CM <- table(predicted = y_hat, 
            truth=Weekly$Direction)
print(CM)
print(sprintf("LR (all features): overall fraction correct= %10.6f", (CM[1,1] + CM[2,2])/sum(CM)))

##d
Weekly.train = ( Weekly$Year >= 1990 ) & ( Weekly$Year <= 2008 ) # our training set 
Weekly.test = ( Weekly$Year >= 2009 ) # our testing set 
lag2_model = glm( Direction ~ Lag2, data=Weekly, family=binomial, subset=Weekly.train )

# CM on test data :
p_hat = predict( lag2_model, newdata=Weekly[Weekly.test,], type="response" )
y_hat = rep( "Down", length(p_hat) )
y_hat[ p_hat > 0.5 ] = "Up"
CM = table( predicted=y_hat, truth=Weekly[Weekly.test,]$Direction )
print( CM )
print( sprintf( "LR (only Lag2): overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM)))

##e. LDA
lda.fit = lda( Direction ~ Lag2, data=Weekly, subset=Weekly.train )

lda.predict = predict( lda.fit, newdata=Weekly[Weekly.test,] )
CM = table( predicted=lda.predict$class, truth=Weekly[Weekly.test,]$Direction )
print( CM )
print( sprintf( "LR (only Lag2): overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM)))

##f. QDA
qda.fit = qda( Direction ~ Lag2, data=Weekly, subset=Weekly.train )

qda.predict = predict( qda.fit, newdata=Weekly[Weekly.test,] ) 
CM = table( predicted=qda.predict$class, truth=Weekly[Weekly.test,]$Direction )

