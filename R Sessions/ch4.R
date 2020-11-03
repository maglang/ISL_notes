require(ISLR)
names(Smarket)
library(ISLR)
names(Smarket)
summary(Smarket)
?summary
pairs(Smarket, col=Smarket$Direction)

# Logistic Regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)
glm.probs~predict(glm.fit, type = 'response')
glm.probs[1:5]
glm.pred=ifelse(glm.probd>0.5, "Up", "Down")
attach(Smarket)
table(glm.pred, Direction)
mean(mean.pred == Direction)
mean(glm.pred == Direction)

# Make training and test set
train = Year < 2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,], type = "response")
glm.pred=ifelse(glm.probs>0.50, "Up", "Down")
Direction.2005 =Smarket$Direction[!train]
table(glm.pred, Direction)
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

# Fit smaller model
glm.fit=glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,], type = "response")
glm.pred=ifelse(glm.probs>0.50, "Up", "Down")
Direction.2005 =Smarket$Direction[!train]
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

# Linear Discriminant Analysis
require(ISLR)
require(MASS)
install.packages(MASS)
install.packages("MASS")
require(MASS)
?lda
lda.fit=lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005=subset(Smarket, Year==2005)
lda.pred=predict(lda.fit, Smarket.32005)
lda.pred=predict(lda.fit, Smarket.2005)
lda.pred[1:5]
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)
require(ISLR)
names(Smarket)
library(ISLR)


