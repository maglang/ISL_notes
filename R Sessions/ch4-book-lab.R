library(ISLR)
names(Smarket)
# The dataset contains Yaear, Lag1-Lag5 variables for 5 previous 
# training days, Volume for number of shares traded the previous day, in billions,
# Today (% return on date), and Direction (up or down), which is the target.
dim(Smarket)
summary(Smarket)
pairs(Smarket)

#get pairwise correlations for predictors
#except for Direction since it is qualitative
cor(Smarket[, -9])
?attach
attach(Smarket)
plot(Volume)

# Logistic Regression
# predict Direction based on all features
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)
# get coefficients for fitted model
coef(glm.fit)
# use summary to access other aspects of fitted model
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

# Predict function to get probabilities
# "Type = response" gives P(Y=1|X) rather than other measures such as logits
# Contrasts function indicates the 1/0 in this model
glm.probs=predict(glm.fit, type = 'response')
glm.probs[1:10]
contrasts(Direction)

# Convert probabilities into Up or Down
# First create a vector of 1250 Down elements
glm.pred=rep("Down", 1250)
# Replace with Up if predicted probability exceeds 0.5
glm.pred[glm.probs>.5] = 'Up'

# Create confusion matrix to determine correctly classified
table(glm.pred, Direction)
# Get accuracy
(145+507)/1250
#52%
# Or use mean function
mean(glm.pred==Direction)

# create training dataset
train = (Year<2005)
Smarket.2005 = Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 = Direction[!train]


# Linear Discriminant Analysis
library(MASS)
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
# Prior probabilities (prob of when market went up or down)
# Group means: average of each predictor within each class - shows that
# there is a tendency for previous 2 days to be negative on "Up" days
# Coefficients: linear combination of Lag1 and Lag2 to form the LDA decision rule
plot(lda.fit)

# Predict function
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)

# Confusion matrix
lda.class=lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)

# applying 50% threshold to the posterior probabilities allows to recreate the predictions in lda.pred$class
sum(lda.pred$posterior[,1]>=0.5)
sum(lda.pred$posterior[,1]<.5)

# Posterior probabilities pertains to "Down"
lda.pred$posterior [1:20 ,1]
lda.class[1:20]


# Quadratic Discriminant Analysis
#import MASS, which contains LDA and QDA models
library(MASS)
qda.fit=qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
names(qda.fit)
# Contains the group means but not the coefficients because it is quadratic

# Predict
qda.class=predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

# K Nearest Neighbors
# knn can form predictions using a single command
# 4 inputs:
# matrix containing the predictors (train.X)
# matrix containing predictors for which predictions will be made, test.X
# vector containing class labels for train (train.Direction)
# value for K

# cbind: column bind
install.packages("class")
library(class)
train.X=cbind(Lag1, Lag2)[train,]
test.X=cbind(Lag1, Lag2)[!train,]
train.Direction=Direction[train]
# use knn function, remember to add random seed
set.seed(1)
knn.pred=knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

# try k=3
knn.pred=knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)
