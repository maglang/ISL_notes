# Validation Set Approach
library(ISLR)
set.seed(1)
train = sample(392,196)
?sample
?subset

lm.fit = lm(mpg~horsepower, data=Auto, subset=train)

attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit=lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit=lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

# Use another sample
set.seed(2)
train = sample(392,196)

lm.fit = lm(mpg~horsepower, data=Auto, subset=train)

attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit=lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit=lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

# Leave-One-Out Cross-Validation

# glm without passing a family argument runs a linear regression
glm.fit=glm(mpg~horsepower ,data=Auto)
coef(glm.fit)

# glm is same as lm
lm.fit=lm(mpg~horsepower ,data=Auto)
coef(lm.fit)

# but glm function has cv.glm as part of the boot library
library(boot)

glm.fit = glm(mpg~horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)

# delta contains cross validation results
cv.err$delta

# more complex polynomial fits
cv.error=rep (0,5)

for (i in 1:5){
  glm.fit = glm(mpg~poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error #Result is a drop in estimated test MSE between linear and quadratic fits
# No improvement from using higher-order polynomials


# K-Fold Cross-Validation

set.seed(17)
cv.error.10 = rep(0,10)
for (i in 1:10){
  glm.fit = glm(mpg~poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10
# delta returns 2 numbers:
# 1. standard k-fold CV estimate
# 2. bias-corrected version

# The Bootstrap
# 2 steps:
# 1. create a function that computes the statistic of interest
# 2. use the boot() function to perform bootstrap by repeatedly sampling observations from the dataset with replacement
require(ISLR)
require(boot)

# function is called alpha.fn
alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/var(X)+var(Y)-2*cov(X,Y))
}

# returns an estimate for alpha based on 5.2 eq in book indexed by the index argument

# estimate alpha using all 100 observations
alpha.fn(Portfolio, 1:100)

# sample function to randomly select 100 observations from 1 to 100, with replacement
# then recompute alpha
set.seed(1)
alpha.fn(Portfolio, sample(100,100, replace = T))

# boot function automates the computation so we don't have to do above function 1000x
boot(Portfolio, alpha.fn, R = 1000)


# Estimating the Accuracy of a Linear Regression Model
boot.fn = function(data, index)
return(coef(lm(mpg~horsepower, data = data, subset = index)))  
boot.fn(Auto, 1:392)

# boot.fn can also be used to make bootstrap estimates
set.seed(1)
boot.fn(Auto, sample(392,392, replace = T))
# another sample
boot.fn(Auto, sample(392,392, replace = T))

# then compute standard errors of 1000 bootstrap estimates
boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower, data = Auto))$coef
# Bootstrap approach doesn't rely on assumption that 
# the Xi are fixed and variability comes from the variation in errors Ei

# using a quadratic approach, since data doesn't look linear:
boot.fn = function(data, index)
coefficients(lm(mpg~horsepower^2), data = data, subset = index)

set.seed(1)
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower+I(horsepower^2), data = Auto))$coef

