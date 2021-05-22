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

