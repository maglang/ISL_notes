require(ISLR)
require(boot)
??cv.glm
plot(mpg~horsepower,data=Auto)

## LOOCV
glm.fit=glm(mpg~horsepower, data=Auto)
cv.glm(Auto,glm.fit)$delta #pretty slow (doesnt use formula (5.2) on page 180)

##Lets write a simple function to use formula (5.2)
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)


cv.error=rep(0,5)
degree=1:5
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

## 10-fold CV

cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")


## Bootstrap
## Minimum risk investment - Section 5.2

alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X,Portfolio$Y)

## What is the standard error of alpha?

alpha.fn=function(data, index){
  with(data[index,],alpha(X,Y))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn (Portfolio,sample(1:100,100,replace=TRUE))

boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)


# Exercise
file.exists('Study/STAT/ISL on edX/ISL_notes/R Sessions/5.RData')
dim(data)
load('Study/STAT/ISL on edX/ISL_notes/R Sessions/5.RData')
ls()
X
x
y
Xy

glm.fit=glm(y~X1+X2, data=Xy)
cv.glm(Xy,glm.fit)$delta #pretty slow (doesnt use formula (5.2) on page 180)

# Standard Error of B1
summary(lm(y~.,data=Xy))

# Plot data
matplot(Xy,type="l")

# Bootstrap to estimate SE of B1

## What is the standard error of alpha?

x<-rnorm(100)
meanFunc <- function(X,i){mean(X[i])}
bootMean <- boot(Xy$X1,meanFunc,100)

boot.out=boot(Xy$X1,meanFunc,R=1000)
boot.out
plot(boot.out)

new.rows = c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)
new.Xy = Xy[new.rows, ]

 ### 
alpha.fn=function(data, index){
  with(Xy[index,],alpha(X1,y))
}

alpha.fn(Xy$X1,1:100)

set.seed(1)
alpha.fn (Xy,sample(1:100,100,replace=TRUE))

boot.out=boot(Xy,alpha.fn,R=1000)
boot.out
plot(boot.out)
