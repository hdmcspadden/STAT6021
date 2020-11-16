##Ridge

##install.packages("glmnet")

library(glmnet)

##model.matrix automatically transform categorical variables into dummy codes, which is needed as the glmnet function cannot handle categorical variables
x<-model.matrix(mpg~.,mtcars)[,-1]
y<-mtcars$mpg

##Note some predictors are highly correlated with each other. 
pairs(x, lower.panel=NULL, main="Scatterplots of Predictors")

##alpha=0 for ridge, alpha=1 for LASSO
##threshold value should be very small if multicollinearity is present. see what happens if thresh was set to a larger value
##we know theoretically the coeffs should be the same as lm when lambda is 0
ridge.r<-glmnet(x,y,alpha=0, lambda=0, thresh = 1e-14)
coefficients(ridge.r)

##MLR
result<-lm(mpg~.,mtcars)
summary(result)

##split data
set.seed(12)
train<-sample(1:nrow(x), nrow(x)/2)
test<-(-train)
y.test<-y[test]

##use CV to find optimal lambda based on training set
set.seed(12)
cv.out<-cv.glmnet(x[train,],y[train],alpha=0)
bestlam<-cv.out$lambda.min
bestlam
plot(cv.out)

##fit ridge regression using training data
ridge.mod<-glmnet(x[train,],y[train],alpha=0,lambda=bestlam, thresh = 1e-14)

##Test MSE with best lambda
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

##fit OLS by setting lambda=0
ridge.mod.0<-glmnet(x[train,],y[train],alpha=0,lambda=0, thresh = 1e-14)

##test MSE with lambda=0
ridge.pred.0<-predict(ridge.mod.0,newx=x[test,])
mean((ridge.pred.0-y.test)^2)

##Compare ridge with OLS using best lambda and all observations
out.ridge<-glmnet(x,y,alpha=0,lambda=bestlam,thresh = 1e-14)
out.ols<-glmnet(x,y,alpha=0, lambda=0, thresh = 1e-14)
cbind(coefficients(out.ridge), coefficients(out.ols))

##Create plot of ridge coeff against lambda
grid<-10^seq(10,-2,length=100)
out.all<-glmnet(x,y,alpha=0,lambda=grid,thresh = 1e-14)
plot(out.all, xvar = "lambda")
abline(v=log(bestlam), lty=2)
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)


