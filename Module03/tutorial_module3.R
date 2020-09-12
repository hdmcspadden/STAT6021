#-----

data <- read.table("windmill.txt", header=TRUE ,sep="")
attach(data)
head(data,5)

#------
result<-lm(output~wind)
summary(result)

result
#-----

##scatterplot of data, with least squares line overlayed
plot(wind, output, main="Plot of DC Output against Wind Velocity")
abline(result,col="red")

#----

##residual plot - tests mean = 0 for each xi
plot(result$fitted.values,result$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

## This looks BAD

#------

##ACF plot of residuals - tests variance of residuals
par(mfrow=c(2,1))
acf(result$residuals, main="ACF of Residuals")

## ACF Looks good

##Normal probability or QQ plot of residuals
qqnorm(result$residuals)
qqline(result$residuals, col="red")

## Normality looks good

#-----
##Transform

inv.wind<-1/wind
result.inv<-lm(output ~ inv.wind)

result.inv
#----

par(mfrow=c(2,2))
plot(inv.wind,output, main="Scatterplot after Transforming Predictor")
abline(result.inv,col="red")
plot(result.inv$fitted.values,result.inv$residuals, main="Plot of residuals against fits")
abline(h=0,col="red")
acf(result.inv$residuals, main="ACF of Residuals")
qqnorm(result.inv$residuals)
qqline(result.inv$residuals, col="red")

#----

summary(result.inv)

#----

##boxcox function found in MASS package. Need to install MASS package first

library(MASS)
boxcox(result.inv)
boxcox(result.inv, lambda = seq(0.6, 1.6, 0.01))

#----



