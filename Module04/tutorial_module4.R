library(Hmisc)

data <- read.table("delivery.txt", header=TRUE ,sep="")
attach(data)

head(data)
summary(data)
#describe(data)

result<-lm(Delivery~Number+Distance)

summary(result)

#qf(0.05,2,22) # this was original line of code
qf(0.95, 2, 22) # including this because in some later sections we do not get p value of F to compare, notice .95 for 95% test

confint(result,level = 0.95) # 95% confidence interval of the regression coefficients

newdata<-data.frame(Number=20, Distance=200) # create the new data point to run CIs of mean and predicted value

predict.lm(result, newdata, level=0.95, interval="confidence")
predict.lm(result, newdata, level=0.95, interval="prediction")

# a residual plot?
plot(result$fitted.values,result$residuals, main="MLR: Residuals vs. Fitted Values")
abline(h=0,col="red")


# ACF plot?
acf(result$residuals, main="MLR: ACF of Residuals")

# Normal probability or QQ plot of residuals
qqnorm(result$residuals)
qqline(result$residuals, col="red")

# Box BOx ??
library(MASS)
boxcox(result, lambda = seq(0, 1, 0.01))
