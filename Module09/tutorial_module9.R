##Example 1

data<-read.table("titanic.txt", header=TRUE, sep="")
attach(data)

result<-glm(Survived ~ Fare, family = "binomial") #generalized linear model, with binomial family, LR = Gaussian family

summary(result) # Z value is the Wald test

# for CI of coefficients use confint
confint(result, level=0.95)

#null deviance is the SSE of intercept only model. df = n - 1 == 951.08
# residual deviance is df = n - p, SSE of our model. smaller better


#consider fell model with 3 predictors
full<-glm(Survived ~ Fare + Sex + Age, family = "binomial")

summary(full)
# can see that gender matters
# fare matters
# age doesnt seem to matter

# notice null deviance stays same, residual dev goes down

# next, test whether all coefs are 0 == analogous with anova f test in linear regression setting
# in logistic regression we look at delta G ^2 statistic == difference between residual and null deviance. df = 3 
# H0 says all Bs 0
# Ha one coef is non zero.

1-pchisq(full$deviance,703)

1-pchisq(full$null.deviance-full$deviance,3)

1-pchisq(result$deviance-full$deviance,2) # if want to remove last two predictors == partial F test, df = 2 because removing 2 predictors. 
# Ha says at least one of 2 coefs is non zero. need more complicated model.

##Example 2
# grouped data
data2<-read.table("dose.txt", header=T)
attach(data2)

prop<-died/size # these are grouped. Size is the group with the particular dose. This is prorprtion died with each dose


# scatterplot of predictor vs log odds
# if straight line then logistic regression is appropriate
plot(logdose, log(prop/(1-prop)), xlab= "Log Dose", ylab="sample log odds")

result2<-glm(prop~logdose, family="binomial", weights=size) # fit model. The weight is the sample/group size
summary(result2)
# summary shows the coefficients are significantly different from 0

# we think because of the plot that the data may not fit well

# Pearson gives us Goodness of Fit
pearson<-residuals(result2,type="pearson")
X2<-sum(pearson^2)
X2 # test statistic is 28
1-pchisq(X2,9-2) # df = n-p, ths gives p vcalue

# this case rejects the null and says the data does not fit the model well.


1 - pchisq(result2$deviance, 7) # GOF deviance, pvalue says usual log regression model does not fit well.
