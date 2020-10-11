# Assignment: Stat 6021: Homework Set 3
### Name: H. Diana McSpadden
### UID: hdm5s

#Attended a Study Group With **: Caprill Wright, Chelsea Alvarado, Arne Newman, Jing Huang, Abby Bernhardt, Brian Nam, Loren Bushkar. Katie Barbre**

#{r q1a-1}
# load the data
#?swiss

attach(swiss)

#{r q1a-2}
swissModel.Full <-lm(Fertility~Education + Catholic + Infant.Mortality + Agriculture + Examination) # I put agriculture and examination on the end
swissModel.Full # view the full model coefficients

swissModel.Reduced <-lm(Fertility ~ Education + Catholic + Infant.Mortality)
swissModel.Reduced # view the reduced coefficients


#{r q1a-3}

# run the anova for the reduced model vs full model
anova(swissModel.Reduced,swissModel.Full)

#{r q1a-4}
print(paste("Test Statistic: ",qf(0.95,2,41)))

#{r q1b-1

#par(mfrow=c(2,2)) # 2 rows, 2 columns - there will be three panes to look through

# Residual plot
plot(swissModel.Reduced$fitted.values,swissModel.Reduced$residuals, main="MLR: Residuals vs. Fitted Values")
abline(h=0,col="red")

# ACF Plot
acf(swissModel.Reduced$residuals, main="MLR: ACF of Residuals")

# QQ Plot of Residuals
qqnorm(swissModel.Reduced$residuals)
qqline(swissModel.Reduced$residuals, col="red")

## BoxCox
library(MASS)
boxcox(swissModel.Reduced, lambda = seq(-0.5, 2, 0.01))


#{r q2c-1}
# calculate that F statistic using SSR's and SSE
F0 = ((0.136 + 5.101 + 0.028) / 3) / (105.413 / 107)
print(paste("F0 is: ", F0))

# create the critical value
criticalFValue = qf(0.95,3,107)
print(paste("The critical value is: ", criticalFValue))

# create the pvalue
pvalue = 1-pf(F0,3,107)
print(paste("The p value for the F statistic is: ", pvalue))

#{r q2c-1}
# calculate that F statistic using SSR's and SSE when dropping x3 and x4
F0 = ((0.136 + 5.101) / 2) / (105.441 / 108)
print(paste("F0 is: ", F0))

# create the critical value
criticalFValue = qf(0.95,2,108)
print(paste("The critical value is: ", criticalFValue))

# create the pvalue
pvalue = 1-pf(F0,2,108)
print(paste("The p value for the F statistic is: ", pvalue))
