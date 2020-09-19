# Assignment: Stat 6021: Homework Set 3
### Name: H. Diana McSpadden
### UID: hdm5s

#Attended a Study Group With**: David Fuentes, Jing Huang, Caprill Wright, Arne Newman, Michael Kastanowski, Abby Bernhardt, Loren Bushkar

### References
#In addition to the textbook and Module 3, I also read: https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/

library(faraway)
#summary(cornnit)

head(cornnit, 5)
attach(cornnit)

cornnit

#q2ascatterplot
# create a model
cornModel <- lm(yield~nitrogen)

#plot the model and the linear model
plot(x=nitrogen, y=yield, main='Plot Corn Yield Against Nitrogen', xlab = 'Nitrogen (lbs per acre)', ylab = 'Yield (bushels per acre')
abline(cornModel,col="red")

#q2bregressionplot
plot(cornModel$fitted.values,cornModel$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

#q2cBoxCox
library(MASS)
#boxcox(cornModel)
boxcox(cornModel, lambda = seq(1.4, 4.5, 0.01))


#q2dTransform1}
# y' = y^2
yield2 <- yield^2
cornModel.yield2 <-lm(yield2~nitrogen)

par(mfrow=c(1,2)) # 1 rows, 2 columns - there will be three panes to look through

# scatter plot transformed y
plot(x=nitrogen, y=yield2, main='Scatter Plot y^2 Trans', xlab = 'Nitrogen (lbs per acre)', ylab = 'Yield^2')
abline(cornModel.yield2,col="red")

#residual plot of transformed y
plot(cornModel.yield2$fitted.values,cornModel.yield2$residuals, main="Residual Plot: y^2")
abline(h=0,col="red")

# boxcox of y^2 model
boxcox(cornModel.yield2, lambda = seq(0.5, 2.5, 0.01))

#QQ plot
qqnorm(cornModel.yield2$residuals) 
qqline(cornModel.yield2$residuals, col="red")

#ACF plot
acf(cornModel.yield2$residuals, main="ACF of Residuals: y^2")

#qdTransformation2
#x' = sqrt(x)

sqrt.nitrogen = sqrt(nitrogen)


cornModel.yield2.sqrtx <-lm(yield2~sqrt.nitrogen)


par(mfrow=c(1,2)) # 1 rows, 2 columns - there will be three panes to look through

# scatter plot yield2, sqrt(x)
plot(x=sqrt.nitrogen, y=yield2, main='Scatter Plot y^2 sqrt(x)', xlab = 'sqrt(nitrogen)', ylab = 'Yield^2')
abline(cornModel.yield2.sqrtx,col="red")

#residual plot yield2, sqrt(x)
plot(cornModel.yield2.sqrtx$fitted.values,cornModel.yield2.sqrtx$residuals, main="Residual Plot: y^2 sqrt(x)")
abline(h=0,col="red")

# boxcox of y^2 model, sqrt(x)
boxcox(cornModel.yield2.sqrtx, lambda = seq(0.5, 2.5, 0.01))

#QQ plot of y^2 model, sqrt(x)
qqnorm(cornModel.yield2.sqrtx$residuals) 
qqline(cornModel.yield2.sqrtx$residuals, col="red")

#ACF plot of y^2 model, sqrt(x)
acf(cornModel.yield2.sqrtx$residuals, main="ACF of Residuals: y^2 sqrt(x)")

#q2dRemovex=0}
#head(cornnit)
cornnitFiltered = cornnit[cornnit[, "nitrogen"]>0, ]


#q2dWorkWithFiltered}
detach(cornnit)
attach(cornnitFiltered)

cornModel.filtered <-lm(yield~nitrogen)

par(mfrow=c(1,2)) # 1 rows, 2 columns - there will be three panes to look through

# scatter plot filtered model
plot(x=nitrogen, y=yield, main='Scatter Plot Filtered', xlab = 'nitrogen', ylab = 'yield')
abline(cornModel.filtered,col="red")

#residual plot filtered model
plot(cornModel.filtered$fitted.values,cornModel.filtered$residuals, main="Residual Plot: Filtered")
abline(h=0,col="red")

# boxcox of filtered model
boxcox(cornModel.filtered, lambda = seq(-0.5, 2.5, 0.01))

#QQ plot of filtered model
qqnorm(cornModel.filtered$residuals) 
qqline(cornModel.filtered$residuals, col="red")

#ACF plot
acf(cornModel.filtered$residuals, main="ACF of Residuals: Filtered")

#q2dAnovaFiltered}
anova(cornModel.filtered)

#q2dFilteredTransformations}
yield.t.filtered = 1/yield

cornModel.t.filtered <-lm(yield.t.filtered~nitrogen)

par(mfrow=c(1,2)) # 1 rows, 2 columns - there will be three panes to look through

# scatter plot filtered transformed
plot(x=nitrogen, y=yield.t.filtered, main='Scatter Plot Filtered 1/y', xlab = 'nitrogen', ylab = '1/y')
abline(cornModel.t.filtered,col="red")

#residual plot filtered transformed
plot(cornModel.t.filtered$fitted.values,cornModel.t.filtered$residuals, main="Residual Plot: filtered 1/y")
abline(h=0,col="red")

#QQ plot of filtered transformed
qqnorm(cornModel.t.filtered$residuals) 
qqline(cornModel.t.filtered$residuals, col="red")

#ACF plot
acf(cornModel.t.filtered$residuals, main="ACF of Residuals: Filtered 1/y")

anova(cornModel.t.filtered)

#q2dsummaryAnovaFiltered}
summary(cornModel.t.filtered)
anova(cornModel.t.filtered)

#q2dsummaryAnovaUnfiltered}
summary(cornModel.yield2.sqrtx)
anova(cornModel.yield2.sqrtx)

#q3c1}
(exp(-0.44993) - 1) * 100

#q3c2}
exp(1.50792)
