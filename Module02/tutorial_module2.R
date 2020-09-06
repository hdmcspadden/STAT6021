## store data file with the variable name data
data<-read.table("purity.txt", header=TRUE ,sep="")

##make data the default data frame
attach(data)

##Fit a regression model
lm(purity~hydro)
result<-lm(purity~hydro)
summary(result)

##to obtain t multiplier for a 95% CI with df=18
qt(0.975,18)

##to obtain p-value from a 2-sided test
2*(1-pt(3.386,18))

##to produce 95% CIs for all regression coefficients
confint(result,level = 0.95)

##to produce 95% CI for the mean response when x=1.2, and the 95% PI for the response of an observation when x=1.2
newdata<-data.frame(hydro=1.2)
predict.lm(result,newdata,level=0.95, interval="confidence")
predict.lm(result,newdata,level=0.95, interval="prediction")

##see what components we can extract from lm
names(result)

##extract the residuals from lm
result$residual



