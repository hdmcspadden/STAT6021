## store data file with the variable name data
data<-read.table("purity.txt", header=TRUE ,sep="")

#=================

##make data the default data frame
attach(data)

#==================

##Fit a regression model
lm(purity~hydro)
result<-lm(purity~hydro)
summary(result)

#==================

##to obtain t multiplier for a 95% CI with df=18
tmulti <- qt(0.975,18) # qt for t value multiplier for 95% CI
print(tmulti)

#==================

##to obtain p-value from a 2-sided test
2*(1-pt(3.386,18)) # pt to get p value, use the t value from the summary(result)

#==================

##to produce 95% CIs for all regression coefficients
confint(result, level = 0.95) # added "hydro" to only get the confint for B1


#==============================

##to produce 95% CI for the mean response when x=1.2, and the 95% PI for the response of an observation when x=1.2
newdata<-data.frame(hydro=1.2)
print(newdata) # print newdata to understand what we get back

predict.lm(result,newdata,level=0.95, interval="confidence") # 95% confidence that the mean of purity for a hydro value of 1.2 is within this range


predict.lm(result,newdata,level=0.95, interval="prediction") # 95% confidence the specific value of purity for a hydro value of 1.2 is within the range. 

#===========================

##see what components we can extract from lm
names(result)

#============================

##extract the residuals from lm
result$residual

#=======================
mean(result$residual) # basically zero



