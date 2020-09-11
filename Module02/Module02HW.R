#'''
#Exercise: Stat 6021: Homework Set 1
#Name: H. Diana McSpadden
#UID: hdm5s
#'''

# QUESTION 1
# q1a Script

#getwd()
copierData <- read.table('copier.txt', sep='\t', header = TRUE)
head(copierData,5)
attach(copierData)

#use plot(...) to create a scatterplot
plot(x=Serviced, y=Minutes, main='Plot Minutes Against Number Copiers Serviced', xlab = 'Number of Copiers Serviced', ylab = 'Service Time (minutes)')


# q1b Script
corvalue <- cor(x=Serviced,y=Minutes) 
print(paste("Correlation between Service Time and Number of Copiers: ", corvalue))

# q1d Script
# create a linear model
copierModel <- lm(Minutes~Serviced)
copierModel

#summary(copierModel)

# get the 95% CI for the Serviced coef
CIB1 = confint(copierModel, "Serviced", level = 0.95)

print(paste("95% CI of slope: ", CIB1[1], " - ", CIB1[2]))


# q1e Script
## 95% CI for response when x=5
newdata<-data.frame(Serviced=5)
#print(newdata) # print newdata to understand what we get back

prediction = predict.lm(copierModel,newdata,level=0.95, interval="prediction")
#prediction

print(paste("The 95% CI for service minutes for a service call for 5 copiers is: ", prediction[2], " - ", prediction[3], " minutes"))


# q1f Script
# get all the residuals from the model
copierResiduals <- copierModel$residual
#copierResiduals

# get the first residual
print(paste("The value of the first residual is: ", copierResiduals[1]))

# q1g Script
meanCopierResidual = mean(copierModel$residual)

print(paste("The mean of the residuals is: ", meanCopierResidual, " minutes."))