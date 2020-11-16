# Stat 6021: Homework Set 11

## H Diana McSpadden (hdm5s)

#q1b-1}
#setup

# get the data
mcData<-read.table("mcgill.txt", header=FALSE, sep="")

colnames(mcData)<-c("company","industry")
mcData
attach(mcData)

#q1b-2}
mcResult<-lm(company~industry)
summary(mcResult)

mcRes<-mcResult$residuals

##PACF plot of residuals
pacf(mcRes, main="PACF of Residuals")
# significant at lag 1


##fit an AR(1) model for residuals
# order = c(1,0,0) indicates the p value == 1
ar.1<-arima(mcRes, order = c(1,0,0), include.mean = FALSE)
ar.1

ar.1$coef

#q1b-3}
##transform response and predictor
shift<-ar.1$coef

#?lag

#create new column bind, shifts company variable by 1
y <- cbind(as.ts(company),lag(company))
yprime <- y[,2] - shift*y[,1]
y
yprime

x<-cbind(as.ts(industry),lag(industry))
xprime<-x[,2] - shift*x[,1]
x
xprime

##perform regression on transformed variables
result.prime<-lm(yprime~xprime)

result.prime

#q1b-4}
##residual plot
plot(result.prime$fitted.values,result.prime$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

##acf plot of residuals
acf(result.prime$residuals)

##qq plot of residuals
qqnorm(result.prime$residuals)

summary(result.prime)

# q4-1}

# setup
library(boot)

# get the data & attach
data<-read.table("nfl.txt", header=TRUE, sep="")
attach(data)


# myLOOCVLoop function
# inpuut: the dataset: dataframe, leaveOutIndex: the index to use as the test value
# output: the MSE for the requested loop of LOOCV 
myLOOCVLoop <- function(theData, leaveOutIndex) {
  
  # leave out the i-th value for testing
  test <- theData[leaveOutIndex, ]
  # keep rest of data for training
  train <- theData[-leaveOutIndex, ]
  
  # fit to training data
  glm.fit<-glm(y~x2+x7+x8, data=train)
  
  ## get predicted value from test data
  preds<-predict(glm.fit,newdata=test)
  
  #print(paste("Prediction: ", preds))
  #print(paste("Actual: ", test[1]))
  
  # calculate square of difference between predicted and actual
  difsquared = (preds - test[1])^2
  
  # return dif squared
  return(difsquared)
}

# I want to calculate: #mean((data$actual - data$pred)^2)
# create an accumulator variable
difSquaredAccummulator <- 0

# loop through all rows in data set
for (i in 1:nrow(data)) 
{ 
  # add to accumulator the dif Squared
  difSquaredAccummulator <- difSquaredAccummulator + myLOOCVLoop(data,i)
}

# get average of dif squared == MSE for LOOCV
mseLOOCV <- difSquaredAccummulator / nrow(data)

print(paste("My LOOCV: ", mseLOOCV))
qqline(result.prime$residuals, col="red")