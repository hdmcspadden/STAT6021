# Stat 6021: Homework Set 7


## Name: H. Diana McSpadden
### UID: hdm5s
### 10.15.2020

#q0-1}
# attach the data
library(MASS)

attach(birthwt)
head(birthwt,5)

#q1a-1}

low<-factor(low) #ensure low is a factor

race<-factor(race)#ensure race is a factor
levels(race) <- c("white", "black", "other")
contrasts(race)

smoke<-factor(smoke) #ensure smoke is a factor
ht<-factor(ht) #ensure ht is a factor
ui<-factor(ui) #ensure ui is a factor

head(birthwt)

#q1c-1}
# add the leaps library
library(leaps)

# create all the regsubsets
# set nbest to 5
allreg <- regsubsets(bwt~age+lwt+race+smoke+ptl+ht+ui+ftv, data=birthwt, nbest=5) # use all variables except low

#q1c-3}
##create a "data frame" that stores the predictors in the various models considered as well as their various criteria
best <- as.data.frame(summary(allreg)$outmat)

best$adjr2 <- summary(allreg)$adjr2
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$mse <- (summary(allreg)$rss)/(dim(birthwt)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic

##sort by various criteria
best[order(best$adjr2),] # want largest
best[order(best$mse),] # want smallest
best[order(best$cp),] # want smallest
best[order(best$bic),] # want smallest

#q1d-1}
# only the intercept
regnull <- lm(bwt~1, data=birthwt) # intercept only/empty model == ENDING POINT for backward

#modelAdd <- lm(bwt~age+race)
#summary(modelAdd)

# model with all predictors (except low)
regfull <- lm(bwt~age+race+lwt+smoke+ptl+ht+ui+ftv, data=birthwt) # full model == END POINT

step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")

#q1d-2}
modelSelected <- lm(bwt~race+lwt+smoke+ht+ui)
summary(modelSelected)

#q4}
#This is a very literal version of the formula
calculatePRESS <- function(theModel) {
  
  sumPRESS = 0
  hatDiagonals <- lm.influence(theModel)$hat
  
  for (i in 1:length(hatDiagonals)){
    sumPRESS <- sumPRESS + ((theModel$residuals[i]) / (1 - hatDiagonals[i]))^2
  }
  
  return(sumPRESS)
}

# This used the built-in array functions in R
press <- function(theModel) {
  
  pr <- theModel$residuals / (1-lm.influence(theModel)$hat)
  
  return (sum(pr^2))
}