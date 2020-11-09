
# Stat 6021: Homework Set 9

## H. Diana McSpadden
## UID: hdm52
## Date: 11/06/2020

#q1-setup1}
library(MASS)
library(Hmisc)
library(dplyr)
library(nnet)
library(ROCR)

#q1a-1}

#?Boston


# even though we dont need it, here is the summary
aboutBoston <- summary(Boston$crim)
aboutBoston

Boston <- Boston %>%
  mutate(crimHigh = (crim >= 1.0) * 1)

#q1a-2}

Boston

attach(Boston)

#q1a-3}
is.factor(crimHigh)

##tell R to treat this variable as categorical
crimHigh<-factor(crimHigh)

is.factor(crimHigh)

levels(crimHigh)<-c("low","high")
levels(crimHigh)

contrasts(crimHigh)

#q1b-1}
detach(Boston) #detach Boston so variable names aren't a mess

set.seed(199) #set seed

sample<-sample.int(nrow(Boston), floor(.50*nrow(Boston)), replace = F) # get random sample to rows
train<-Boston[sample, ]
test<-Boston[-sample, ]


#q1b-2}
result<-glm(crimHigh ~ indus + nox + rad + tax + lstat + medv, family = "binomial", data=train)

#q1b-3}
##predicted prob of high crime vs. low crime rate for testing data based on training data
preds<-predict(result,newdata=test, type="response") # need to use type=response for probabilities.

##produce the numbers associated with classification table
rates<-prediction(preds, test$crimHigh) #did it correctly identify CHD diagnosis?

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

#q1b-4}
##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for High Crime")
lines(x = c(0,1), y = c(0,1), col="red")
lines(x = c(0.08,0.08), y = c(0,1), col="blue")
lines(x = c(0,1), y = c(0.93,0.93), col="blue")


#q1c-1}
##compute the AUC
aucvalues <- performance(rates, measure = "auc")
auc <- aucvalues@y.values[[1]]
auc

#q1d-1}
table(test$crimHigh, preds>0.5)

#q1d-1}

trueNeg <- 164
falseNeg <- 11
truePos <- 75
falsePos <- 3

# here are all my calculations
overallErrorRate <- (falseNeg + falsePos) / (falseNeg + falsePos + trueNeg + truePos)
print(paste("Overall Error Rate: ",overallErrorRate))

falsePosRate <- falsePos / (trueNeg + falsePos)
print(paste("False Positive Rate: ",falsePosRate))


falseNegRate <- falseNeg / (falseNeg + truePos)
print(paste("False Negative Rate: ",falseNegRate))

#sensitivity <- 1 - falseNegRate
#sensitivity

#specifiity <- 1 - falsePosRate
#specifiity

#q2c-1}

#preTermNutritionZ <- -0.065 / 0.018
#preTermNutritionP <- (1 - pnorm(abs(preTermNutritionZ)))*2
#print(paste("Preterm Nutrition Z Score: ", preTermNutritionZ))
#print(paste("Preterm Nutrition P Value: ", preTermNutritionP))

#preTermLess20Z <- 2.96 / .096
#preTermLess20P <- (1 - pnorm(abs(preTermLess20Z)))*2
#print(paste("Preterm Less 20 Z Score: ", preTermLess20Z))
#print(paste("Preterm Less 20 P Value: ", preTermLess20P))

#preTermGreater30Z <- 2.06 / 0.89
#preTermGreater30P <- (1 - pnorm(abs(preTermGreater30Z)))*2
#print(paste("Preterm Greater 30 Z Score: ", preTermGreater30Z))
#print(paste("Preterm Greater 30 P Value: ", preTermGreater30P))

preTermAlcoholZ <- 2.04 / 0.71
preTermAlcoholP <- (1 - pnorm(abs(preTermAlcoholZ)))*2
print(paste("Preterm Alcohol Z Score: ", preTermAlcoholZ))
print(paste("Preterm Alcohol P Value: ", preTermAlcoholP))

#preTermSmokingZ <- 2.45 / 0.73
#preTermSmokingP <- (1 - pnorm(abs(preTermSmokingZ)))*2
#print(paste("Preterm Smoking Z Score: ", preTermSmokingZ))
#print(paste("Preterm Smoking P Value: ", preTermSmokingP))

#intNutritionZ <- -0.046 / 0.015
#intNutritionP <- (1 - pnorm(abs(intNutritionZ)))*2
#print(paste("Intermediate Nutrition Z Score: ", intNutritionZ))
#print(paste("Intermediate Nutrition P Value: ", intNutritionP))

#intLess20Z <- 2.91 / 0.86
#intLess20P <- (1 - pnorm(abs(intLess20Z)))*2
#print(paste("Intermediate Less 20 Z Score: ", intLess20Z))
#print(paste("Intermediate Less 20 P Value: ", intLess20P))

#intGreater30Z <- 1.89 / 0.81
#intGreater30P <- (1 - pnorm(abs(intGreater30Z)))*2
#print(paste("Intermediate Greater 30 Z Score: ", intGreater30Z))
#print(paste("Intermediate Greater 30 P Value: ", intGreater30P))

intAlcoholZ <- 1.07 / 0.65
intAlcoholP <- (1 - pnorm(abs(intAlcoholZ)))*2
print(paste("Intermediate Alcohol Z Score: ", intAlcoholZ))
print(paste("Intermediate Alcohol P Value: ", intAlcoholP))

#intSmokingZ <- 2.23 / 0.67
#intSmokingP <- (1 - pnorm(abs(intSmokingZ)))*2
#print(paste("Intermediate Smoking Z Score: ", intSmokingZ))
#print(paste("Intermediate Smoking P Value: ", intSmokingP))

#q2c-1}

preBhat <- 2.0429
preBse <- 0.7097461

preciLow <- preBhat - (1.96 * preBse)
preciHigh <- preBhat + (1.96 * preBse)

print(paste("preterm vs full term alcohol CI: ",preciLow, " - ",preciHigh))
print(paste("EXP preterm vs full term alcohol CI: ",exp(preciLow), " - ",exp(preciHigh)))


intBhat <- 1.067001
intBse <- 0.6495262

intciLow <- intBhat - (1.96 * intBse)
intciHigh <- intBhat + (1.96 * intBse)

print(paste("intermediate vs full term alcohol CI: ",intciLow, " - ",intciHigh))
print(paste("EXP intermediate vs full term alcohol CI: ",exp(intciLow), " - ",exp(intciHigh)))

