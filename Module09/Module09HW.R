# Stat 6021: Homework Set 9

## H. Diana McSpadden
## UID: hdm52
## Date: 10/30/2020

# q1-setup1}

library(dplyr)

insectsData <- read.table("insects.txt", header=FALSE, sep="")

# q1-setup2}

insectsData <- insectsData %>% rename(logdose = V1, size = V2, died = V3)

#names(insectsData)

#q1-setup3}

head(insectsData)
attach(insectsData)

#knowns
n <- 1500
groupn <- 250
groups  <- 6

# q1a-1}
prop<-died/size

plot(logdose, log(prop/(1-prop)), xlab= "Log Dose", ylab="sample log odds")

# q1b-1}
modelInsects <- glm(prop~logdose, family="binomial", weights=size) # fit model. The weight is the sample/group size
summary(modelInsects)

# q1c-1}
exp(0.67)

# q1d-1}
x_logdose <- 2

oddsOfDeath <- exp(-2.64 + (0.67 * x_logdose))
print(paste("Odds of Death: ",oddsOfDeath))

# q1e-1}
probOfDeathStart <- exp(-2.64 + (0.67 * x_logdose)) / ( 1 + exp(-2.64 + (0.67 * x_logdose)))

probDeath <- oddsOfDeath / (1 + oddsOfDeath)
print(paste("Probability of Death: ",propDeath))

# q1f-1}
#Pearsons
pearson<-residuals(modelInsects,type="pearson")
X2<-sum(pearson^2)
X2 # test statistic is 1.45
print(paste("Pearson test statistic",X2))

n= 6
p=2

pValue <- 1-pchisq(X2,n-p) # df = n-p, this gives p value
print(paste("pvalue: ",pValue))

# q1f-2}
1 - pchisq(modelInsects$deviance, n-p) 

# q2a-1}

exp(0.43397)

# q2b-1}
Bjhat <- 0.43397
seBjhat <- 0.52179

wTestStatistic <- Bjhat / seBjhat
print(paste("Wald test statistic for B3: ", wTestStatistic))

#compare to pnorm
wPValue <- (1-pnorm(wTestStatistic))*2
print(paste("Wald p value for B3: ", wPValue))

# q2c-1}

zValue <- 1.96 # for 95% CI

CILow <- Bjhat - (zValue * seBjhat)

CIHigh <- Bjhat + (zValue * seBjhat)

print(paste("The 95% CI for B3 is: ", CILow, " - ", CIHigh))

print(paste("Low odds ratio: ", exp(CILow)))

print(paste("High odds ratio: ", exp(CIHigh)))

# q2e}
partialDeviance <- 113.2

fullDeviance <- 105.09

df <- 2 # considering removing 2 predictors

deltaG2 <- partialDeviance - fullDeviance

1-pchisq(deltaG2, df)

qchisq(.025, df) # critical value

# q2f-1}

x_age <- 70
x_gender <- 1
x_awareness <- 65

linearPart <- -1.17716 + (0.07279 * x_age) - (0.09899 * x_awareness) + (0.43397 * x_gender)
print(paste("log odds: ", linearPart))

print(paste("odds: ", exp(linearPart)))

Ey1 <- exp(linearPart) / (1 + exp(linearPart))
Ey1 #prob

print(paste("prob: ", Ey1))

# q2g-1}

exp(0.07279)

# q2g-2}
x_age <- 71
x_gender <- 1
x_awareness <- 65

linearPart <- -1.17716 + (0.07279 * x_age) - (0.09899 * x_awareness) + (0.43397 * x_gender)
exp(linearPart)
