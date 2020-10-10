# Stat 6021: Homework Set 6

## Name: H. Diana McSpadden
### UID: hdm5s
### 10.10.2020

#q1a-1}
# attach the data
library(MASS)

attach(birthwt)
head(birthwt,5)

#q1a-2}
race<-factor(race) # this asks R to see race as categorical
is.factor(race) 

contrasts(race) 

# need to label the classes for race
levels(race) <- c("white", "black", "other")  # Dr. Woo HIGHLY recommends making names for classes in a category
contrasts(race)

#q1a-3}
# subset the data set by race
# need to use the original data values, not labels
dataW <- subset(birthwt,race=="1") 
dataB <- subset(birthwt,race=="2") 
dataO <- subset(birthwt,race=="3") 

##fit 3 separate regressions, one for each region
modW <- lm(bwt~age,data=dataW)
modB <- lm(bwt~age,data=dataB)
modO <- lm(bwt~age,data=dataO)

##create a scatterplot with different colors and symbols for each race
plot(age,bwt,main="Birth Weight vs Maternal Age, Grouped by Race")

points(dataW$age,dataW$bwt, pch=1, col="orange") 
points(dataB$age,dataB$bwt, pch=2, col="black") 
points(dataO$age,dataO$bwt, pch=12, col="purple")

abline(modW,lty=1, col="orange")
abline(modB,lty=2, col="black") 
abline(modO,lty=3, col="purple")

legend("topleft", c("White","Black","Other"), lty=c(1,2,3), pch=c(1,2,12), col=c("orange","black","purple")) 


#q1b-1}
modelInteractions <- lm(bwt~age*race)
summary(modelInteractions)


#q1b-2}

interceptBlack <- 2583.54 + 1022.79
interceptBlack

interceptOther <- 2583.54 + 326.05
interceptOther

slopeBlack <- 21.37 - 62.54
slopeBlack

slopeOther <- 21.37 - 26.03
slopeOther

#q2a}

averageAllRegions <- (26159 + 22894 + 22894) / 3
averageAllRegions

averageSpend <- (3901 + 3274 + 3919) / 3
averageSpend

#q3a-1}
MSRSpendArea <- 9720281 * 2
print(paste("Mean Squared Regression Spend_Area: ", MSRSpendArea))

#q3a-2}
# knowns
dfReducedModel <- 47
dfFullModel <- 45 #in the anova table, AND also 51 - the six coefficients in the E(y)
SSEReduced <- 232498501 + 9720281
SSEFull <- 232498501

# calculate that F statistic using SSR's and SSE
# page 272 I think gives the F0 calculation
F0 <- ((SSEReduced - SSEFull) / (dfReducedModel - dfFullModel)) / (SSEFull / dfFullModel)
print(paste("F0 is: ", F0))

# create the critical value
# page 272 gives the test statistic
criticalFValue <- qf(0.95,(dfReducedModel - dfFullModel),dfFullModel)
print(paste("The critical value is: ", criticalFValue))

pvalue <- 1-pf(F0,(dfReducedModel - dfFullModel),dfFullModel) # remember to use 1 - pf
print(paste("The p value for the F statistic is: ", pvalue))

B2hat <- 529.40 

#q3d-1}

# multiplier for confidence interval is t((1-.05)/(2*g),n-p)
# g == 3 == number of pairwise comparisons we are making
# n = 51
# p = 4 == parameters Intercept, Spend, AreaSouth, AreaWest
deltaMultiplier <- qt((1 - (.05/6)), (51 - 4))
print(paste("The 95% multiplier is: ", deltaMultiplier))

seB2hat <- sqrt(588126.71689) # sqrt(# from the vcov matrix), BUT also the same value in the Est. Std Error table for the coef.

# confidence interval = Bi-hat +- deltaMultiplier * se(Bi-hat)
B2CIlow <- B2hat - (deltaMultiplier * seB2hat)
B2CIhigh <- B2hat + (deltaMultiplier * seB2hat)

print(paste("The 95% CI for the mean difference in the mean teacher salary between the North region and the South region is: ", B2CIlow, " to ", B2CIhigh))

#q3d-2}
B3hat <- 1674.00

seB3hat <- sqrt(641873.8) # sqrt(# from the vcov matrix), BUT also the same value in the Est. Std Error table for the coef.

B3CIlow <- B3hat - (deltaMultiplier * seB3hat)
B3CIhigh <- B3hat + (deltaMultiplier * seB3hat)

print(paste("The 95% CI for the mean difference in the mean teacher salary between the North region and the West region is: ", B3CIlow, " to ", B3CIhigh))

#q3d-3}
B2B3hat <- B2hat - B3hat
#print(paste("B2 B3 hat:", B2B3hat))
#print(paste("t value: ", deltaMultiplier))


# above calculation may be wrong, also trying:
# Var(B2-hat) + Var(B3-hat) - 2Cov(B2,B3)
seB2B3hatv2 <- sqrt(588126.72 + 641873.8 - (2 * 244238.0)) # use the vcov table

B2B3CIlowv2 <- B2B3hat - (deltaMultiplier * seB2B3hatv2)
B2B3CIhighv2 <- B2B3hat + (deltaMultiplier * seB2B3hatv2)

print(paste("The 95% CI for the mean difference in the mean teacher salary between the South region and the West region is: ", B2B3CIlowv2, " to ", B2B3CIhighv2))


