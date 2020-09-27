data <- read.table("mileage.txt", header=TRUE ,sep="")
attach(data)

# regress data set against 4 variables: 
# x1: displacement (in^3)
# x2: horsepower
# x6: barrels in carburetor
# x10: weight (lb)
# y = gas mileage
result<-lm(y~x1+x2+x6+x10)
summary(result)

reduced<-lm(y~x1)
anova(reduced,result)

anova(result)

# the anova comparison shows we fail to reject H0, so the simplier model is what we will keep. 
# the P value says that the additional variables' coefficients are all ~0.

((6.55+12.04+3.37)/3)/(259.86/27)

1-pf(0.7608,3,27)
qf(0.95,3,27)

# combine the vectors for the 4 predictors = 32 x 4 matrix
preds<-cbind(x1,x2,x6,x10)

preds
cor(preds) # correlation of the predictors

library(faraway)

#VIF functions = variance inflation factors for all predictors
vif(result)

# what do these tell us? By what factor the variance is multiplied by when 
# comparing between what we have, and when vectors are perfectly uncorrelated

# 3 of the vif values are pretty big


# if we don't have vif() how do we calculate vif
# regress the first predictor against the other three
result2<-lm(x1~x2+x6+x10)
summary(result2)

# see multiple R squared: .9496

1/(1-0.9496)
# == 19.84 == the vif of x1