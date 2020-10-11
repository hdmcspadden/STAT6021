data<-read.table("wine.txt", header=TRUE, sep="")
attach(data) # look at data

# notice that Region is numberic with 1,2,3
# 1 = North
# 2 = Central
# 3 = Napa Valley
is.numeric(Region)

##have R treat region as categorical
Region<-factor(Region) # this asks R to see Region as a factor/categorical
is.factor(Region) 

##check coding scheme
contrasts(Region) # this is the current coding scheme that R is using

levels(Region)

##Give names to the classes
levels(Region) <- c("North", "Central", "Napa")  # Dr. Woo HIGHLY recommends making names for classes in a category
contrasts(Region)

# now we have text labels for the classes

##Set a different reference class
Region<-relevel(Region, ref = "Napa") # now Napa will be the 0-0 class
contrasts(Region)

# Now we are setting Napa as our referene class. This was intuitive choice we Napa is understood as highest quality wine
# Napa is the one we want to compare to everything else. That is the easiest math.

##consider each region a subset
a1<-subset(data,Region=="1") 
a2<-subset(data,Region=="2") 
a3<-subset(data,Region=="3") 

##fit 3 separate regressions, one for each region
reg1<-lm(Quality~Flavor,data=a1)
reg2<-lm(Quality~Flavor,data=a2)
reg3<-lm(Quality~Flavor,data=a3)

##create a scatterplot with different colors and symbols for each region
plot(Flavor,Quality, main="Quality Rating against Flavor Rating, by Region")
points(a2$Flavor,a2$Quality, pch=2, col="red") 
points(a3$Flavor,a3$Quality, pch=12, col="blue")
abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
legend("topleft", c("North","Central","Napa"), lty=c(1,2,3), pch=c(1,2,12), col=c("black","red","blue")) 

##fit regression with interaction between the 2 predictors
result<-lm(Quality~Flavor*Region)
summary(result)

##fit regression with no interaction
reduced<-lm(Quality~Flavor+Region)

anova(reduced,result) # compare additive vs. interaction
# the anova shows that the sample could come from a population where the slopes are all the same.
# difference in slope could reasonably be because of the sample.


##residual plot of model with no interaction
plot(reduced$fitted.values,reduced$residuals,main="Residual plot")
abline(h=0,col="red")

##ACF plot of residuals
acf(reduced$residuals)

##QQ plot of residuals
qqnorm(reduced$residuals)
qqline(reduced$residuals, col="red")

##additional assumption to check with categorical predictor. 
# Is the variance of the response variable constant between all classes of the categorical predictor?
boxplot(Quality~Region, main="Boxplot of Quality Rating by Region")

# these look about the same. So we also want to run the hypothesis check.


##perform levene's test. Null states the variances are equal for all classes. 
# p value of .90 so we fail to reject that the variances are ~ 0
library(lawstat)
levene.test(Quality,Region)

# look at the coefficients for model
summary(reduced) # interpret as comparisons to Napa (reference class)


# compare all possible pairs of classes
##perform Tukey's multiple comparisons
library(multcomp)
pairwise<-glht(reduced, linfct = mcp(Region= "Tukey")) # glht = general linear hypothesis test
summary(pairwise)

reduced$coef

# The model is: y = B0 + B1x1 + B2I1 + B3I2 + E:

#So the regression functions are:

#North region: EfY g = B0 + B1x1 + B2(1) + B3(0) = (B0 + B2) + B1x1

#Central region: EfY g = B0 + B1x1 + B2(0) + B3(1) = (B0 + B3) + B1x1

#Napa region: EfY g = B0 + B1x1 + B2(0) + B3(0) = B0 + B1x1

#The difference in mean quality rating between the North and Napa regions is denoted by B2.
#The difference in mean quality rating between the Central and Napa regions is denoted by B3.
#The difference in mean quality rating between the North and Central regions is denoted by B2 - B3.


##obtain the variance-covariance matrix of the coefficients
vcov(reduced)

# the diagonals give the estimated variance of corresponding coefficient.
# E.g. the estimated variance of B0 is 1.020, so B0's standard error = sqrt(1.02)

# the off-diagonal give the estimated covariance of the corresponding coefficients
# E.g. the estimated covariance between B0 and B1 is -0.17
# E.g. the estimated covariance between B2 and B3 is 0.11


# Pairwise comparisons using Bonferroni Procedure
# compute the 95% family confidence intervals for the difference 
# in mean quality rating between wines in the
#1. North and Napa regions;
#2. Central and Napa regions;
#3. North and Central regions.

# multiplier for confidence interval is t((1-.05)/(2*g),n-p)
# g == number of pairwise comparisons we are making
# n = 38
# p = 4
deltaMultiplier <- qt((1 - (.05/6)), (38 - 4))
deltaMultiplier

# confidence interval = Bi-hat +- deltaMultiplier * se(Bi-hat)

#For B2
# B2-hat = -1.223366
# deltaMultiplier = 2.518259
# se(Bi-hat) is from the vcov() diagonal element for the class == sqrt(0.16)

# so the 95% CI for B2 is
lowB2CI <- -1.223366 - (2.518259 * sqrt(0.16))
highB2CI <- -1.223366 + (2.518259 * sqrt(0.16))

# so the 95% CI for B3 is
lowB3CI <- -2.757 - (2.518259 * sqrt(0.202))
highB3CI <- -2.757 + (2.518259 * sqrt(0.202))

# For 95% CI in mean quality rating between North and Central regions we use CI for B2 - B3
# So we need the estimated variance of B2 - B3:

# Var{B2-hat -  B3-hat} = Var(B2-hat) + Var(B3-hat) - 2Cov(B2-hat, B3-hat)
varB2B3 =  (0.160 + 0.202) - (2 * 0.113)
varB2B3

# so the 95% CI for difference in mean quality between North and Central is
# (B2-hat - B3-hat) +- deltaMultipler * (se(B2-hat - B3-hat))

difference <- (2.518259 * sqrt(varB2B3))
difference

difference2 <- 2.518 * (sqrt(0.136))
difference2

B2B3hat <- -1.223366 + 2.756851
B2B3hat

lowB2B3CI <- B2B3hat - difference2
highB2B3CI <- B2B3hat + difference2

lowB2B3CI
highB2B3CI
