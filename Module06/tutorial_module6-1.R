data<-read.table("wine.txt", header=TRUE, sep="")
attach(data)

is.numeric(Region)

##have R treat region as categorical
Region<-factor(Region) 
is.factor(Region) 

##check coding scheme
contrasts(Region)

levels(Region)

##Give names to the classes
levels(Region) <- c("North", "Central", "Napa") 
contrasts(Region)

##Set a different reference class
Region<-relevel(Region, ref = "Napa") 
contrasts(Region)

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
anova(reduced,result)

##residual plot of model with no interaction
plot(reduced$fitted.values,reduced$residuals,main="Residual plot")
abline(h=0,col="red")

##ACF plot of residuals
acf(reduced$residuals)

##QQ plot of residuals
qqnorm(reduced$residuals)
qqline(reduced$residuals, col="red")

##additional assumption to check with categorical predictor. Is the variance of the response variable constant between all classes of the categorical predictor?
boxplot(Quality~Region, main="Boxplot of Quality Rating by Region")

##perform levene's test. Null states the variances are equal for all classes. 
library(lawstat)
levene.test(Quality,Region)

summary(reduced)

##perform Tukey's multiple comparisons
library(multcomp)
pairwise<-glht(reduced, linfct = mcp(Region= "Tukey"))
summary(pairwise)

reduced$coef

##obtain the variance-covariance matrix of the coefficients
vcov(reduced)
