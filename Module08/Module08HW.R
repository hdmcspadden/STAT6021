# Stat 6021: Homework Set 8

## H. Diana McSpadden
## UID: hdm52
## Date: 10/22/2020

#**Attended group with**: Wright, McSpadden, Nam, Alvarado, Chivaluri, Barbre, Bernhardt, Bushkar


#prepareModel}
library(ggplot2)

attach(swiss)
#?swiss

swissModel <- lm(Fertility ~ Education + Catholic + Infant.Mortality)

#q1a-1}

##residuals
#regResiduals <- swissModel$residuals # regular residuals

##studentized residuals
#studentResiduals <- rstandard(swissModel) # rstandard give studentized residuals == ri

##externally studentized residuals
extStudentResiduals <- rstudent(swissModel) # externally studentized 

#q1a-2}
n <- nrow(swiss)
p <- 4
##critical value using Bonferroni procedure
criticalValue <- qt(1-(0.05/(2*n)), n-p-1)
criticalValue

sort(extStudentResiduals)
plot.new()
plot(extStudentResiduals,main="Externally Studentized Residuals", ylim=c(-4,4))
abline(h=criticalValue, col="red")
abline(h=-criticalValue, col="red")

extStudentResiduals[abs(extStudentResiduals)>criticalValue]

#q1b-1}
lev <- lm.influence(swissModel)$hat

#q1b-2}

comparisonValue <- 2*p/n
comparisonValue

sort(lev)

#q1b-3}

plot(lev, main="Leverages", ylim=c(0,0.5))
abline(h=2*p/n, col="red")

#q1c-1}

DFFITS <- dffits(swissModel)
DFFITS[abs(DFFITS)>2*sqrt(p/n)] # how drastically fitted value changes with and without the presence of the observation

#DFBETAS - 
DFBETAS <- dfbetas(swissModel) # measures how estimated coefficients change without presence of the observation
DFBETAS[abs(DFBETAS)>2/sqrt(n)]
# how to find the data point
DFBETAS # can find that value with eyeballing

# COOKS considered how fitted values changes for all values (not just the observation) when data point is removed.
COOKS<-cooks.distance(swissModel)
COOKS[COOKS>qf(0.5,p,n-p)] # f distribution

#q2a-1}

# Knowns
n <- 19
p <- 2
MSE <- 40.13^2
ei <- 120.829070
hii <- 0.23960510

Si2 <- (((n-p) * MSE) - (ei^2/ (1 - hii))) / (n - p - 1)
print(paste("Sihat^2: ", Si2))

ti <- ei / sqrt((Si2 * (1-hii)))
print(paste("ti: ", ti))

#q2a-2}
criticalValue <- qt(1-(0.05/(2*n)), n-p-1)
print(paste("critical value for comparison: ", criticalValue))

#q2b-1}

leverageComparison <- (2 * p) / n
leverageComparison

#q2c-1}
#hii
#ti
DFFITSi6 <- (sqrt(hii / (1 - hii)) * ti)

print(paste("DFFITSi for observation 6: ", DFFITSi6))

#q2d-1}
#ei
#MSE
ri = ei / (sqrt(MSE * (1 - hii)))
#ri

Di = (ri^2 / p) * (hii / (1 - hii))

criticalValueCooks = qf(0.5,p,n-p)

print(paste("Cook's Distance for Observation 6: ", Di))
print(paste("Cook's Distance F Comparison: ", criticalValueCooks))