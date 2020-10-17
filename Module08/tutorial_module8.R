data<-read.table("bp.txt", header=TRUE, sep="")
attach(data)

result<-lm(BP~weight)

##residuals
res<-result$residuals # regular residuals

##studentized residuals
student.res<-rstandard(result) # rstandard give studentized residuals == ri

##externally studentized residuals
ext.student.res<-rstudent(result) # externally studentized 

#par(mfrow=c(1,3))
plot(result$fitted.values,res,main="Residuals")
plot(result$fitted.values,student.res,main="Studentized Residuals")
plot(result$fitted.values,ext.student.res,main="Externally  Studentized Residuals")

# NOTICE the scaling of the various plots

# we will use the externally studentized residuals to detect outliers

n<-length(BP)
p<-2

##critical value using Bonferroni procedure
qt(1-0.05/(2*n), n-p-1)

sort(ext.student.res)

plot(ext.student.res,main="Externally Studentized Residuals", ylim=c(-4,4))
abline(h=qt(1-0.05/(2*n), n-p-1), col="red")
abline(h=-qt(1-0.05/(2*n), n-p-1), col="red")

ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)] # numberic(0) = none of these have that characteristics

# example of re-writing so that we get some results where externally studentized residual > 1
ext.student.res[abs(ext.student.res)>1] 

##leverages
lev<-lm.influence(result)$hat

sort(lev)
2*p/n

# notice we have two points with leverages over 2*p/n

plot(lev, main="Leverages", ylim=c(0,0.4))
abline(h=2*p/n, col="red")

##identify data points on plot
identify(lev)

lev[lev>2*p/n]

##influential observations
DFFITS<-dffits(result)
DFFITS[abs(DFFITS)>2*sqrt(p/n)] # how drastically fitted value changes with and without the presence of the observation


#DFBETAS - tells me estimated B0 changes by magnituide of 0.45, B1 changes by magnitude 0.52
DFBETAS<-dfbetas(result) # measures how estimated coefficients change without presence of the observation
DFBETAS[abs(DFBETAS)>2/sqrt(n)]
# how to find the data point
DFBETAS # can find that value with eyeballing

# COOKS considered how fitted values changes for all values (not just the observation) when data point is removed.
COOKS<-cooks.distance(result)
COOKS[COOKS>qf(0.5,p,n-p)] # f distribution
# shows no data points are an outlier. Data point 8 affects itself, but not the other data points