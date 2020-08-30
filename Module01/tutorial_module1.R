##read data file in
getwd()

read.table("purity.txt", header=TRUE ,sep="")

## store data file with the variable name data
data<-read.table("purity.txt", header=TRUE ,sep="")

##list the components in your data file
names(data)

##call the variable hydro
data$hydro


##make data the default data frame
attach(data)
hydro
detach(data)
hydro
attach(data)

##create a scatterplot of purity (y) against hydro (x)
plot(hydro,purity)
plot(purity~hydro)

##label axes and provide a title
plot(hydro,purity,xlab="Percent of Hydrocarbons", ylab="Purity of Oxygen", main="Plot of Purity of Oxygen against Percent of Hydrocarbons")

##split the plotting device so plots are displayed in a 1 by 2 matrix
par(mfrow=c(1,2))
plot(hydro, main="Plot of Percent of Hydrocarbons for each Sample")
plot(purity, main="Plot of Purity of Oxygen for each Sample")

##save the plot to a jpg file
jpeg("joint.jpg")
par(mfrow=c(1,2))
plot(hydro, main="Plot of Percent of Hydrocarbons for each Sample")
plot(purity, main="Plot of Purity of Oxygen for each Sample")
dev.off()

##Fit a regression model
lm(purity~hydro)
result<-lm(purity~hydro)
summary(result)


