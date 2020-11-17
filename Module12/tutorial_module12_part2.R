
## Use principal Component Analysis on the Quant variables

names(mtcars)
?mtcars

##perform PCA, with scaling.
# scaling makes the units the same. Just do every time
pr.out<-prcomp(mtcars[,c(-8,-9)], scale=TRUE)

##extract the mean and SD of each variable
pr.out$center
pr.out$scale

##same result if you calculated their means and SDs on your own - checking pr function
apply(mtcars[,c(-8,-9)], 2, mean)
apply(mtcars[,c(-8,-9)], 2, sd)

##obtain the loading vector for the PCs
# Look at first two and see how we could interpret first two PC's
pr.out$rotation

##SD of each PC
# sum up and see what proportion of variance is explained by each
pr.out$sdev

##variance of each PC
pr.var<-pr.out$sdev^2
pr.var

##proportion of variance in data explained by each PC
# first and second account for 85% variance
pve<-pr.var/sum(pr.var)
pve

##plot of first two PCs
biplot(pr.out, scale=0)

##Scree plot
# plots proportion of variance each component accounts for
plot(pve, ylim=c(0,1))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", main="Scree Plot", ylim=c(0,1),type='b')

##Cumulative plot
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", main="Cumulative Proportion", ylim=c(0,1),type='b')
