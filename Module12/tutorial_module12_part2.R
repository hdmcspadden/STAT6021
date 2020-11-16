names(mtcars)
?mtcars

##perform PCA, with scaling.
pr.out<-prcomp(mtcars[,c(-8,-9)], scale=TRUE)

##extract the mean and SD of each variable
pr.out$center
pr.out$scale

##same result if you calculated their means and SDs on your own
apply(mtcars[,c(-8,-9)], 2, mean)
apply(mtcars[,c(-8,-9)], 2, sd)

##obtain the loading vector for the PCs
pr.out$rotation

##SD of each PC
pr.out$sdev

##variance of each PC
pr.var<-pr.out$sdev^2
pr.var

##proportion of variance in data explained by each PC
pve<-pr.var/sum(pr.var)
pve

##plot of first two PCs
biplot(pr.out, scale=0)

##Scree plot
plot(pve, ylim=c(0,1))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", main="Scree Plot", ylim=c(0,1),type='b')

##Cumulative plot
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", main="Cumulative Proportion", ylim=c(0,1),type='b')
