data <- read.table("delivery.txt", header=TRUE ,sep="")
attach(data)

result<-lm(Delivery~Number+Distance)

summary(result)

qf(0.05, 2, 22)

confint(result,level = 0.95)

newdata<-data.frame(Number=20, Distance=200)

predict.lm(result, newdata, level=0.95, interval="confidence")
predict.lm(result, newdata, level=0.95, interval="prediction")



