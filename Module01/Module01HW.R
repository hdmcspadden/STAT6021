# Name: H. Diana McSpadden

# UID: hdm5s

# Assignment: Homework Set 1

# Question 1
# first get the data
#getwd()
data <- read.table('copier.txt', sep='\t', header = TRUE)
head(data,5)

#attach the data frame
attach(data)
m
#use plot(...) to create a scatterplot
plot(x=Serviced, y=Minutes, main='Plot Minutes Against Number Copiers Serviced', xlab = 'Number of Copiers Serviced', ylab = 'Service Time (minutes)')


# create the linear model
lmodel = lm(data)
#produce the summary table
print(summary(lmodel))

# get sigma
lmodel_sigma = sigma(lmodel)
print(lmodel_sigma)

# print the anova of the linear model
print(anova(lmodel))
