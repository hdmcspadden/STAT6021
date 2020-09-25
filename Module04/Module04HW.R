
#q1-1
# load the data
?swiss

attach(swiss)

#q1-2
head(swiss)

summary(swiss)

#q1a-1
pairs(swiss,lower.panel = NULL)

#q1a-2
swiss.cor <- cor(swiss)

threshold <- 0.6 # set a correlation threshold, is there a standard? A: Not really.

corWorking <- swiss.cor

diag(corWorking) <- 0 # set diagonal to 0 so 1's of unrelated don't get caught in threshold

swiss.cor.related <- apply(abs(corWorking) >= threshold, 1, any) # apply the filter to the absolute value of the correlations between variables.

swiss.cor[swiss.cor.related, swiss.cor.related]

#q1b-1

swissModel <-lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)

swissModel

#q2b-1
# calculate the B2 test statistics
B2.teststatistic = -0.014071 / 0.022708 # se(Bj-hat) is given
print(paste("The B2 test statistic is: ", B2.teststatistic))

# calculate the B2 p-value of the test statistic
B2.pvalue = 2*pt(-abs(B2.teststatistic), df=(113 - 4 - 1))  # calc p Value - two-sided is the 2 *
print(paste("The B2 p-value is: ", B2.pvalue))

# calculate the B2 critical value to compare the test statistic to
criticalValue = qt(0.975, (113 - 4 - 1))
print(paste("The critical value is: ", criticalValue))


#q2d-1
B1 <- 0.237209
se.B1 <- 0.060957

B2 <- -0.014071
se.B2 <- 0.022708

B3 <- 0.020383
se.B3 <- 0.005524

delta <- qt((1 - (.05/6)), (113 - 4 - 1))

print(paste("tvalue: ",delta))

B1.CI.low <- B1 - (delta * se.B1)
B1.CI.high <- B1 + (delta * se.B1)
print(paste("The simultaneous 95% CI for B1 is: ", B1.CI.low, " - ", B1.CI.high))

B2.CI.low <- B2 - (delta * se.B2)
B2.CI.high <- B2 + (delta * se.B2)
print(paste("The simultaneous 95% CI for B2 is: ", B2.CI.low, " - ", B2.CI.high))

B3.CI.low <- B3 - (delta * se.B3)
B3.CI.high <- B3 + (delta * se.B3)
print(paste("The simultaneous 95% CI for B3 is: ", B3.CI.low, " - ", B3.CI.high))

#q2e1
n = 113 # number of observations
k = 4 # number regressor variables
p = k + 1

MSE = 1.04^2

SSE = MSE * (n-p)

F0 = 19.56

MSR = MSE * F0

SSR = MSR * k

SST = SSE + SSR

R2 = SSR / SST

print(paste("Regression df: ",k))
print(paste("Error df: ",n - p))
print(paste("Total df: ",n - 1))
print(paste("MSE: ", MSE))
print(paste("SSE: ", SSE))
print(paste("F0: ", F0))
print(paste("MSR: ", MSR))
print(paste("SSR: ", SSR))
print(paste("SST: ", SST))
print(paste("R2: ", R2))


#q2f-1
print(paste("R2 for the model is: ", R2))

#q2g-1
R2.adj <- 1 - ((SSE / (n - p)) / (SST / (n-1)))

print(paste("R2 Adjusted for the model is: ", R2.adj))