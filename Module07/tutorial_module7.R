data<-read.table("nfl.txt", header=TRUE, sep="")
attach(data)
# 1 response, 9 possible predictors, 1976 nfl season

library(leaps)

##perform all possible regressions (1st order)
# use the dataframe called "data", response variable == y, ~. nbest=9, for all potential sizes, consider 9 best with 1 predictor, 9 best with 2 predictors
allreg <- regsubsets(y ~., data=data, nbest=9)

summary(allreg) # this is ugly

##create a "data frame" that stores the predictors in the various models considered as well as their various criteria
best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(data)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic

#best
best$bic

##sort by various criteria
best[order(best$r2),] # largest
best[order(best$adjr2),] # largest
best[order(best$mse),] # smallest
best[order(best$cp),] # smallest
best[order(best$bic),] # smallest

##intercept only model
## now Dr. Woo is showing us various stepwise search procedures to select a model
regnull <- lm(y~1, data=data) # intercept only/empty model == STARTING POINT
##model with all predictors
regfull <- lm(y~., data=data) # full model == END POINT

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward") # start from intercept only model. Step lower and upper models. Directions
# How to read output?
# above step creates this model: lm(formula = y ~ x8 + x2 + x7 + x9, data = data)
# Start shows intercept.

# Start with x8
# Go to next step
# selects x2
# Go to next step
# selects x7
# Go to next step
# selects x9
# End


step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
# above step creates: lm(formula = y ~ x2 + x7 + x8 + x9, data = data)


step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")
# above step call creates: Call: lm(formula = y ~ x8 + x2 + x7 + x9, data = data)

# automated search procedures don't always agree, and can produce other models with different starting points

step(lm(y~x1), scope=list(lower=regnull, upper=regfull), direction="both")
# above step call creates: Call: lm(formula = y ~ x1 + x2 + x8 + x9)