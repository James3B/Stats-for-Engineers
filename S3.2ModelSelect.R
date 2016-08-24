


#****************************************
#
#  	Model Selection 
#
#****************************************

#******************************
#
#	 Source Code
#
#******************************

setwd("D:/R/R_Code")
source("TestSet.R")
source("SPM_Panel.R")


# Data

xdmgimpute <- read.csv("D:\\R\\R_Output\\xdmgImpute.csv")


#***********************************************
#
#		Partial F Test
#
#***********************************************

anova(train.step, train.mainbc)
anova(train.step, train.int)

# t tests
# Don't use them for Model Selection!

# probability of at least one type I error
# in the interaction model

1 - 0.05^700

#********************************************
#
#		Penalty Methods
#
#***********************************************

# Adjusted R2 - bigger - the better - variance can  be expa

round(summary(train.mainbc)$adj.r,2)

round(summary(train.step)$adj.r,2)

round(summary(train.int)$adj.r,2)

# AIC - smalled the bettter 

round(AIC(train.mainbc))

round(AIC(train.step))

round(AIC(train.int))
# BIC

round(AIC(train.mainbc, k =log(nrow(xdmgimpute))))#BIC

round(AIC(train.step, k =log(nrow(xdmgimpute))))#BIC

round(AIC(train.int, k =log(nrow(xdmgimpute))))#BIC

summary(train.int)

#****************************************
#
#		Data Cleaning for Test Sets  - Sampling 
#
#****************************************

# Data Cleaning for test sets
# what problem will we have with TYPE? TYPEQ? ACCTRKCL"?

summary(xdmgimpute$TYPE)
summary(xdmgimpute$TYPEQ)
summary(xdmgimpute$ACCTRKCL)
# New Type variable with Derailment
# Hwy-Rail 

xdmgimpute$Type <- rep("Other", nrow(xdmgimpute))
xdmgimpute$Type[which(xdmgimpute$TYPE == "Derailment" )] <- "Derailment"
xdmgimpute$Type[which(xdmgimpute$TYPE == "Hwy-Rail" )] <- "Hwy-Rail"
xdmgimpute$Type <- factor(xdmgimpute$Type)

# New Typeq with Freight, Passenger, Yard


xdmgimpute$Type <- rep("Other", nrow(xdmgimpute))
xdmgimpute$Type[which(xdmgimpute$TYPE == "Derailment" )] <- "Derailment"
xdmgimpute$Type[which(xdmgimpute$TYPE == "Hwy-Rail" )] <- "Hwy-Rail"
xdmgimpute$Type <- factor(xdmgimpute$Type)

# New Typeq with Freight, Passenger, Yard

xdmgimpute$Typeq <- rep("Other", nrow(xdmgimpute))
xdmgimpute$Typeq[which(xdmgimpute$TYPEQ == "Freight" )] <- "Freight"
xdmgimpute$Typeq[which(xdmgimpute$TYPE == "Passenger" | xdmgimpute$TYPE == "Commuter" )] <- "Passenger"
xdmgimpute$Typeq[which(xdmgimpute$TYPEQ == "Yard" )] <- "Yard"
xdmgimpute$Typeq <- factor(xdmgimpute$Typeq)

# New ACCTRKCL

xdmgimpute$Trkclas <- rep("Other", nrow(xdmgimpute))
xdmgimpute$Trkclas[which(xdmgimpute$ACCTRKCL == "1" )] <- "1"
xdmgimpute$Trkclas[which(xdmgimpute$ACCTRKCL == "2" )] <- "2"
xdmgimpute$Trkclas[which(xdmgimpute$ACCTRKCL == "3" )] <- "3"
xdmgimpute$Trkclas[which(xdmgimpute$ACCTRKCL == "4" )] <- "4"
xdmgimpute$Trkclas[which(xdmgimpute$ACCTRKCL == "5" )] <- "5"


# New Trkclas


# Remove the old variables TYPE, TYPEQ, ACCTRKCL

matrix(names(xdmgimpute))

xdmgimpute <- xdmgimpute[,-c(2,9,33)]
#****************************************
#
#	 Test Sets
#
#****************************************



# You can use the code in TestSet.R to 
# get a training and test set.
# The default is 1/3 for testing


set.seed(1234)

Xacts <- test.set(xdmgimpute, .33)



#Check data in training and test sets

boxplot(list(Xacts$test$ACCDMG^lambda, Xacts$train$ACCDMG^lambda, xdmgimpute$ACCDMG^lambda), names = c("Test", "Training", "Full"))


# Look at training at test sets for the cateorical variables

summary(Xacts$test$Type)/nrow(Xacts$test)
summary(Xacts$test$Typeq)/nrow(Xacts$train)



# Build the main effects, stepwise, interaction with the training set.
# Use the Box-Cox Transformation

train.mainbc2<-lm((ACCDMG^lambda)~., data = Xacts$train)

train.step2 <-step(train.mainbc2)

train.int2 <- update(train.step2, .~.^2, data = Xacts$train)
# What do the summaries tell us?
summary(train.mainbc2)
summary(train.step2)
summary(train.int2)

# Correct by removing the offending variable



# Set seed  and then get the test & training sets

set.seed(1234)


#  models with training data




# What do the summaries tell us?



# Degrees of freedom/size of training and test sets


nrow(Xacts$train)
#3349
nrow(Xacts$test)
#1604

# Get the predictions for each model

mainbc2.pred <- predict(train.mainbc2, newdata = Xacts$test)
step2.pred <- predict(train.step2, newdata = Xacts$test)
int2.pred <- predict(train.int2, newdata = Xacts$test) 

# Check out the raw predictions

summary((mainbc2.pred))
summary(step2.pred)
summary(int2.pred)
# Correct the problem with one of the models


int2.pred[which(int2.pred < 0)] <- min(mainbc2.pred)
summary(int2.pred)
# Invert the transformation


mainbc2d.pred <- mainbc2.pred^(1/lambda)
step2d.pred <- step2.pred^(1/lambda)
int2d.pred <- int2.pred^(1/lambda)

# to get $



# Look at summaries of the predictions


summary((mainbc2d.pred))
summary(step2d.pred)
summary(int2d.pred)



# Get NRMSE for each model



regr.eval(Xacts$test$ACCDMG, mainbc2d.pred, stats = c("rmse", "nmse"), train.y = mean(Xacts$train$ACCDMG))

regr.eval(Xacts$test$ACCDMG, step2d.pred, stats = c("rmse", "nmse"), train.y = mean(Xacts$train$ACCDMG))

regr.eval(Xacts$test$ACCDMG, int2d.pred, stats = c("rmse", "nmse"), train.y = mean(Xacts$train$ACCDMG))

# Round the results




#*********************************************
# Cross Validation
#*********************************************

## A function for generating lm models

cv.lm <- function(form, train, test, ...) {
  model <- lm(form, train, ...)
  preds <- predict(model, test)
  regr.eval(resp(form, test), preds,
            stats=c('mae','nmse'), train.y=resp(form, train))
}


# Main effects data

meData <- xdmgimpute
#Add response
meData$Response = xdmgimpute$ACCDMG^lambda
meData = meData[,-which(names(xdmgimpute) == "ACCDMG")]

# Stepwise data

step.terms  <- attr(train.step2$terms, "term.labels")
StepData <- xdmgimpute[,step.terms]
# Add response
StepData$Response <- xdmgimpute$ACCDMG^lambda


## The evaluation with CV

# Main effects

eval.res <- crossValidation(learner('cv.lm',pars=list()), dataset(Response ~., meData), cvSettings(1,10,1234))


## Check a summary of the results
summary(eval.res)


## Plot results 

plot(eval.res)


# Stepwise 

step.eval.res <- crossValidation(learner('cv.lm',pars=list()), dataset(Response ~., StepData), cvSettings(1,10,1234))

## Check a summary of the results
summary(step.eval.res)

## Plot results 

plot(step.eval.res)
james.lm <- lm((ACCDMG^lambda) ~ AMPM +CARS+ TRNSPD+ POSITON2+ HEADEND1 + MIDREM1 + RREM1 + HEADEND2 + LOADF1 + LOADF2 + LOADP2 + EMPTYF2 + EMPTYP2 + CAUSE + Type + Typeq + Trkclas, data = xdmgimpute)
summary(james.lm)
plot(james.lm)

dev.off()
par(mfcol = c(1,1), oma=c(0,0,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
pairs(~ TRNSPD+ POSITON2+ HEADEND1 + MIDREM1 + RREM1 + HEADEND2 + LOADF1 + LOADF2 + LOADP2 + EMPTYF2 + EMPTYP2 , data = xdmgimpute)


library(ggplot2)
dev.off()
ggplot(xdmg, aes(x = xdmg$YEAR, fill = as.factor(xdmg$POSITON2))) + geom_histogram()
ggplot(xdmg, aes(x = xdmg$YEAR, fill = as.factor(xdmg$HEADEND1))) + geom_histogram()
ggplot(xdmg, aes(x = xdmg$YEAR, fill = as.factor(xdmg$AMPM))) + geom_histogram()
ggplot(xdmgimpute, aes(x = xdmg$YEAR, fill = as.factor(xdmgimpute$LOADF1> 57))) + geom_histogram()
ggplot(xdmg, aes(x = xdmg$YEAR, fill = as.factor(xdmg$EMPTYF1 > 18))) + geom_histogram()
ggplot(xdmgimpute, aes(x = xdmgimpute$ACCDMG, fill = as.factor(xdmg$EMPTYF1 >1))) + geom_histogram(alpha =I(.5)) + scale_x_log10() ###look at this one 
ggplot(xdmg, aes(x = xdmg$YEAR, fill = as.factor(xdmg$EMPTYF2 > 0 ))) + geom_histogram() ###look at this one 
ggplot(xdmg, aes(x = xdmg$YEAR, fill = as.factor(xdmg$TYPEQ ))) + geom_histogram() ###look at this one 
ggplot(xdmgimpute, aes(x = xdmgimpute$ACCDMG, fill = as.factor(xdmg$ACCTRKCL))) + geom_density(alpha = 0.5) + scale_x_log10() ###look at this one 
ggplot(xdmgimpute, aes(x = xdmgimpute$ACCDMG, fill = as.factor(xdmg$HEADEND1))) + geom_histogram(alpha =I(.5)) + scale_x_log10() ###look at this one 
ggplot(xdmgimpute, aes(x = xdmgimpute$ACCDMG, fill = as.factor(xdmg$RREM1))) + geom_histogram(alpha =I(.5)) + scale_x_log10() ###look at this one 
ggplot(xdmgimpute, aes(x = xdmgimpute$ACCDMG, fill = as.factor(xdmg$POSITON1 >1))) + geom_histogram(alpha =I(.5)) + scale_x_log10() ###false are head on 
#ggplot(xdmgimpute, aes(x = xdmgimpute$Trkclas, fill = as.factor(xdmgimpute$Typeq))) + geom_histogram(alpha =I(.5)) ###look at this one #
#hist(xdmg$ACCDMG~xdmg$AC)
ggplot(xdmgimpute, aes(x = xdmgimpute$ACCDMG, fill = as.factor(xdmgimpute$Typeq))) + geom_density(alpha =I(.5), binwidth = 50) + scale_x_log10() ###look at this one 
#ggplot(xdmgimpute, aes(x = xdmgimpute$ACCDMG, fill = as.factor(xdmgimpute$AMPM))) + geom_density(alpha =I(.5), binwidth = 50) + scale_x_log10() ###look at this one 
