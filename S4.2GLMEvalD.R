

#************************************************************
#
#				GLM 2 
#				Logistic Regression
#				Evaluation
#
#
#************************************************************


#*****************************
#
# Parameter Interpretation
#
#*****************************

# How much does a one unit change in
# variable 57 affect the odds?

# Variable 57 (total number of capital letters)
# notice that we add one to the variable number (57)
# to account for the intercept

spam.glm <- glm(V58 ~. , data = spam, family = binomial)
(exp(spam.glm$coefficients[58]) -1)*100 #0.08440195 = 0.08% for 1-unit increase

# round to 2 decimal points

round((exp(spam.glm$coefficients[58]) -1)*100,2)

#  Is variable 57 significant in the model? 
#  use the likelihood ratio test

spam.nc <- glm(V58~., data = spam[,-57], family = binomial) ###glm without Capitals
anova(spam.nc, spam.glm, test = "Chi")

### highly significant 7.115e-06 ***

# How much does a one unit change in
# variable 56 affect the odds?


# Is variable 56 significant?


# Repeat for Log model with variable 57.

Lspam.glm <- glm(V58 ~. , data = Lspam, family = binomial)
(exp(Lspam.glm$coefficients[58]) -1)*100 #0.08440195 = 0.08% for 1-unit increase

Lspam.glm <- glm(V58 ~. , data = Lspam, family = binomial)
(exp(Lspam.glm$coefficients[57]) -1)*100 #0.08440195 = 0.08% for 1-unit increase
# 
# Variable 57 (total number of capital letters)
# notice that we add one to the variable number (57)
# to account for the intercept

Lspam.nc <- glm(V58~., data = Lspam[,-57], family = binomial) ###glm without Capitals
anova(Lspam.nc, Lspam.glm, test = "Chi")
#  How much do the capital letter variables contribute to 
#  the model's performance?



#*****************************
#
# Diagnostics
#
#*****************************

# main effects no transform
par(mfrow = c(2,2))
plot(spam.glm)
par(mfrow = c(1,1))

###RVF tells us that all the error shows up in a straight line--we like this!
###Scale location tells us something similar
### QQ plot is not all that significant since we don't really care about Gaussian-ness of model

# main effects log transform


#*****************************
#
# GLM with stepwise
#`
#*****************************


# summary


# likelihood ratio test 
# with main effects


# with log transform of predictors




# summary



# likelihood ratio test




# AIC and BIC for stepwise vs
# main effects models



#*****************************
#
#  Principal Components Regression
#
#*****************************

# obtain the principal components for the predictors with a correlation matrix

spam.pca <- princomp(spam[,-58], cor = T)

# Scree plot

screeplot(spam.pca)

# Proportion of variance

plot(spam.pca$sdev^2/sum(spam.pca$sdev^2), type = "h", xlab = "Components", ylab = "Proportion")

# Cumulative Proportion of variance

plot(cumsum(spam.pca$sdev^2)/sum(spam.pca$sdev^2), type = "h", xlab = "Components", ylab = "Proportion", main = "Cumulative Proportion")

# to see how many components are needed for 90% of the variance we use

var.comp(spam.pca, 90)

#How many components do we need for 98%?


# Use pc.glm() to get the principal component regression results.
# choose the amount of variance (e.g., 98%)

spampca.glm98 <- pc.glm(spam.pca, 98, spam[,58])

# Do a model utility test starting with pc.null()

spampc.null <- pc.null(spam.pca, 98, spam[,58])

anova(spampc.null, spampca.glm98, test = "Chi")


# Do a model utility test for model that uses 
# PC that account for 90% of the variance



# Do a model utility test for model that uses 
# PC that account for 50% of the variance


# Do a partial likelihood test between the 
# 98 % and 90% models



# Compare the AIC for your PC model for 98% 
# of the variance to one that with 
# components that account for 90%



# Do the comparisons with BIC


# Use AIC to choose among all models.
# Use BIC to choose among all models.


#*****************************
#
#   Obtaining test and training sets
#
#*****************************

# Picking 1/3 of the data for the test set
# normally we would use

###getwd()
source("TestSet.R")
Spam <- test.set(spam, .33)

# But to get everyone using the same test set 
# we will use a chosen set in TestLabels.txt

# getting the labels for the test set


Labels <- scan("TestLabels.txt")

spam.test <- spam[Labels,]
spam.train <- spam[-Labels,]

#Check
nrow(spam)
nrow(spam.test) + nrow(spam.train)

summary(spam)
summary(rbind(spam.test,spam.train))


# log transform

Lspam.test <- Lspam[Labels,]
Lspam.train <- Lspam[-Labels,]


# Check of the random draw

round(abs(mean(spam.test$V58) - mean(spam$V58)), 2)

apply(spam.test, 2, mean) - apply(spam.train, 2, mean)

summary(spam.test[,c(56,57)])
summary(spam.train[,c(56,57)])
summary(spam[,56:57])

###Training dataset includes extreme outliers --this gives us a more includsive model, represents the kind of variation you might see in the real-world


boxplot(spam[,56:57])

par(mfrow = c(2,2))
boxplot(spam.test[,c(56,57)],ylim = c(min(spam[,57]),max(spam[,57])), main = "Test")
boxplot(spam.train[,c(56,57)], ylim = c(min(spam[,57]),max(spam[,57])), main = "Train")
boxplot(spam[,56:57], main = "Full Set")
par(mfrow = c(1,1))

# The training set is now in spam.train and the test set is in spam.test.

# here is a plot to see how much of the training set is spam.


par(mfrow = c(2,2))

barplot(table(spam.train[,58]), xlab = "Email Classfication", main = "Spam in the Training Set", names.arg = c("Ham", "Spam"), col = "steelblue")

barplot(table(spam.test[,58]), xlab = "Email Classfication", main = "Spam in the Test Set", names.arg = c("Ham", "Spam"), col = "steelblue")

barplot(table(spam[,58]), xlab = "Email Classfication", main = "Spam in the Original Set", names.arg = c("Ham", "Spam"), col = "steelblue")

par(mfrow = c(1,1))



#*****************************
#
# GLM with the training set
#
#*****************************


# Main effects:  all the variables for the model

#spam.trainglm 

spam.trainglm<-glm(V58~., data = spam.train, family = binomial)


# Log model

Lspam.trainglm<-glm(V58~., data = Lspam.train, family = binomial)

### No need to bother with model utility tests, as we know these models are good.


# Stepwise

spam.trainstep  <- step(spam.trainglm)

Lspam.trainstep <- step(Lspam.trainglm)


#  PCR with training set
#


# obtain the principal components for the 
# training set with a correlation matrix

spam.trainpca <- princomp(spam.train[,-58], cor = T)

# Get the principal component regression results.
# choose the amount of variance (e.g., 98%)

spampcr.train98 <- pc.glm(spam.trainpca, 98, spam.train[,58])



#*****************************
#
# Confusion matrices
#
#*****************************
 
# test set prediction
# main effects model


glm.pred <- predict(spam.trainglm, newdata = spam.test, type = "response")
glm.pred[1:5]

# Get the confusion matrix with score.table()
# the score table function takes 3 arguments: 
# the predicted values, the actuals,
# and the threshold.

# Thresholds: .4, .5, .6

score.table(glm.pred, spam.test[,58],  .4)

score.table(glm.pred, spam.test[,58],  .5)

score.table(glm.pred, spam.test[,58],  .6)

# score table for the log model

# predict

#Lglm.pred 
Lglm.pred <- predict(Lspam.trainglm, newdata = Lspam.test, type = "response")
# Thresholds: .4, .5, .6



# Evaluate the stepwise models

# Step no transform
glmstep.pred <- predict(spam.trainstep, newdata = spam.test, type = "response")
# prediction
#glmstep.pred 

# confusion matrices
# Thresholds: .4, .5, .6
 

# Step log transform
Lglmstep.pred <- predict(Lspam.trainstep, newdata = Lspam.test, type = "response")
# prediction

score.table(glmstep.pred, spam.test[,58],  .4)
score.table(glmstep.pred, spam.test[,58],  .5)
score.table(glmstep.pred, spam.test[,58],  .6)



score.table(Lglmstep.pred, Lspam.test[,58],  .4)
score.table(Lglmstep.pred, Lspam.test[,58],  .5)
score.table(Lglmstep.pred, Lspam.test[,58],  .6)




# confusion matrices

# Thresholds: .4, .5, .6
 

# PCR 
# test set prediction
# with predict.pc.glm()

pcr.pred <- predict.pc.glm(spampcr.train98, spam.trainpca, ndata = spam.test[,-58], type = "response")

#*****************************
#
#   ROC Curves
#
#*****************************
### Goes back to WW2, What is the signal you're receiving (by characteristic)?
par(mfrow = c(1,1))

plot.roc(glm.pred, spam.test[,58], main = "ROC Curve - SPAM Filter")

lines.roc(Lglm.pred, Lspam.test[,58], col = "orange")
lines.roc(glmstep.pred, Lspam.test[,58], col = "purple")
lines.roc(Lglmstep.pred, Lspam.test[,58], col = "green")
lines.roc(pcr.pred, Lspam.test[,58], col = "red")

# Add lines for step (purple), log step (green) and pcr (red)


legend(.65, .45, legend = c("Main", "Log", "StepMain", "StepLog", "PCR"), lwd = 2, col = c("blue", "orange", "purple", "green", "red"))



# Now add a PCR model using the 
# log transformed data
# Look at its performance using a test and training
# set witht an ROC curve

