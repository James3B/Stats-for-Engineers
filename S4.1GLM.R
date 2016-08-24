

#************************************************************
#
#				GLM 1 
#				Logistic Regression
#
#
#************************************************************


#*****************************
#
# Load the data & source files
#
#*****************************
setwd("D:/R/R_Code")
spam <- read.table("D:\\R\\R_Data_Spam\\Spam.txt", header = F)

source("SPM_Panel.R")
source("FactorPlots.R")
source("PCAplots.R")
source("pc.glm.R")
source("ROC.R")

summary(spam)


#*****************************
#
# Graphical analysis and visualization
#	Scatter plot matices
#
#*****************************

# How much spam do we have? we have 58 variables. Description of variables is
# in homework 4 assignment
# GLM no longer require models to no longer be gaussin
# we use a binomial dist for binary variables. 
# sacrifice lnearity on the betas
# use a link function to take it back tolinear
# model probability of the response



sum(spam$V58)



# How many emails are ham?

nrow(spam)-sum(spam$V58)

# proportion of spam

sum(spam$V58)/nrow(spam)

# proportion of ham


library(ggplot2)
# plot spam and ham

barplot(table(spam[,58]), xlab = "Email Classfication", main = "spam in Data Set", names.arg = c("Ham", "Spam"), col = "steelblue")

# highest % word in the email
# note that variables 1-48 are word %


summary(spam[,1:48])[6,]


max(apply(spam[,1:48], 2, max))

which(apply(spam[,1:48], 2, max) == max(apply(spam[,1:48], 2, max)))

# minimum of the max % word in the email


# Look at the scatter plot matrices
# Use uva.pairs()
# Variables 1:10 and V58


par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,1), tcl=-0.1, mgp=c(0,0,0))
dev.off()
uva.pairs(spam[,c(25,26,28,58)])
uva.pairs(spam[,c(53,56,57,58)])


# Variables 49- 58

uva.pairs(spam[,c(51:58)])

summary(spam[,49:58])


max(apply(spam[,49:58], 2, max))

which(apply(spam[,49:58], 2, max) == max(apply(spam[,49:58], 2, max)))


#*****************************
#
# 	Factor plots
#	
#*****************************

# Obtain boxplots with variables 1-9 vs. the response.
# These are also called factor plots
# Which variable are more discriminatory?

# Variables 1-9
dev.off()  
par(mfrow = c(3,3))
for(i in 1:9)
{
	boxplot(spam[,i]~spam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Variable", i))	
}
par(mfrow = c(1,1))

# spam is 1 ham is 0
# how discriminatory is each variable, abovee
# variable 5 is more discriminatory then 4. greater seperation betreen distributions
# 
# Obtain boxplots with variables 49-57 vs. the response.
# Which variable are more discriminatory?
par(mfrow = c(3,3))
for(i in 49:57)
{
  boxplot(spam[,i]~spam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Variable", i))	
}
par(mfrow = c(1,1))

# Show how disciminatory each factort is for spam.
# no spread means not discriminatory

#*****************************
#
# 	 Plots with log transform
#	
#*****************************


# Log transform of the variables
# offset of 0.01, dont pick one we have observations with value of 1
# use offset to elliminate the zero values.

Lspam <-log(spam[,1:57]+.01)


# Add the response to the data frame

Lspam$V58 <-spam$V58

# Check with summary()
summary(Lspam)

# Scatter plot matrices with 
# log transform of variables 1-8, 58
# use uva.pairs()
# som edegree of predictablility, 
# what we see in linear was want line
# what we want now is an s shape for logistic shape
# anything going up is good and we'll take it
# V7 and V* look good'
# from this we can see we have variables that should be able topredict spam
# very little correlation between variables

par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,1), tcl=-0.1, mgp=c(0,0,0))

uva.pairs(Lspam[,c(1:7,58)])

uva.pairs(Lspam[,c(51:58,58)])
# variables look good but collinear between 55 and 56, 656 and 57
# 
# Scatter plot matrices with 
# log transform of variables 48-58.



# Factor plots
# Variables 1-9

par(mfrow = c(3,3))
for(i in 1:9)
{
	boxplot(Lspam[,i]~Lspam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Log Variable", i))	
}
par(mfrow = c(1,1))

# Variables 49-57

par(mfrow = c(3,3))
for(i in 49:57)
{
  boxplot(Lspam[,i]~Lspam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Log Variable", i))	
}
par(mfrow = c(1,1))
# Variables 20-25

par(mfrow = c(3,3))
for(i in 20:25)
{
  boxplot(Lspam[,i]~Lspam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Log Variable", i))	
}
par(mfrow = c(1,1))

par(mfrow = c(3,3))
for(i in 30:33)
{
  boxplot(Lspam[,i]~Lspam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Log Variable", i))	
}
par(mfrow = c(1,1))

#****************************************************
#
#		Principal Component Analysis
#
#****************************************************

# Obtain the principal components for variables 1-57. 
# Look at the biplot and explain what you see.

spam.pca = princomp(spam[,1:57], cor = T)

Lspam.pca = princomp(Lspam[,1:57], cor = T)

# Get the biplot
biplot(spam.pca)
#biplot(Lspam.pca) - only need on spam not on Log Spam 
cumplot(spam.pca)
# What is the outlier?

max(spam.pca$scores[,2])

which(spam.pca$scores[,2] == max(spam.pca$scores[,2]))

# What are the outlier's values for the variables
# that contribute to component 2?

summary(spam[,which(abs(spam.pca$loadings[,2]) > 0.2)])
summary(spam[,which(abs(spam.pca$loadings[,57]) > 0.2)])
spam[1754,which(abs(spam.pca$loadings[,2]) > 0.2)]
barplot(spam.pca$loadings[which(abs(spam.pca$loadings[,2]) > 0.2),1])

# What are the loadings for the primary variables
# in component 1?1

summary(spam[,which(abs(spam.pca$loadings[,1]) > 0.2)])

spam.pca$loadings[which(abs(spam.pca$loadings[,1]) > 0.2),1]

barplot(spam.pca$loadings[which(abs(spam.pca$loadings[,1]) > 0.2),1])


barplot(spam.pca$loadings[which(abs(spam.pca$loadings[,9]) > 0.2),1])
# Obtain the biplot.fact of your principal components.
# Explain what you see.

biplot.fact(spam.pca, spam[,58])

legend(20, 15, legend = c("Spam", "Ham"), pch = c(18, 19), col = c("red", "blue"))
# seperation is good. seperated by hyperplane
# We have some ham and spam that are close but we have good seperation
# 
# Repeat the principal component analysis for 
# the log transformed variables

biplot.fact(Lspam.pca, Lspam[,58])

legend(-6, 10, legend = c("Spam", "Ham"), pch = c(18, 19), col = c("red", "blue"))
# nicer seperation
# Are there any outliers visible in the 
# biplot?
# 

# What are the loadings for the primary variables
# in component 1?


# Obtain the biplot.fact of your principal components.
# Explain what you see.


#*****************************
#
# GLM
#`
#*****************************

# main effects model

spam.glm <- glm(V58~., data = spam, family = binomial)
# defaults to gaussian
# error says to fit is very good, the closer to fitting to 0, 1
# the harder we must work to get stable
# it's a good thing, getting good answer
# Summary
summary(spam.glm)
# approximation of gaussian to binomial. Z and p from that
# not correct approximation, We ewant chi squared statistic
# AIC in good
# model utility test

spam.null <- glm(V58~1, data=spam, family = binomial)
anova(spam.null, spam.glm, test = "Chi")
# conclude we can rejects numm hyp since chi squared really small
# Log transform
Lspam.glm <- glm(V58~., data = Lspam, family = binomial)

summary(Lspam.glm)


# model utility test for log transform

Lspam.null <- glm(V58~1, data=Lspam, family = binomial)
anova(Lspam.null, Lspam.glm, test = "Chi")
# also signifigant


spam.glm.2 <- glm(V58~., data = spam[,-c(48:57)], family = binomial) 

summary(spam.glm.2)
# parameter tests
# using likelihood ratio
# compare to Gaussian approximation
# drop one for each indiv variable
# another test drop term
# they do Chi squared. complicated fit
# goes thru one term at a time and applies Chic squared test. better pprox than gaussian approx
# equivalent to T-test


library(MASS)

dropterm(spam.glm, test = "Chi")


# compare to Gaussian approx
# given in summary


# repeat for log model
dropterm(Lspam.glm, test = "Chi")

# can also use dropterm()

#*****************************
#
# AIC and BIC Model comparisons
#
#*****************************

# Use AIC & BIC to compare the main effects
# model with no transformtion to the 
# main effects model with log transformed
# predictors

AIC(spam.glm) #1931.765
AIC (Lspam.glm)#1478.782 - Best 


# we like Lspam. it does better in AIC
BIC(spam.glm) #2304.939
BIC(Lspam.glm) #1851.955


# Lspam does better in BIC too

#Lspamquiz<- log(spam[,1:47]+0.01)
#Lspamquiz <- spam$V58

