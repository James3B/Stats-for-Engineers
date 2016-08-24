
#***********************************************
#
#		Regression Evaluation Examples
#
#**********************************************

#***********************************************
#
#		Load the Data
#
#***********************************************


#xdmg <- read.csv("D:\\R\\R_Output\\extreme.csv", header = T)

##xdmg <- read.csv( "xdmg.csv", header = T)


#***********************************************
#
#		More Data Cleaning
#
#***********************************************

#matrix(names(xdmg))
#summary(xdmg[,1:80])

# Go down the list of variables and select 
# the ones to use in modeling
# Variable List



# Get levels for weather and visibility





# What about AMPM?




# What about ACCTRKCL?


# Select the variables
#matrix(names(xdmg))

#xdmgClean <- xdmg

#xdmgClean <- xdmg[,c(18:20, 26:33, 35:59, 71,106,107)] 
#xdmgClean<- xdmgClean[,-39]

#matrix(names(xdmgClean))

# Write the data to a file

#write.csv(xdmgClean, file ="D:\\R\\R_Output\\xdmgclean.csv", row.names = F)

#summary(xdmgClean)


#*****************************************************
#                                                          
# 		Regression with All variables
#
#*****************************************************
# Main effects model


train.main <- lm(ACCDMG ~., data = xdmgClean)
summary(train.main)
plot(train.main)
# Size of data set



# Variables



# Main effects
# no transform



# Evalute the model with summary & plots


##############################
# Multicollinearity

# Any problems? Yes with Carsum


# Correct and Redo the model



#***********************************************
#
#		Box-Cox Plots and Analysis
#
#***********************************************

#  For Box-Cox Plots
# you will need the MASS package

library(MASS)


summary(xdmgClean$ACCDMG)

# note, the response variable must be nonzero
# check it


# Box-Cox analysis of the response

train.bc<- boxcox(train.main)


# if you want a list of plotted points to better judge the optimal use this
train.bc$x[which.max(train.bc$y)]
# Round your choice of lambda (-0.4)

lambda = -0.4

# This tells you the value for lambda that maximizes the log-likelihood.



#   Main Effects Model with BC transform


train.mainbc <- lm((ACCDMG^lambda)~., data = xdmgClean)
plot(train.mainbc)

# Evaluate this model

#***********************************************
#
#		Stepwise
#
#***********************************************

# step may take a minute

train.step<- step(train.mainbc)
plot(train.step)

summary(xdmgClean[,c("ACCTRKCL", "TYPEQ")])

# Do you have problem  What should we do?

# na.exclude? na.omit?

# what do we lose?

# Do imputation




# Freight train difference in number - before it was 3736 and   3739
# of accidents



# Get a new main effects model

train.mainbc <- lm((ACCDMG^lambda)~.,data = xdmgimpute)

summary(train.mainbc)
# Get stepwise model

train.step <- step(train.mainbc) 

# difference in number of coefficients
length(coefficients(train.step)); 
length(coef(train.mainbc))
##Stepwise has 51 and Main effect w/ BC has 72
#82-44

# Partial F
anova(train.step, train.mainbc)

###PR is really Hign - so we know Stepwise removed vars that are none zero 
##Stepwise was better 
# Evaluation

plot(train.step)

#******************************
#
#	Interaction Model
#
#******************************

# Use stepwise as the starting point
# How many terms?

# How many terms if we use the 
# main effects with all variables?
# how many with stepwise?


# Interaction model
##train.int <- lm((ACCDMG^ lambda)~.^2, data = xdmgimpute)

train.int <-update(train.step, .~.^2, data = xdmgimpute)
plot(train.int)
plot(train.step)
#same as above

# Size?

length(coef(train.int))

# partial F

anova(train.int, train.step)
# Evaluation

