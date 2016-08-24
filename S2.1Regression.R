

#********************************************************************************
#********************************************************************************
#
#			Understanding Train Accidents with 
#			Ordinary Least Squares (OLS) Regression
#
#********************************************************************************
#********************************************************************************




#***********************************************************
#
#			Data Loading 
#
#***********************************************************
setwd("D:\\R")

mydatapath <- "R_Output\\totactsClean.csv" 
cleanData <- read.csv(mydatapath)


#***********************************************************
#
#			Regression Modeling
#
#***********************************************************

# Models with quantitative variables

# Build a regression model with the variables
# TRNSPD, TONS, TEMP, MONTH, TIMEHR
# using xdmg

xdmg.lm1 <- lm(ACCDMG ~ TRNSPD+ TONS+ TEMP+ MONTH+ TIMEHR, data = xdmg)
xdmg.lm11 <- lm(ACCDMG ~ TRNSPD+ TONS, data = xdmg)

summary(xdmg.lm1)
summary(xdmg.lm11)

anova(xdmg.lm1, xdmg.lm11)
# Comment on the t-tests
# Comment on the F test (model utility)



# How do you interpret this model?

# Comment on the adjusted R squared

# Plot residuals vs. fitted
####1st Model 
plot(xdmg.lm1,1)


plot(xdmg.lm1)


# Plot QQ
#plot(xdmg.lm1,2)

##2nd Model 

#plot(xdmg.lm11,1)

#plot(xdmg.lm11)


# Plot QQ
#plot(xdmg.lm11,2)






# Try taking the log of the response
# Redo the regession


xdmg.lm2 <- lm(log(ACCDMG) ~ TRNSPD+ TONS+  TEMP+ MONTH+ TIMEHR, data = xdmg)

summary(xdmg.lm2)



# Comment on the t-tests
# Comment on the F test (model utility)



# Comment on the adjusted R squared

# Plot residuals vs. fitted

plot(xdmg.lm2, 1)

# Plot QQ

plot(xdmg.lm2, 2)

plot(xdmg.lm2)

# Try an interaction model

xdmg.lm3 <- lm(ACCDMG ~ (TRNSPD+ TONS+ TEMP+ MONTH+ TIMEHR)^2, data = xdmg)

summary(xdmg.lm3)

xdmg.lm4 <- lm(log(ACCDMG) ~ (TRNSPD+ TONS+ TEMP+ MONTH+ TIMEHR)^2, data = xdmg)

summary(xdmg.lm4)

# Comment on the t-tests
# Comment on the F test (model utility)

plot(xdmg.lm3, 1)
plot(xdmg.lm3, 2)
plot(xdmg.lm3)

plot(xdmg.lm4, 1)
plot(xdmg.lm4, 2)
plot(xdmg.lm4)

# How do you interpret this model?

# Comment on the adjusted R squared

# Plot residuals vs. fitted


# Plot QQ


# Compare the interaction model to the main effects model

anova(xdmg.lm2, xdmg.lm4)
anova(xdmg.lm1, xdmg.lm3)

# Try a complete second order model


xdmg.lm5 <- lm(ACCDMG ~ (TRNSPD+ TONS+ + TEMP+ MONTH+ TIMEHR)^2 + I(TRNSPD^2)+ I(TONS^2)+ I(TEMP^2)+ I(MONTH^2)+ I(TIMEHR^2) , data = xdmg)

xdmg.lm6 <- lm(log(ACCDMG) ~ (TRNSPD+ TONS+ + TEMP+ MONTH+ TIMEHR)^2 + I(TRNSPD^2)+ I(TONS^2)+ I(TEMP^2)+ I(MONTH^2)+ I(TIMEHR^2) , data = xdmg)

summary(xdmg.lm5) ##mutiplicity
summary(xdmg.lm6)
# Comment on the t-tests

# Comment on the F test (model utility)



# Comment on the adjusted R squared

# Plot residuals vs. fitted

plot(xdmg.lm5,1)
plot(xdmg.lm5,2)
plot(xdmg.lm6,1)
plot(xdmg.lm6,2)

# Plot QQ


# Compare the interaction model to the complete second order model

anova(xdmg.lm3, xdmg.lm5)


#*****************************
# Repeat for extreme accidents
#*****************************


# Models with quantitative variables

# Build a regression model with the variables
# TRNSPD, TONS, CARSUM, TEMP, MONTH, TIMEHR
# using xcas

xcas.lm1 <- lm(CASUALTY ~ TRNSPD+ TONS+ CARSUM+ TEMP+ MONTH+ TIMEHR, data = xcas)

summary(xcas.lm1)


# Comment on the t-tests
# Comment on the F test (model utility)


# How do you interpret this model?

# Comment on the adjusted R squared

# Plot residuals vs. fitted

plot(xcas.lm1, 1)

# Plot QQ

plot(xcas.lm1, 2)


# Try taking the log of the response
# Redo the regession


xcas.lm2 <- lm(log(CASUALTY) ~ TRNSPD+ TONS+ CARSUM+ TEMP+ MONTH+ TIMEHR, data = xcas)

summary(xcas.lm2)




# Comment on the t-tests
# Comment on the F test (model utility)



# Comment on the adjusted R squared

# Plot residuals vs. fitted

plot(xcas.lm2, 1)
# Plot QQ

plot(xcas.lm2, 2)


# Try an interaction model


xcas.lm3 <- lm(log(CASUALTY) ~ (TRNSPD+ TONS+ CARSUM+ TEMP+ MONTH+ TIMEHR)^2, data = xcas)

summary(xcas.lm3)

# Comment on the t-tests
# Comment on the F test (model utility)



# How do you interpret this model?

# Comment on the adjusted R squared

# Plot residuals vs. fitted


# Plot QQ


# Compare the interaction model to the main effects model

anova(xcas.lm2, xcas.lm3)

# Try a complete second order model



# Comment on the t-tests
# Comment on the F test (model utility)



# Comment on the adjusted R squared

# Plot residuals vs. fitted


# Plot QQ


# Compare the interaction model to the main effects model



