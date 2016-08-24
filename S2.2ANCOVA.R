

#********************************************************************************
#********************************************************************************
#
#			Understanding Train Accidents with 
#			Analysis of Covariance
#
#********************************************************************************
#********************************************************************************


#***********************************************************
#
#			Data Loading and Cleaning
#
#***********************************************************

# Get totacts, xdmg and xcas as in S2.1Regression.R


#***********************************************************
#
#			Qualitative Variable Modeling for XDMG
#
#***********************************************************

# Create a model with just causa as the predictor for ACCDMG in xdmg

summary(totacts$causa)

xdmg.lm1q <- lm(ACCDMG ~ causa, data = xdmg)

###quiz



summary(xdmg.lm1q)


# what is the base case
# use the contrasts() function

contrasts(xdmg$causa)
##zeroes is the base case

# What is the estimated damage from a human factors accident?


#  Change base case to H

contrasts(xdmg$causa) <- matrix(c(1,0,0,0, 0,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1), byrow =T, nrow = 5)

colnames(contrasts(xdmg$causa)) <- c("E", "M", "S", "T")
contrasts(xdmg$causa)


# compare the significance of track accidents to human factors accidents

#xdmg.lm2q <- lm(log(ACCDMG)~ causa + TYPE + TYPEQ, data = xdmg)
xdmg.lm2ql <- lm(ACCDMG ~ causa + TYPE + TYPEQ, data = xdmg)

# Create a model with just causa, TYPE, and TYPEQ as the predictors for log(ACCDMG) in xdmg

xdmg.lm2q <- lm(log(ACCDMG)~ causa + TYPE + TYPEQ, data = xdmg)

summary(xdmg.lm2ql)

summary(xdmg.lm2q)

contrasts(xdmg$TYPE)

contrasts(xdmg$TYPEQ)

# what are the base cases for all variables?



# Diagnostic plots
# How good is the model?


plot(xdmg.lm2ql, 1)

plot(xdmg.lm2ql, 2)

plot(xdmg.lm2q, 1)

plot(xdmg.lm2q, 2)


# Look at frequency of levels in each variable

table(xdmg$causa)
table(xdmg$TYPE)
table(xdmg$TYPEQ)


# Create variables for Freight and for Derailment

# Derail

xdmg$Derail <- rep(0, nrow(xdmg))

xdmg$Derail[which(xdmg$TYPE == "Derailment")] <- 1

sum(xdmg$Derail)

# Freight

xdmg$Freight <- rep(0, nrow(xdmg))

xdmg$Freight[which(xdmg$TYPEQ == "Freight")] <- 1

sum(xdmg$Freight)

# Box plots

boxplot(ACCDMG~Derail, data = xdmg, main = "Total Derailments")

boxplot(ACCDMG~Freight, data = xdmg, main ="Freights")



boxplot(log(ACCDMG)~Derail, data = xdmg, main = "Derailments")

boxplot(log(ACCDMG)~Freight, data = xdmg, main = "Freights")



# Now create a model with causa, Derail, and Freight as the predictor for log(ACCDMG) in xdmg 

xdmg.lm3q <- lm(log(ACCDMG)~ causa + Derail + Freight, data = xdmg)
xdmg.lm3q1 <- lm(ACCDMG~ causa + Derail + Freight, data = xdmg)

summary(xdmg.lm3q1)
summary(xdmg.lm3q)

plot(xdmg.lm3q,1)
plot(xdmg.lm3q,2)
plot(xdmg.lm3q1,1)
plot(xdmg.lm3q1,2)
# Is derailment significant? Yes

# Are freight trains significanltly different from other trains?

# An interaction model


xdmg.lm4q <- lm(log(ACCDMG)~ (causa + Derail + Freight)^2, data = xdmg)

summary(xdmg.lm4q)

anova(xdmg.lm3q, xdmg.lm4q)

#should you use the causa variable? Yes


xdmg.lm5q<- lm(log(ACCDMG)~ (Derail+Freight)^2, data = xdmg)

anova(xdmg.lm3q, xdmg.lm4q)
anova(xdmg.lm4q, xdmg.lm5q)

#*********************************************
# Build a regression model with the prediction variables
# TRNSPD, TONS, TEMP, MONTH, TIMEHR
# Cause, TYPE, and TYPEQ
# log(ACCDMG) as the response and 
# using xdmg
#*********************************************

xdmg.lm5 <- lm(log(ACCDMG)~TRNSPD+ TONS+ TEMP+ MONTH+ TIMEHR+ causa + TYPE+ TYPEQ, data = xdmg)
xdmg.lm5 <- lm(log(ACCDMG)~TRNSPD+ TONS+ TEMP+ MONTH+ TIMEHR+ causa + Derail + Freight, data = xdmg)
# Cause, TYPE, and TYPEQ)

summary(xdmg.lm5)

# Look at model utility test


# Test all variables not significant in the t-test at 0.1

xdmg.lm6 <- lm(log(ACCDMG)~TRNSPD+ TONS+  causa + TYPE+ TYPEQ, data = xdmg)

xdmg.lm6 <- lm(log(ACCDMG)~TRNSPD+ TONS+  causa + TYPE, data = xdmg)

summary(xdmg.lm6)

anova(xdmg.lm6, xdmg.lm5)

# How do you interpret the model?

# Explain using TRNSPD and the categorical variables.

# Plot residuals vs. fitted

plot(xdmg.lm6, 1)
plot(xdmg.lm6, 2)
# Plot QQ


# Should we keep the qualitative variables?

xdmg.lm7 <- lm(log(ACCDMG)~TRNSPD+ TONS +TEMP + MONTH + TIMEHR, data = xdmg)

summary(xdmg.lm7)

anova(xdmg.lm7, xdmg.lm6)

# Should we keep the quantitative variables?


# Should we keept causa?


#********************
# Now consider an interaction model

xdmg.lm9 <- lm(log(ACCDMG)~TRNSPD+ TONS+  causa + Derail +Freight, data = xdmg)

summary(xdmg.lm9)

xdmg.lm10 <- lm(log(ACCDMG)~(TRNSPD+ TONS+  causa + Derail +Freight)^2, data = xdmg)

summary(xdmg.lm10)

# How do you interpret the model?

# Explain using TRNSPD and the categorical variables.

# Plot residuals vs. fitted

plot(xdmg.lm10, 1)

# Plot QQ

plot(xdmg.lm10, 2)

# Compare with the main effects model

anova(xdmg.lm9, xdmg.lm10)


# Is derailment significanly correlated with extreme damage accidents?

xdmg.lm11 <- lm(log(ACCDMG)~(TRNSPD+ TONS+  causa +Freight)^2, data = xdmg)


anova(xdmg.lm11,xdmg.lm10)

##Speed FREIGHTS, WEIGHT DERAILMENT
# Are freight trains significantly correlated with extreme damage accidents?



#**************************************************
# Plotting the impact of train speed and derailment
# on accident damage
#**************************************************


#  Create a DF with varying speed (30-80) and derailment and FREIGHT - setting cause to T; 

basedata <- data.frame(TONS = rep(mean(xdmg$TONS), 100), causa = rep("T", 100), Derail = rep(1, 100), Freight = rep(1, 100), TRNSPD = seq(30, 80, length.out = 100))


#  Create a DF with varying speed (30-80) and no derailment

basedata2 <- data.frame(TONS = rep(mean(xdmg$TONS), 100), causa = rep("T", 100), Derail = rep(0, 100), Freight = rep(1, 100), Visib = rep("Day", 100), TRNSPD = seq(30, 80, length.out = 100))

# Add the standard error to the plots
# main = "Predicted Total Damage from Derailment"

xdmgpred.derail <- predict(xdmg.lm10, newdata = basedata, se.fit = T)
xdmgpred.noderail <- predict(xdmg.lm10, newdata = basedata2, se.fit = T)

plot(basedata$TRNSPD, exp(xdmgpred.derail$fit), type = "l", xlab = "Train Speed", ylab = "Cost ($)", col = "orange")


lines(basedata$TRNSPD, exp(xdmgpred.derail$fit - 2*xdmgpred.derail$se.fit), lty = "dashed", col = "orange" )

lines(basedata$TRNSPD, exp(xdmgpred.derail$fit + 2*xdmgpred.derail$se.fit), lty = "dashed", col = "orange" )

lines(basedata$TRNSPD, exp(xdmgpred.noderail$fit), col = "cyan")


lines(basedata$TRNSPD, exp(xdmgpred.noderail$fit - 2*xdmgpred.noderail$se.fit), lty = "dashed", col = "cyan" )

lines(basedata$TRNSPD, exp(xdmgpred.noderail$fit + 2*xdmgpred.noderail$se.fit), lty = "dashed", col = "cyan" )

legend(35, 14.5e5, legend = c("Derailment", "No Derailment"), col = c("orange", "cyan"), lwd = 2)



