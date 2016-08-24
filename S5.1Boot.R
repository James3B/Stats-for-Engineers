

#*********************************************************
#
#		Bootstrapping Introduction
#
#*********************************************************

#*************************************
# Get the energy data
setwd("D:\\R\\R_code")
energy <- read.table("D:\\R\\wk5_bootstrap\\ENB2012_data.csv", header = T, sep = ",")

vars <- c("Compactness", "SurfaceArea", "WallArea", "RoofArea", "Height", "Orientation", "GlazingArea", "GADistribution", "HeatingLoad", "CoolingLoad") 

colnames(energy) <- vars

summary(energy)

source("TSbootfunctions.R")
source("SPM_Panel.R")


#********************************************
#
#			Graphics
#
#********************************************

par(mfrow = c(1,1))
# Look at histograms and box plot of heating and cooling loads

qqnorm(energy$Coolingload)
qqline(energy$Coolingload)
hist(energy$CoolingLoad)

qqnorm(energy$HeatingLoad)
qqline(energy$HeatingLoad)
hist(energy$HeatingLoad)

# Look at the QQ plots

plot(energy)
# SPM with uva.pair()

uva.pairs(energy)
# PCA and biplot()
energy.pca <-princomp(energy[,1:8], cor =T)

biplot(energy.pca)

# Loadings


energy.pca$loadings


#****************
# Empirical CDF

# Cooling Load

cool.ecdf <- ecdf(energy$CoolingLoad)
heat.ecdf <- ecdf(energy$HeatingLoad)
# plot of ecdf

plot(cool.ecdf, xlab = "Cooling Load", main = "CDF of Cooling Load")
abline(v = mean(energy$CoolingLoad), col = "blue")

lines(heat.ecdf, col = "red")

legend(40, .3, c("colling", "Heating"), col = c("black", "red"))

#****************
# Bootstrapping
#****************

library(boot)


quantile(mean.boot$t, .0975)
# 2000 bootstrap samples for mean

mean.boot <- boot(energy$CoolingLoad, statistic = function(d,i){mean(d[i])}, 2000)

median.boot <- boot(energy$CoolingLoad, statistic = function(d,i){median(d[i])}, 2000)

mean.boot.heat <- boot(energy$HeatingLoad, statistic = function(d,i){mean(d[i])}, 2000)

median.boot.heat <- boot(energy$HeatingLoad, statistic = function(d,i){median(d[i])}, 2000)

# Histogram and QQ plot

plot(mean.boot)
plot(median.boot)
hist(mean.boot)
# Bootstrap CI

plot(mean.boot.heat)
plot(median.boot.heat)
quantile( mean.boot$t, 0.025)
quantile(median.boot$t, 0.025)
quantile(median.boot.heat$t, 0.025)
#2.5%

quantile(mean.boot$t, 0.975)
quantile(median.boot$t, 0.975)

quantile(median.boot.heat$t, 0.975)



# 2000 bootstrap samples for median

#********************************************
#
#			Regression Example
#
#********************************************



#Step 1 Build a linear model 
#Analyze the result: R^2, model utility test, t-tests, etc. 

energy.lm1 <- lm(CoolingLoad ~ Compactness + RoofArea + GlazingArea, data = energy)


##Surface Area, Wall Area, Overall Height, Orientation, Glazing Area, and Glazing Area Distribution

energy.lm2 <- lm(HeatingLoad ~ SurfaceArea  + WallArea+ Height + Orientation + GlazingArea + GADistribution, data = energy)


summary(energy.lm2)

summary(energy.lm1)

par(mfrow = c(2,2))
plot(energy.lm2)
par(mfrow = c(1,1))
#Step 2 Generate the diagnostic plots. Do you see any problems? 


par(mfrow=c(2,2))
plot(energy.lm1)
par(mfrow=c(1,1))

#Step 3 Estimate the model with bootstrapping (by residuals). Is b1 significant? 
#	Get the fitted values from the regression model

energy.fit <- fitted(energy.lm1)

energy.fit.2 <- fitted(energy.lm2)

#	Get the residuals from the regression model

energy.resid <- residuals(energy.lm1)
energy.resid.2 <- residuals(energy.lm2)

#	Get the regression model 


energy.mod <- model.matrix(energy.lm1)

energy.mod.2 <- model.matrix(energy.lm2)

#   Bootstrapping LM
# Use RTSB(resp, pred, fit, resid, X, num)
#  	resp: the response
#	pred: the predictor(s)
#	fit: the fit of the linear model on the data
#	resid: the residual from the fit without the time series
#	X: the model with the predictors and time series.)
# 	num: number of bootstrap replicates

energy.boot <- RTSB(energy$CoolingLoad, energy[,c("Compactness", "RoofArea","GlazingArea")], energy.fit, energy.resid, energy.mod, 2000)

energy.boot.2 <- RTSB(energy$HeatingLoad, energy[,c("SurfaceArea","WallArea", "Height","Orientation", "GlazingArea", "GADistribution")], energy.fit.2, energy.resid.2, energy.mod.2, 2000)

energy.boot.2

boot.var <- apply(energy.boot$t, 2, var)

boot.var.2 <- apply(energy.boot.2$t, 2, var)

#	95%  CI for beta 1

boot.ci(energy.boot, .95, index=2, type = "norm")
boot.ci(energy.boot.2, .95, index=2, type = "norm")

#	Distribution of beta 1
par(mfrow = c(1,2))
hist(energy.boot$t[,2], main = "Compactness",xlab ="Coefficient Values",   col = "steelblue", breaks = 50)
qqnorm(energy.boot $t[,2])
qqline(energy.boot $t[,2])
par(mfrow = c(1,1))



# Plot of the 95% CI for beta 1

hist(energy.boot$t[,2], main = "Compactness", xlab = "Coefficient", col = "steelblue")

abline(v = boot.ci(energy.boot, .95, index=2, type = "norm")$norm[,3])

abline(v = boot.ci(energy.boot, .95, index=2, type = "norm")$norm[,2])


#*************************************
# Forecasting Energy Load


# For this you need new data

# Create your new data set
# We call this one newdata1

newdata1 <- data.frame(Compactness = .6, RoofArea = 140, GlazingArea = 3 )
#glazing area = 0.4, the glazing area distribution = 5, the Orientation = 1

newdata2 <- data.frame(SurfaceArea = mean(energy$SurfaceArea),WallArea = mean(energy$WallArea), Height = mean(energy$Height),Orientation = 1, GlazingArea = .4, GADistribution = 5)
# Bootstrapping the forecast

predGlaze <- RFB(energy, energy.lm1, ndata = newdata1)

predGlaze.2 <- RFB(energy, energy.lm2, ndata = newdata2)
# Basic bootstrap prediction limits would be

quantile(predGlaze$t, c(.025, .975))
quantile(predGlaze.2$t, c(.025, .975))
# mean of forecast

mean(predGlaze$t)
mean(predGlaze.2$t)
median(predGlaze.2$t)
