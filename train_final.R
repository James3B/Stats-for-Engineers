

#########
setwd("D:/R/R_Code")
source("TSbootfunctions.R")
source("SPM_Panel.R")
source("FactorPlots.R")
source("PCAplots.R")
source("pc.glm.R")
source("ROC.R")
source("TestSet.R")


xdmg.train <- read.csv("D:\\R\\R_final\\xdmg.train.csv", header = T)
xdmg.test <-  read.csv("D:\\R\\R_final\\xdmg.test.csv", header = T)

str(xdmg.train)
str(xdmg.test)
summary(xdmg.train)
summary(xdmg.test)

par(mfrow = c(1,2))
for(i in 3:4)
{
  boxplot(xdmg.train[,i]~factor(xdmg.train[,5]), xlab = "Derailment",
          ylab = "Count", main = paste("Variable", i))
}
par(mfrow = c(1,1))

Tren.glm <- glm(CAUSE ~., data = xdmg.train, family = binomial)
summary(Tren.glm)
Tren.null <- glm(CAUSE~1, data=xdmg.train, family = binomial)
anova(Tren.glm,Tren.null, test = "Chi")
AIC(Tren.glm)
library(MASS)
dropterm(Tren.glm, test ="Chi")

tren_mas.glm <-update(Tren.glm, data=xdmg.train[,c(1,2,4)])
summary(tren_mas.glm)



set.seed(1234)

TAC <- test.set(xdmg.train, .33)



