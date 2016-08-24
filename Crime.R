

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
crimen <- read.table("D:\\R\\R_crime_data\\communitycrime.txt", sep = ',', header = F)

source("SPM_Panel.R")
source("FactorPlots.R")
source("PCAplots.R")
source("pc.glm.R")
source("ROC.R")

summary(crimen)
