

#********************************************************************************
#
#					Univariate Graphics
#
#********************************************************************************


#************************************
# Reading in data
#************************************

# Set working directory

# For example, an iOS user

#setwd("/Users/me/data/TrainAccidents")

# or a windows user

setwd("D:\\R")

mydatapath14 <- "R_data\\RailAccidents14.txt" 
mysourcepathAI <- "R_Code\\AccidentInput.R"
mylistpath <- "D:\\R\\R_data"

#***********************************************

# Read in the accident files one at at time


acts14 <- read.table(mydatapath14,sep = ",",header = TRUE)

# Since the files are really csv you can use

acts14 <- read.csv(mydatapath14)


#**************************************************

# To get a summary of all of the variables use

summary(acts14)
# To get a summary of a subset of the variables (e.g., "ACCDMG", "TOTKLD", "CARS" ) 
# you can use

summary(acts14$ACCDMG,acts14$TOTKLD,acts14$CARS)

# To get individual statistics (e.g. mean, var) you can use
mean(acts14$ACCDMG)
var(acts14$ACCDMG, na.rm = FALSE, use = "complete")
mean(acts14$TOTKLD)
var(acts14$TOTKLD, na.rm = FALSE, use = "complete")
mean(acts14$CARS)
var(acts14$CARS)


# You can round your answer using the round() function


#**************************************************

# You will need to read in all 14 years of the data 
# You will put the data into a data structure called a list

# To do this you will use code I have written, AccidentInput.R 
# Put that file in your working directory and then source it:

source(mysourcepathAI)


# Now use it to read in all the data. You must have ALL and ONLY the rail accident data
# files in one directory. Call that directory and its path, path.
# You can give your data a name
# In my examples below I use acts as the name for data sets
# Then use the command

acts <- file.inputl(mylistpath) 


# E.G.

#acts <- file.inputl("C:\\Users\\james Bennett\\Desktop\\R_Code\\AccidentInput.R")

# path is the specification of the path to your file.

# Now acts[[1]] is the data set from year 2001, 
# acts[[2]] is the data set from year 2002, etc.

# Before we put all the data into one data frame
# we must clean the data


##################################################
#
#	Data Cleaning
#
##################################################

#************************************************
# Variable names

matrix(names(acts[[1]]))

matrix(names(acts[[8]]))

# Notice that the number of columns changes from year to year - 146 vs 140

ncol(acts[[1]])
ncol(acts[[8]])


# Get a common set the variables
	
	comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))

	
	# Now combine the data frames for all 12 years
	# Use combine.data()
		
		
	totacts <- combine.data(acts, comvar)

# How many accidents? 46883

	dim(totacts)

# View of accident damage - Most of them are minor accideint 
par(mfcol=c(1,1), oma=c(1,0,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(2,0,0))
boxplot(totacts$ACCDMG,range = 1.5, main = "Boxplot of accident damage")

hist(totacts$ACCDMG)

#*************************************************
# Accident Reports

# Look at the most costly accident in the data set

which(totacts$ACCDMG == max(totacts$ACCDMG))


# Check out the narratives for this extreme accident

totacts[42881,]

# How do we find duplicates?

# Are there other duplicates?

duplicated(totacts[1:100, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])


# why not use latitude and longitude? - Because date and time does the trick - what are the chances we had had accidents at the same time?!

# why not use INCDTNO? - it is not unique to an incident - We need a primary key - Think DB 

totacts[totacts$INCDTNO == "1", 1:10]


# Remove duplicates

totacts <- totacts[!duplicated(totacts[, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]),]

#*******************************************
# What is the second most extreme accident?
which(totacts$ACCDMG > 1.5e7)


# what should we do? - We remove it - This was a result of a terrorist attack - cannot be expected to controll this

totacts <- totacts[-1223,]

#********************************************
# Missing data

# Do a summary of totacts

names(summary(totacts$Latitude))


# Are we missing values for any variables?


# How many?

nafind <- function(x){sum(is.na(x))}

apply(totacts,2, "nafind")

# Do we need all the variables?

matrix(names(totacts))

# Remove unnecessary variables, then get a summary

nacount <- apply(totacts,2, "nafind")

varWna <- which(nacount > 0)

# Keep TYPEQ, we'll use it. The others we don't need.

which(colnames(totacts)[varWna] == "TYPEQ")

varWna <- varWna[-which(colnames(totacts)[varWna]== "TYPEQ")]


totacts <- totacts[, -varWna]

# Save your data frame

# check you working directory and change it if necessary
getwd()

write.csv(totacts, file ="D:\\R\\R_Output\\totactsClean.csv", row.names = F)

#***********************************
#
# 	Summary of accidents
#
#***********************************

# Get a summary of the cleaned data set
# It does not seem to address the expense associated with Bodily Injuries or Deaths or litigation from deaths or injuries 

summary(totacts)
# How many accidents?

dim(totacts)

# Total cost of all accidents
sum(totacts$ACCDMG)
#summary(totacts$CAUSE)
# Average annual cost of accidents
sum(totacts$ACCDMG)/14
# first yearly costs (sums)
dmgyrsum <- tapply(totacts$ACCDMG, totacts$YEAR, sum)

##what about the cost for injuries and deaths
## for instance

which(totacts$TOTKLD == max(totacts$TOTKLD))
pd_dmg <- sum(totacts$EQPDMG[15012],totacts$TRKDMG[15012])

discrepancy <- totacts$ACCDMG[15012] - pd_dmg
discrepancy
## of 592800 - but there were 9 deaths and 292 injuries ??

#then average

mean(dmgyrsum)
# Total number killed
sum(totacts$TOTKLD)

# Largest number killed in an accident - But the data shows that 
max(totacts$TOTKLD)

# Total number injured
sum(totacts$TOTINJ)


# Largest number injured in an accident 

max(totacts$TOTINJ)
# What is the average number of injuries per year?
round(sum(totacts$TOTINJ)/14)

# types of variables

str(totacts)

#**************************************************
#
#   Time series of Accidents
#
#**************************************************

# Yearly no. of accidents

plot(1:max(totacts$YEAR), tapply(totacts$ACCDMG, totacts$YEAR, length), type = "l", col = "black", xlab = "Year", ylab = "Frequency", main = "Number of Accidents per Year", lwd =2)


# Yearly total cost of accidents

plot(1:max(totacts$YEAR), tapply(totacts$ACCDMG, totacts$YEAR, sum), type = "l", col = "black", xlab = "Year", ylab = "Cost ($)", main = "Total Damage per Year", lwd =2)

# Yearly maximum cost of accidents

plot(1:max(totacts$YEAR), tapply(totacts$ACCDMG, totacts$YEAR, max), type = "l", col = "black", xlab = "Year", ylab = "Cost ($)", main = "Total Damage per Year", lwd =2)

# Putting total and maximum together using symbols 

symbols(2001:2014, tapply(totacts$ACCDMG, totacts$YEAR, sum), circles=tapply(totacts$ACCDMG, totacts$YEAR, max),inches=0.35, fg="white", bg="red", xlab="Year", ylab="Cost ($)", main = "Total Accident Damage")
lines(2001:2014, tapply(totacts$ACCDMG, totacts$YEAR, sum))



# Repeat this for total killed and total injured and the sum of them.
symbols(2001:2014, tapply(totacts$TOTKLD, totacts$YEAR, sum), circles = tapply(totacts$TOTKLD, totacts$YEAR, max), inches = 0.35, fg ="yellow", bg ="red", xlab = "Year", ylab = "People Killed", main = "Total Killed")
lines(2001:2014, tapply(totacts$TOTKLD, totacts$YEAR, sum))

symbols(2001:2014, tapply(totacts$TOTINJ, totacts$YEAR, sum), circles = tapply(totacts$TOTINJ, totacts$YEAR, max), inches = 0.35, fg ="black", bg ="red", xlab = "Year", ylab = "People Injured", main = "Total Injured")
lines(2001:2014, tapply(totacts$TOTINJ, totacts$YEAR, sum))


#***********************************
#
# 	histograms of ACCDMG and TEMP
#
#***********************************

# These examples are for 2011 

hist(acts[[11]]$ACCDMG) # for 2011

hist(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")


# Different bin widths

par(mfrow = c(2,2))

hist(totacts$TEMP, breaks = "scott", main = "Accident Temperatures (Scott)", xlab = "Temp (F)", col = "steelblue")

hist(totacts$TEMP, breaks = "fd", main = "Accident Temperatures (FD)", xlab = "Temp (F)", col = "steelblue")

hist(totacts$TEMP, main = "Accident Temperatures (Sturges)", xlab = "Temp (F)", col = "steelblue")

hist(totacts$TEMP, breaks = 100, main = "Accident Temperatures (100)", xlab = "Temp (F)", col = "steelblue")

par(mfrow = c(1,1))

# Different bin widths

hist(acts[[11]]$ACCDMG, breaks = "scott", main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

hist(acts[[11]]$ACCDMG, breaks = "fd", main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

hist(acts[[11]]$ACCDMG, breaks = 20, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

hist(acts[[11]]$ACCDMG, breaks = 100, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")


# other years


par(mfrow = c(2,2))
hist(acts[[1]]$ACCDMG, main = "Total Accident Damage in 2001", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[4]]$ACCDMG, main = "Total Accident Damage in 2004", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[8]]$ACCDMG, main = "Total Accident Damage in 2008", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")
par(mfrow = c(1,1))



#*********************************************************************
#
# 				Box Plots of Metrics
#         and Extreme Accidents
#
#*********************************************************************

#*****************************
# ACCDMG

boxplot(totacts$ACCDMG, main = "Xtreme Accident damage")
boxplot(totacts$TOTKLD, main = "Deaths in Extreme incident")

# Plot only the extreme points
# (extreme defined by the box plot rule)

# Get the values in the box plot

dmgbox <- boxplot(totacts$ACCDMG)

dmgbox2 <- boxplot(totacts$TOTKLD)

# How many extreme damage accidents?

length(dmgbox$out)
##extreme accident dmg 4862
length(dmgbox2$out)
dmgbox$stats
##extreme accident relative to deaths 479 (this is not commom)

# What proportion of accidents are extreme? (round to 2 digits) - 13%
round(length(dmgbox$out)/length(totacts$ACCDMG),2)

# What is the proportion of costs for extreme damage accidents? (round to 2 digits)
round(sum(dmgbox$out)/sum(totacts$ACCDMG),2) ##13% causes 74% of the damages - Insanity!!
# Create a data frame with just the extreme ACCDMG accidents
round(length(dmgbox2$out)/length(totacts$TOTKLD),2)
##.01 are extreme - deaths are an wear event
round(sum(dmgbox2$out)/sum(totacts$TOTKLD),2)

##all deaths are were events

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

dim(xdmg)
###4862 are were 

# Look at the boxplots and histograms of these extreme accidents

boxplot(xdmg$ACCDMG, col = "steelblue", main = "Accidents with Extreme Damage", ylab = "Cost ($)")

plot(1:14, tapply(xdmg$ACCDMG, xdmg$YEAR, sum), type = "l", xlab = "Year", ylab = "Total Damage ($)", main = "Total Extreme Accident Damage per Year")

# also plot number of accidents per year.

plot(1:14, tapply(xdmg$ACCDMG, xdmg$YEAR, length), type = "l", xlab = "Year", ylab = "No. of Accidents", main = "Number of Extreme Accidents per Year")

# Frequency of accident types

barplot(table(xdmg$TYPE)) #compare with the totacts plot
##Lots of Derailments - wonder is speeding has to do with this - Type = 1

# Repeat for TOTKLD and TOTINJ
# Create a variable called Casualty = TOTKLD + TOTINJ

max(totacts$TOTINJ) ##1000 in a single accident 
max(totacts$TOTKLD) ##9 in a single maccident
Casualidad = totacts$TOTKLD + totacts$TOTINJ
max(Casualidad) ###1001
plot(1:max(totacts$YEAR), tapply(totacts$TOTKLD, totacts$YEAR, max), type = "l", col = "black", xlab = "Year", ylab = "Frequency", main = "Number of KILLED", lwd =2)
plot(1:max(totacts$YEAR), tapply(totacts$TOTINJ, totacts$YEAR, max), type = "l", col = "black", xlab = "Year", ylab = "Frequency", main = "Number of Injured", lwd =2)
plot(1:max(totacts$YEAR), tapply(Casualidad, totacts$YEAR, max), type = "l", col = "blue", xlab = "Year", ylab = "Frequency", main = "Combined Casualties", lwd =2)

