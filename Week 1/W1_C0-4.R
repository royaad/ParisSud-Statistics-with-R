# Basics
## set the working directory
setwd("C:/Users/user/Desktop/R (Paris Sud)/data")
## read csv
smp <- read.csv2("smp2.csv")
## display data
str(smp)
## display data only for column prof
str(smp$prof)
## import a library
library(psy)
## access R documentation
?read.csv2
?`^`
help.search("swirl")
help("factor")
## create a frequency table for prof and attribute it to prof_cnt
prof_cnt <- table(smp$prof)
## make a barplot of the frequency table
barplot(prof_cnt)
## make a pie chart of the table
pie(prof_cnt)
## create a histogram for age
hist(smp$age)
## customize the histogram
hist(smp$age, col="grey", main = "", xlab = "age")
## create a boxplot for age
boxplot(smp$age, xlab = "age")
## create a boxplot for age with rs as a subset
boxplot(smp$age~smp$rs, ylab = "age", xlab = "Recherche de Sensation")
## plot n.enfant vs. age
plot(smp$age, smp$n.enfant)
## add a jitter to the plot
plot(jitter(smp$age), jitter(smp$n.enfant))
## read a new csv
repdat <- read.csv2("outils_hdrs.csv")
## display it
str(repdat)
## call library gplots
library(gplots)
## realize a plotmeans of HDRS with VISIT as a subset
plotmeans(repdat$HDRS~repdat$VISIT, gap = 0, barcol = "black")
## realize an interaction plot of HDRS versus VISIT
interaction.plot(repdat$VISIT, repdat$NUMERO, repdat$HDRS, lty = 1, legend = FALSE)
## display a summary of smp data
summary(smp) 
## display a customized summary using prettyR library
library(prettyR) 
## standard describe function
describe(smp)
## customized describe function
describe(smp, num.desc = c("mean", "median", "min", "max", "sd", "valid.n"))
## compute the mean of age
## the na.rm param removes the missing values
## if na.rm = TRUE is not specified the mean function returns NA
mean(smp$age, na.rm = TRUE) 
## compute the standard deviation of age
sd(smp$age, na.rm = TRUE)
## create a frequency table for prof
## useNA="always" param is to do a count of NAs
## deparse.level = 2 param will show the variable names
table(smp$prof, deparse.level = 2, useNA = "always")