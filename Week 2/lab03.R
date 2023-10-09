##
## Lab 3
##

# set working directory
setwd("C:/Users/user/Desktop/R (Paris Sud)/Week 1/data")
# load data to variable smp
smp <- read.csv2("smp2.csv")

smp$n.enfant.cat <- factor(smp$n.enfant) # treat the number as a factor/catergory and add it to smp
levels(smp$n.enfant.cat)[6:13] <- "5+"
table(smp$n.enfant.cat)
barplot(prop.table(tab) * 100, ylim=c(0,30), las=1)