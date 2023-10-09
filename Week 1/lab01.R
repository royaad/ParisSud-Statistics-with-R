##
## Lab 1
##

# set working directory
setwd("C:/Users/user/Desktop/R (Paris Sud)/data")
# load data to variable smp
smp <- read.csv2("smp2.csv", stringsAsFactors = TRUE)

View(smp) # view smp in a spreadsheet tab

names(smp) # get the names of the dataframe variables

str(smp) # display the dataframe

summary(smp) # get a statistical summary of the variables

summary(smp$age) # get a statistical summary for the age variable

smp$age # access the data of the age variable

smp$age[1] # access the first value of the age variable

smp$age[1:10] # access the first 10 values of the age variable

min(smp$age) # get the minimum of the age variable

help(min) # get help on how to use the function min

min(smp$age, na.rm=TRUE) # na.rm = TRUE removes the missing values, if it is not specified and age has missing values NA is returned

smp$abus[1:10]

unique(smp$abus) # returns the unique values of the variable "abus"

head(smp$abus, n=10) # returns the first 10 values of "abus" equal to smp$abus[1:10]

length(smp$abus) # returns the length (n. of observations) for "abus"

nrow(smp) # returns the number of rows (usually equal to length)

table(smp$abus) # returns a frequency table of the varius unique values of "abus"

table(smp$abus,useNA="always") # useNA="always" in order to return NA count

summary(smp$abus)

head(smp$abus)

head(factor(smp$abus)) # factor is used to encode a vector as a factor/category

abus <- factor(smp$abus,levels=c(0,1),labels=c("Non","Oui")) # the levels 0 and 1 are given labels No and Yes

table(abus)

names(smp)

head(smp$n.enfant)

summary(smp$n.enfant)

summary(smp$dur.interv) #

table(smp$n.enfant)

table(smp$prof) #

table(smp$n.enfant>4) # returns a frequency count of number of children bigger or less and equal to 4 

smp$n.enfant.cat <- factor(smp$n.enfant) # treat the number as a factor/catergory and add it to smp

table(smp$n.enfant)

levels(smp$n.enfant.cat) # get the levels of the variable

nlevels(smp$n.enfant.cat)

levels(smp$n.enfant.cat)[6:13] <- "5+"

table(smp$n.enfant.cat)

save(smp,file="smp_v1.rda")

savehistory("commandes.R")

## Quiz
sum(smp$n.fratrie >= 5) # number of fratrie bigger or equal to 5 -> 307
sum(smp$n.fratrie < 5) # number of fratrie less than 5 -> 492
sum(complete.cases(smp$n.fratrie)) # total number of fratrie excluding missing NA -> 799
# the following line returns a factor with labels not specified the default label is sort(unique(smp$n.fratrie >= 5)) in that case "False" and "TRUE"
table(factor(smp$n.fratrie >= 5, labels=c('<5', '5+')))
# the following line does not work well in this order
smp$n.fratrie[smp$n.fratrie < 5] <-'<5'
smp$n.fratrie[smp$n.fratrie >= 5] <- '5+'
table(smp$n.fratrie)
describe(smp$n.fratrie)
# Quiz
## place has 7 NAs
table(smp$place, useNA = "always")
place <- factor(smp$place, labels=c('Non', 'Oui')) # by defauls 0 is Non and 1 is Oui (sorted) and NA are left as NA
table(place, useNA = "always")
# Quiz
## age mean of the first 10 observations
mean(smp$age[1:10], na.rm = TRUE)
## dur.inter median of the first 300 observations
median(smp$dur.interv[1:300], na.rm = TRUE)
