setwd("C:/Users/user/Desktop/R (Paris Sud)/data")
smp <- read.csv2("smp2.csv")
library(prettyR) 
describe(smp$age)
library(binom)
# get confidence intervals
binom.confint(3,10,method="all")
binom.confint(300,1000,method="all")
# compute correlation coefficient r
# x is age and y is n.enfant
# the parameter use="complete.obs" is necessary to have a number in case we have NAs
cor(smp$age,smp$n.enfant,use="complete.obs")
# create a binary vector with 1 for ed>2 and 0 for less
smp$ed.bin <- ifelse(smp$ed>2,1,0)
# verify if it's ok
table(smp$ed.bin,smp$ed,useNA="always",deparse.level = 2)
# compute the relative risk and odds ratio
library(Epi)
# by default twoby2 0 means yes and 1 is no
# Usually the order is reversed in tables, thus we need to flip 0s to 1s and vice-versa
# Therefore we use 1-x and 1-y in the twoby2 function
twoby2(1-smp$ed.bin,1-smp$dep.cons)

# Quiz 2
## 4. mean of n.enfant for depressif cases dep.con = 1
mean(smp[smp$dep.cons == 1, 'n.enfant'], na.rm = TRUE)
## 5. 3rd quartile for time of people with ages strictly below 35
summary(smp$duree[which(smp$age<35)])
summary(smp$duree)
## 6. mean of dur.interv for persons with a past suicide attempt.
mean(smp[smp$suicide.past == 1, 'dur.interv'], na.rm=TRUE)
## 7. make age into categorical based on quartiles and count the number of the 3rd class
summary(smp$age)
### summary returns Q1 = 28 and Q3 = 48. Q2 which is the median is 37
### another way to get quartiles
quartiles <- quantile(smp$age, probs = c(0,0.25,0.5,0.75,1), na.rm = TRUE)
### define breaks
smp$age.cat <- cut(smp$age, breaks=quartiles, labels=c('Q1', 'Q2', 'Q3', 'Q4'), include.lowest = TRUE)
### display table
table(smp$age.cat)
### check manually 
age.tab = table(smp$age)
sum(age.tab[1:10]) # Q1
sum(age.tab[11:19]) # Q2
sum(age.tab[20:30]) #Q3
sum(age.tab[31:length(age.tab)]) #Q4 
# test
## create data frame
df <- data.frame(team=c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
                 points=c(70, 80, 86, 90, 99, 104, 100, 110))
## view data frame
df
## add new column that cuts 'points' into categories
## breaks lower bound and exclusive and upper bound are inclusive by default
df$cat <- cut(df$points,
              breaks=c(70, 80, 90, 100, 110),
              labels=c('Bad', 'OK', 'Good', 'Great'))
## param include.lowest = TRUE makes only the lowest boundary inclusive
df$cat2 <- cut(df$points,
              breaks=c(70, 80, 90, 100, 110),
              labels=c('Bad', 'OK', 'Good', 'Great'), include.lowest = TRUE)
#view updated data frame
df