##
## Lab 2
##

load("smp_v1.rda")

names(smp)

smp$age[1]

smp[1,1] # the second index is the variable or column index

smp[1,"age"] # an equivalent way is to use the name of the column

head(smp$prof)

head(smp$prof == "agriculteur") # returns a Boolean checking if the value is "agriculteur"

table(smp$prof == "agriculteur")

which(smp$prof == "agriculteur") # returns indices for variable prof equal to "agriculteur"

smp$age[which(smp$prof == "agriculteur")] # returns the age correponding to "agriculteur"

subset(smp, prof == "agriculteur", age) # does the same as line 21

subset(smp, prof == "agriculteur", 1:5) # returns the values of the first 5 columns corresponding to "agriculteur"

names(smp)[1:5]

subset(smp, prof == "agriculteur", c(age, duree, discip, n.enfant)) # does the same as line 27, only here we specify the columns by their name

subset(smp, prof == "agriculteur" & n.enfant > 2, c(age, duree, discip, n.enfant)) # here we have 2 conditions

subset(smp, prof == "agriculteur" & n.enfant > 2 & complete.cases(duree), c(age, duree, discip, n.enfant)) # complete.cases returns FALSE if NA, so with this command rows that have "duree" == NA are excluded

subset(smp, prof == "agriculteur" & n.enfant > 2 & complete.cases(duree), c(age, duree, discip, n.enfant)) # complete.cases returns FALSE if NA, so with this command rows that have "duree" == NA are excluded

table(smp$n.enfant.cat)

tab <- table(smp$n.enfant.cat)

sum(tab) # get the count of all observations

sum(smp$ecole<=3, na.rm = TRUE) # get the number of variables having less than 3 schools, we have no 0 schools...

tab / sum(tab)

prop.table(tab) # get the proportion of each unique observation

round(prop.table(tab),3)

round(prop.table(tab),2)

barplot(prop.table(tab) * 100)

barplot(prop.table(tab) * 100, ylim=c(0,30))

barplot(prop.table(tab) * 100, ylim=c(0,30), las=1)

head(smp$age)

summary(smp$age)

hist(smp$age)

hist(smp$age, nclass=8)

hist(smp$age, nclass=8, prob=TRUE)

hist(smp$age, nclass=8, prob=TRUE, las=1)

lines(density(smp$age, na.rm=TRUE))
