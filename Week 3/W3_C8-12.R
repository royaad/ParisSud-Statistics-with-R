setwd("C:/Users/user/Desktop/R (Paris Sud)/Week 1/data")
smp <- read.csv2("smp2.csv")
# create a binary vector with 1 for ed>2 and 0 for less
smp$ed.bin <- ifelse(smp$ed>2,1,0)
# compute the cross-table of depression vs danger avoiding
edxdep.tab <- table(smp$ed.bin,smp$dep.cons,deparse.level = 2)

# compute the probability normalize along ed.bin
prop.table(edxdep.tab, 1)
# compute the probability normalize along dep.cons
prop.table(edxdep.tab, 2)
# to compare the dependence of two variables using proportions, we use the chi^2 test
# compute chi^2 of the 2 variables
chisq.test(smp$ed.bin,smp$dep.cons,correct=FALSE)
# the order by which we specify the variable doesn't matter
chisq.test(smp$dep.cons,smp$ed.bin,correct=FALSE)
# In case the chi^2 conditions are not verified, the we use Fisher's test
fisher.test(smp$ed.bin,smp$dep.cons)
# To compare the difference between two variables using means, we use the t-test
# the t-test requires that the variables have a normal distribution and the same variance or std
# we can check for normality using histograms
# we should plot the histogram for each category/case
hist(smp$age)
# we can also use qqplots to check for normality
# we should plot the qqplots for each category/case
qqnorm(smp$age)
qqline(smp$age)
# we can also calculate the std of age for each category/case
by(smp$age,smp$ed.bin,sd,na.rm=TRUE)
# t-test for equal variance
t.test(smp$age~smp$ed.bin, var.equal=TRUE)
# if we cannot do the t-test, non-normal samples, we can use Wilcox
wilcox.test(smp$age~smp$ed.bin)
# to compute Pearson's correlation, at least one of the variables should follow the normal distribution
cor.test(smp$age,smp$rs)
# if not, we can use Spearman
cor.test(smp$age,smp$rs,method="spearman")
# to compare a mean to a value we use the t.test
t.test(smp$age, mu=24)


# Quiz
## All the following functions do the same thing
## they compute the mean of ages for the 2 sub-classes
aggregate(age~subst.cons,data=smp,mean,na.rm=TRUE)
with(smp, tapply(age, subst.cons, mean, na.rm=TRUE))
by(smp$age,smp$subst.cons,mean,na.rm=TRUE)
tapply(smp$age, smp$subst.cons, mean, na.rm=TRUE)
## median of duree between (dep.cons = 1) and (dep.cons = 0)
tapply(smp$dur.interv, smp$dep.cons, median, na.rm=TRUE)
## correlation between duree and age
cor.test(smp$age, smp$dur.interv)
## wilcoxon test for duree between suicide.past
wilcox.test(smp$dur.interv~smp$suicide.past)
## t-test for equal variance
t.test(smp$dur.interv~smp$dep.cons, var.equal=TRUE)


# Extra

#make this example reproducible
set.seed(1)

#create some fake data that follows a normal distribution
data <- rnorm(2000)

#create Q-Q plot
qqnorm(data)
qqline(data)

#make this example reproducible
set.seed(1)

#create some fake data that follows an exponential distribution
data <- rexp(2000, rate=3)

#create Q-Q plot
qqnorm(data)
qqline(data)