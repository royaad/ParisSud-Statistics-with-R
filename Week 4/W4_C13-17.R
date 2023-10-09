setwd("C:/Users/user/Desktop/R (Paris Sud)/data")
smp <- read.csv2("smp2.csv")
# View data
str(smp)
# plot dur.interv vs age
plot(smp$age,smp$dur.interv)
# add some noise to remove overlap, increase the factor to 4 for dur.interv
plot(jitter(smp$age),jitter(smp$dur.interv, factor = 4))
# do a simple linear regression
mod1 <- lm(dur.interv~age, data = smp)
# plot the line using abline
abline(mod1, lwd = 2, col = "red")
# show the fit parameters
summary(mod1)
# linear regression is just generalized case of correlation test
cor.test(smp$dur.interv,smp$age)
# we can see that we have the same p values
# the correlation coefficient 0.08573358 is linked to the slope by
coefficients(mod1)[2]*sd(smp$age,na.rm = TRUE)/sd(smp$dur.interv,na.rm = TRUE)
# which gives 0.08524562 ~ 0.08573358
# we can also use linear regression to test correlation between binary variable
plot(smp$dep.cons,smp$dur.interv)
mod2 <- lm(dur.interv~dep.cons, data = smp)
abline(mod2, lwd = 2, col = "red")
summary(mod2)
# linear regression here is a generalized t-test 
t.test(smp$dur.interv~smp$dep.cons, var.equal = TRUE)
# we see that we have the same p value and the slope corresponds to the difference between the two means

# we can realize multi-variable linear regression with the same function
mod3 <- lm(dur.interv~age+dep.cons+subst.cons+scz.cons+prof, data = smp)
summary(mod3)
# for prof it is made of characters. R automatically encodes it into n-1 binary lists (i.e. dummies)
# the reference dummy is chosen alphabetically.
# if we want to change the reference dummy, we can use
smp$prof <- relevel(as.factor(smp$prof),ref = "ouvrier")
# we can also use the option stringsAsFactors=TRUE when using read.csv2
# this will directly read strings as factors
mod4 <- lm(dur.interv~age+dep.cons+subst.cons+scz.cons+prof, data = smp)
summary(mod4)
# prof is made out of 8 classes, so we have 7 coefficients
# to summarize them into 1, we can use drop1
drop1(mod4,.~.,test = "F")
# to add interaction, we simply use * in lm
mod5 <- lm(dur.interv~age+dep.cons*subst.cons+scz.cons, data = smp)
summary(mod5)
# there doesn't seem to be any significant interaction
# ANOVA test is just a special case of Multi-variable linear regression when all X variable are categorical
mod6 <- lm(dur.interv~prof, data = smp)
summary(mod6)
drop1(mod6,.~.,test = "F")
# for a linear regression to be valid, we must verify the normality of the residuals
mod3 <- lm(dur.interv~age+dep.cons+subst.cons+scz.cons, data = smp)
hist(resid(mod3))
qqnorm(resid(mod3))
qqline(resid(mod3))

# for logistic regression we can use glm
plot(jitter(smp$abus),jitter(smp$suicide.hr))
mod1 <- glm(suicide.hr~abus,data = smp,family = "binomial")
summary(mod1)
# the coefficient by itself means nothing but its exponential is the odds ratio
exp(coefficients(mod1)[2]) # we get 2.1571...
# if we apply the twoby2
library(Epi)
twoby2(1-smp$suicide.hr,1-smp$abus) # we get an odds ratio 2.1571...
# for multi-variable logistic regression, we use again glm
mod2 <- glm(suicide.hr~abus+discip+duree,data = smp,family = "binomial")
summary(mod2)
# it is the exponential of coefficients that holds meaning
exp(coefficients(mod2))
# if the variable has multiple classes, dummies are created
mod3 <- glm(suicide.hr~prof,data = smp,family = "binomial")
summary(mod3)
# to simplify, we use drop1
drop1(mod3,.~.,test = "Chisq")
# for interaction we use *
mod4 <- glm(suicide.hr~abus+discip*duree,data = smp,family = "binomial")
summary(mod4)

# Quiz
## 4. We consider the age (age) of individuals who have 4 or more children (n.enfant) and whose socio-professional category (prof) belongs to the following categories: 'unemployed,' 'worker,' 'manager,' and 'employee.' For this subset of the smp data frame sample, the ratio between the two most extreme variances in these 4 groups is:
### check profession
table(smp$prof)
## create the subset
subset1 <- subset(smp, n.enfant >= 4 & (prof == "sans emploi" | prof == "ouvrier" | 
                    prof == "cadre" | prof == "employe"), c(age, n.enfant, prof))
### verify the results
summary(subset1)
### aggregate age by prof and compute the variance
aggregate(age ~ prof, data=subset1, var)
### ~281.22/88.22

## 5. We wish to perform a one-way ANOVA, considering age as the response variable, and the size of the siblings (n.siblings) recoded into 3 classes (0-2, 3-4, 5+) as the explanatory variable. The interval boundaries are included for each of the three classes. Please indicate the result of the Fisher-Snedecor F-test for equality of means.
### check levels of n.fratrie
smp$n.fratrie.cat<-factor(smp$n.fratrie)
table(smp$n.fratrie.cat)
levels(smp$n.fratrie.cat)
### change levels of n.fratrie
levels(smp$n.fratrie.cat)[1:3]<-"0-2"
levels(smp$n.fratrie.cat)[2:3]<-"3-4"
levels(smp$n.fratrie.cat)[3:18]<-"5+"
table(smp$n.fratrie.cat)
### fit with linear regression
qmod1 <-lm(age~n.fratrie.cat, smp)
summary(qmod1)
### we can use
anova(qmod1)
### or
drop1(qmod1,.~., test = "F")

## 6.We are interested in the relationship between the separation variable (separation) and the age (age) of individuals, which is modeled using logistic regression. Provide the lower bound of the 95% confidence interval for the odds ratio (3 decimal places).
qmod2 <- glm(separation~age, data = smp, family=binomial("logit"))
summary(qmod2)
confint(qmod2) # to get the confidence intervals
exp(confint(qmod2)[2]) # to get the odds ratio

## 3. For the simple linear regression model in which the interview duration variable (dur.interv) serves as the dependent variable and the age variable (age) as the independent variable, indicate the upper bound of the 90% confidence interval for the regression slope (3 digits after the decimal point)
qmod3 <- lm(dur.interv~age, data = smp)
confint(qmod3,level = .9)

## 4. Using the logistic regression model, considering the variable 'suicide.hr' as the dependent variable and 'age' as the explanatory variable, and only considering individuals for whom 'dep.cons' equals 1, indicate the probability of high suicidal risk for a 35-year-old individual (3 digits after the decimal point). [difficult question]
qmod4 <- glm(suicide.hr~age, data = smp, family = binomial("logit"), subset = dep.cons == 1)
summary(qmod4)
newdata = data.frame(age=35)
predict(qmod4, newdata, type = "response")