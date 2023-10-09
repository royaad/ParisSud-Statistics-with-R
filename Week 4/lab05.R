##
## Lab 5
##

setwd("C:/Users/user/Desktop/R (Paris Sud)/data")
load("smp_v1.rda")

## create a subset for columns age, n.enfant, and prof which verify the conditions
smpb <- subset(smp, prof == "sans emploi" | prof == "prof.intermediaire" | 
                 prof == "cadre", c(age, n.enfant, prof))

## display the first 6 observations
head(smpb)

## display a summary
summary(smpb)
## we conclude that prof kept the levels agriculteur and others...
## to refresh the levels we use factor
smpb$prof <- factor(smpb$prof)
## we verify again
summary(smpb)
## we create a table
table(smpb$prof)

## to get the mean of n.enfant by prof
aggregate(n.enfant ~ prof, data=smpb, mean)

## for a boxplot
boxplot(n.enfant ~ prof, data=smpb, xlab="Profession", ylab="Nombre d'enfants",
         col="cornflowerblue", border="cornflowerblue",
         pars=list(medcol="white", whiskcol="cornflowerblue",
             staplecol="cornflowerblue", whisklty=1), lwd=1.5)

## ------------------------------------------------------------------------
mod1 <- lm(n.enfant ~ prof, data=smpb)
summary(mod1)
drop1(mod1, test="F")

## lm allow the use of subset as a parameter
mod1 <- lm(n.enfant ~ age, data=smpb)
summary(mod1)
## gives the same result as
lm(n.enfant ~ age, data=smp, subset = prof == "sans emploi" | prof == "prof.intermediaire" | 
     prof == "cadre")

## to display coefficients
coefficients(mod1)
coef(mod1)
coef(mod1)[2]
coef(mod1)["age"]

## to display confidence intervals
confint(mod1)
confint(mod1)[3]
confint(mod1)[1,2] # same as before

## to get 90% confidence intervals
confint(mod1,level = 0.9)

## to do anova test on mod1
anova(mod1)

## ------------------------------------------------------------------------
head(fitted(mod1))
head(resid(mod1))

## ------------------------------------------------------------------------
smp$n.enfant[1:2] - fitted(mod1)[1:2]

## ------------------------------------------------------------------------
predict(mod1, data.frame(age=c(20, 30, 40)), interval="confidence")

## encode n.enfant into binary
smp$n.enfant.bin <- factor(ifelse(smp$n.enfant > 2, 1, 0))
table(smp$n.enfant.bin)

## ------------------------------------------------------------------------
mod2 <- glm(n.enfant.bin ~ age, data=smp, family=binomial("logit"))
summary(mod2)

## ------------------------------------------------------------------------
coef(mod2)
coef(mod2)[2]
exp(coef(mod2)[2])

## ------------------------------------------------------------------------
exp(5*coef(mod2)[2])

## predicts the log of the odds ratio
head(predict(mod2))
## predicts the probability
head(predict(mod2, type="response"))
