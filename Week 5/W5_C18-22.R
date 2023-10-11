setwd("C:/Users/user/Desktop/R (Paris Sud)/data")
alc <- read.csv2("alcool.csv")
# View data
str(alc)
# call survival library
library(survival)
# plot the survival function
plot(survfit(Surv(alc$t,alc$SEVRE)~1), mark.time=TRUE, main="Courbe de maintien dans l'abstinence")
# plot the survival function with sex as subset
plot(survfit(Surv(t,SEVRE)~SEXE, data=alc), mark.time=TRUE, col=c('black','red'), main="Courbe de maintien dans l'abstinence")
# get the median survival
survfit(Surv(t,SEVRE)~1, data=alc)
# long-rank test to compare if the difference between the two survival curves is statistically significant
# the long-rank test is only valid if we have a lot of data or a lot of events
diff <- survdiff(Surv(t,SEVRE)~SEXE, data=alc)
p <- pchisq(diff$chisq, length(diff$n)-1, lower.tail = FALSE)
sprintf("%.3f",p)
# Cox model to test the association between survival and other variables
coxph(formula = Surv(t,SEVRE)~AGE, data=alc)
# for multi-variable model similar syntax to regression
mod <- coxph(formula = Surv(t,SEVRE)~AGE+SEXE+EDVNEG, data=alc)
mod
# the exponential of the coefficients represent the "hazard ratio" for binary variables
exp(coef(mod))
# 0.64 for EDVNEG means we have 36% less chance of having an event if EDVNEG is positive
# the Cox model is only valid if we have sufficient data more than 5~10 per variable and hazard ratios are assumed to be proportional
# to verify proportional hazard ratio model assumptions, we use beta plots
par(mfrow=c(2,2))
plot(cox.zph(mod))
# the beta plots must be almost horizontal

# Correlation study for SMP
smp <- read.csv2("smp2.csv")
# set variables to correlate
var <- c("age", "n.enfant", "scz.cons", "dep.cons", "grav.cons", "rs", "ed", "dr")
# compute correlation matrix
corr.matrix <- cor(smp[, var], use = "complete.obs") 
round(corr.matrix, digits = 3)
# the option use = "complete.obs" only considers rows that have no missing data
# another option is use = "pairwise.complete.obs" considers observations that are pairwise complete
# to graph the correlation matrix
library(corrplot)
corrplot(corr.matrix, method = "circle")
# another what is to do PCA
library(psy)
mdspca(smp[, var])
# only the point that are near the circle are interpretable
# point near the circle cannot be studied
# another method is to graph the correlations on a sphere instead of a plane
sphpca(smp[, var])
# to rotate the sphere around the vertical by 55 degrees, we add the parameter v=55
sphpca(smp[, var], v=55)
# however the sphere is hard to analyze
# we use a better representation
dep.var <- "grav.cons"
ind.var <- c("age", "n.enfant", "scz.cons", "dep.cons", "rs", "ed", "dr")
fpca(data = smp, y = dep.var, x = ind.var, partial = "No")
# PCA analysis is not good for more than 15 independent variable
# We can use clustering to study correlations for more than 15 independent variable
cha <- hclust(dist(t(scale(smp[, var]))), method = "ward.D")
# plot the cluster
plot(cha, xlab = "", ylab = "", main = "Classification")
# mix correlation graph with classification
obj <- cor(smp[, var], use = "pairwise.complete.obs")
heatmap(obj, col = gray(seq(1,0,length=16)))