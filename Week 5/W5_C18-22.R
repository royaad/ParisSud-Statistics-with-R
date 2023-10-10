setwd("C:/Users/user/Desktop/R (Paris Sud)/data")
alc <- read.csv2("alcool.csv")
# View data
str(alc)
# call survival library
library(survival)
plot(survfit(Surv(alc$t,alc$SEVRE)~1), mark.time=TRUE, main="Courbe de maintien dans l'abstinence")
# sex subset
plot(survfit(Surv(t,SEVRE)~SEXE, data=alc), mark.time=TRUE, col=c('black','red'), main="Courbe de maintien dans l'abstinence")
# survival median
survfit(Surv(t,SEVRE)~1, data=alc)
