# load the data
setwd("C:/Users/user/Desktop/R (Paris Sud)/data")
sh <- read.csv2("satisfaction_hopital.csv")
# view a summary of the data
summary(sh)
# Q1. Estimate the linear regression model explaining the "score.relation" variable using the variables "age," "gender," "score.information," "health.improvement," "mood.improvement," "occupation," and "service." (The script should include the possible verification of the validity conditions of the method used.)
## we can realize multi-variable linear regression with the function lm
## service and profession are categorical, so we will change them using asfactor
sh$profession <- as.factor(sh$profession)
sh$service <- as.factor(sh$service)
## sex is categorical but is it binary so no need to change it
mod1 <- lm(score.relation~age+sexe+score.information+amelioration.sante+amelioration.moral+profession+service, data = sh)
summary(mod1)
## we see a strong positive association to age, score.information, and amelioration.moral
## to summarize them into 1, we can use drop1
drop1(mod1,.~.,test = "F")
## for a linear regression to be valid, we must verify the normality of the residuals
hist(resid(mod1))
qqnorm(resid(mod1))
qqline(resid(mod1))
## the residuals seem to fairly follow a normal distribution except for the lower part


# Q2. Estimate the logistic regression model explaining the variable 'recommend.b' by the variables 'age,' 'gender,' 'information score,' 'health improvement,' 'mood improvement,' 'profession,' and 'service.' Note that the variable 'recommend.b' is a transformation of the 'recommend' variable into a binary variable where 'recommend.b' is 0 if 'recommend' is 0 or 1, and 1 if 'recommend' is 2. (The script should include the possible verification of the validity conditions of the method used)."
## 'recommender.b' is equal to 0 if 'recommender' is 0 or 1;
## 'recommender.b' is equal to 1 if 'recommender' is 2.For the three categorical variables in the file (you determine which variables they are), present the percentages of subjects belonging to each of the categories.
## To transform, we use the function ifelse
sh$recommander.b <- ifelse(sh$recommander<2,0,1)
## Verify if the transformation was OK
table(sh$recommander,sh$recommander.b,deparse.level = 2,useNA = "always")
## for multi-variable logistic regression, we use again glm
mod2 <- glm(recommander.b~age+sexe+score.information+amelioration.sante+amelioration.moral+profession+service,data = sh,family = "binomial")
summary(mod2)
## we see a strong association to score.information and amelioration.sante
exp(coef(mod2))
## we have score.information 1.11 and amelioration.sante 1.57
## recommender.b increases by 11% with the every increase of score.information
## and increases 57% with with every increase of amelioration.sante
## to simplify, we use drop1
drop1(mod2,.~.,test = "Chisq")
## For the validity, the essential and the most important, we will remember that at least 5 to 10 events are needed per explanatory variable.
## checking the na.action
getOption("na.action")
## we see that the action is omit which removes observations that have any missing values
## we can also see in the glm message the following:
##   (220 observations deleted due to missingness)
## 534 - 220 = 314
## first let us take a look at the count of 0s and 1s, we will exclude observations missing any values
col <- c("recommander.b", "age", "sexe", "score.information", "amelioration.sante", "amelioration.moral", "profession", "service")
table(sh[complete.cases(sh[,col]),"recommander.b"])
## we have 215 (1) and 99 (0) which corresponds to 314 complete.obs
## we only consider the 215 observations
## in addition, we have:
## age (1), sexe(1), score.information (1), amelioration.sante (1),   
## amelioration.moral (1), profession (7), service (7).
## so we have 1+1+1+1+1+7+7 = 19 independent variable
## 19*10 = 190 < 215 the logistic regression is valid