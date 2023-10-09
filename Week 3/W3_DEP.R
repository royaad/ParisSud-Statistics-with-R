# load the data
setwd("C:/Users/user/Desktop/R (Paris Sud)/data")
sh <- read.csv2("satisfaction_hopital.csv")

# Q1. Transform the variable 'recommender' into a binary variable 'recommender.b':
# 'recommender.b' is equal to 0 if 'recommender' is 0 or 1;
# 'recommender.b' is equal to 1 if 'recommender' is 2.For the three categorical variables in the file (you determine which variables they are), present the percentages of subjects belonging to each of the categories.
## To transform, we use the function ifelse
sh$recommander.b <- ifelse(sh$recommander<2,0,1)
## Verify if the transformation was OK
table(sh$recommander,sh$recommander.b,deparse.level = 2,useNA = "always")

# Q2. Using an odds ratio, estimate the strength of the association between "recommender.b" and "sexe" Estimate a confidence interval for this odds ratio.
## We use the twoby2 function from the Epi library
library(Epi)
## there is no order for homme and femme so we can keep 0 and 1 and not flip them.
twoby2(sh$sexe,1-sh$recommander.b)
## there is not a strong association between recommender.b and sexe the odds ratio is 0.922 with a conf. interval of 0.6 to 1.4 and a p of 0.75.

# Q3. Calculate the (Pearson) correlation between 'score.relation' and 'age.' Statistically test this correlation (the script should include the possible verification of the validity conditions of the method used).
## Pearson is valid when one of the variable has a normal distribution.
## we check the distribution for age
hist(sh$age, xlab="age", main = "Age Distribution")
## We can also plot the qqplots for age
qqnorm(sh$age)
qqline(sh$age)
## The histogram and qqplots for age show that they fairly follow a normal distribution
## The age variable deviates from the normality line at its ends
## We check for score.relation
hist(sh$score.relation, xlab = "Relation Score", main = "score.relation Distribution")
## score.relation is highly non-normal. However, it is enough to have 1 normally distributed variable
## correlation calculation
cor.test(sh$age,sh$score.relation)

# Q4. Is the average relationship score significantly different between men and women? (The script should include the possible verification of the validity conditions of the method used).
## We want to check if the averages are different or not. We should use t-test or Wilcox.
## to use the t-test, the score.relation distribution should be normal and the variances should be almost the same.
hist(sh$score.relation, xlab = "Relation Score", main = "score.relation Distribution")
## We can check the qqplots for further investigation
qqnorm(sh$score.relation)
qqline(sh$score.relation)
## the distributions don't seem to be normally distributed
## let us check the st.dev for both anyway
tapply(sh$score.relation, sh$sexe, sd, na.rm=TRUE)
## the score.relation distribution doesn't seem to be normally distributed
## we will use wilcox
wilcox.test(sh$score.relation~sh$sexe)

#### Extra
sr.sexe <- table(sh$sexe,sh$score.relation)
barplot(sr.sexe, beside = TRUE, xlab = "Relation Score", ylab = "Frequency", main = "Relation Score Distribution", legend.text = c("Homme","Femme"), args.legend = list(x = "topleft", inset = c(0.01, 0.1)))
## for women
qqnorm(sh$score.relation[which(sh$sexe==1)])
qqline(sh$score.relation[which(sh$sexe==1)])