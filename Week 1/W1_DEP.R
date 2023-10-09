# load the data
setwd("C:/Users/user/Desktop/R (Paris Sud)/Week 1/data")
sh <- read.csv2("satisfaction_hopital.csv")

# Q1. For the three categorical variables in the file (you determine which variables they are), present the percentages of subjects belonging to each of the categories.
## The three categorical variables are "service", "sexe", and "profession".
## They are discrete and they are qualitative measurements
## "service", "sexe", and "profession" are nominal variables with no particular order.
## N.B. the variables "amelioration.sante", "amelioration.moral", "recommander" can also be considered as categorical.
## They are discrete and qualitative. However, they are ordinal since they hold a certain order.

### For "service"
### We first a table of the frequencies and attribute it to service.tab
service.tab <- table(sh$service, deparse.level = 2, useNA = "always")
### We use the function prop.table to display the proportions
### We multiply by 100 to convert to percentages
### We use the round funtion to display only 1 decimal
round(prop.table(service.tab)*100, 1)

### For "sexe"
### We start by encoding the vector as a factor
### 0 is for "homme" and 1 is for "femme"
sh$sexe.cat <- factor(sh$sexe,labels = c("homme", "femme"))
### We create a table of the factors
sexe.tab <- table(sh$sexe.cat, deparse.level = 2, useNA = "always")
### We display the percentages
round(prop.table(sexe.tab)*100, 2)

### For profession
### We start by encoding the vector as a factor
sh$profession.cat <- factor(sh$profession,labels = c("agriculteur", "artisan", "cadre", "technicien", "employé", "ouvrier", "étudiant", "autre"))
### We create a table of the factors
profession.tab <- table(sh$profession.cat, deparse.level = 2, useNA = "always")
### We display the percentages
round(prop.table(profession.tab)*100, 2)

# Q2. For the other variables, provide in a concise manner: mean, median, standard deviation, minimum, maximum, and the number of available (non-missing) data points.
## We will use the function describe from the library prettyR 
library(prettyR) # calls prettyR library
### We create a vector which specifies the statistics we want to show
stat2show <- c("mean", "median", "sd", "min", "max", "valid.n")
### We create a vector which specifies the variable we want to show
var2show <- c("age", "amelioration.sante", "amelioration.moral", "recommander", "score.information", "score.relation")
describe(sh[,var2show], num.desc = stat2show)

# Q3. Create a histogram of the relationship score (score.relation).
### We use the function hist to create the histogram
### We pass on to it few other parameters to make is a bit more elegant
hist(sh$score.relation, col="red", prob=TRUE, las = 1, ylim=c(0,0.2), nclass=28, xlab = "Score Relation", main = "")
### plot the KDE along with it, lwd is for linewidth
lines(density(sh$score.relation, na.rm=TRUE, bw="SJ"), lwd=2)

# Q4. Using two box plots, represent side by side the distribution of relationship scores for men and women.
### We use the function boxplot
boxplot(sh$score.relation~sh$sexe.cat, ylab = "Score Relation", xlab = "Sexe")
