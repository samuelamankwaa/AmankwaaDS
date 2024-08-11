######## binomial / binary Logistic regression= response is 0/1, yes/no
library(dplyr)     # for data wrangling
library(ggplot2)   # for awesome plotting
library(rsample)   # for data splitting
library(caret)     # for logistic regression modeling
library(vip)       # variable importance
library(aod)

##example- not machine learning
mydata <- read.csv("binary.csv")
head(mydata)
summary(mydata)

str(mydata)

mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)

####interpretation
##For every one unit change in gre, the log odds of admission (versus non-admission) increases by 0.002.
##For a one unit increase in gpa, the log odds of being admitted to graduate school increases by 0.804.
##having attended an undergraduate institution with rank of 2, versus an institution with a rank of 1, changes the log odds of admission by -0.675
##having attended an undergraduate institution with rank of 3, versus an institution with a rank of 1, changes the log odds of admission by -0.34
##having attended an undergraduate institution with rank of 4, versus an institution with a rank of 1, changes the log odds of admission by -1.55

########multinomial logistic regression: response is more than 2
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(stargazer)

ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
summary(ml)

#reference category is general

###change reference category
ml$prog2 <- relevel(ml$prog, ref="academic")
test <- multinom(prog2 ~ ses + write, data = ml)
summary(test)

stargazer(test, type="html", out="test.htm")

##interpretation of significant variables: p-value <=0.05
#A one-unit increase in the variable write is associated with the decrease in the log odds of being in general program vs. academic program in the amount of 0.058 .
#A one-unit increase in the variable write is associated with the decrease in the log odds of being in vocation program vs. academic program. in the amount of .114 .
#The log odds of being in general program vs. in academic program will decrease by 1.163 if moving from ses="low" to ses="high".

#########more examples
library(rio)
mydata= import("Academic Programes.csv")
head(mydata)

# Checking the output (dependent) variable
table(mydata$ses)

str(mydata$ses) ## ses is integer, but has to be a factor

mydata$ses <- as.factor(mydata$ses) #change from integer to factor

#change reference level
mydata$ses2 = relevel(mydata$ses, ref = "2")

multi1 = multinom(ses2 ~ science + socst+  female, data=mydata)
summary(multi1)
library(stargazer)

stargazer(multi1, type="html", out="multi1.htm")

###interpretation
####1 under ‘science’, the -0.02 suggests that for one unit increase in ‘science’ score, 
#the logit coefficient for ‘low’ relative to ‘middle’ will go down by that amount, -0.02. 

### Home Work

####socst
##### The results suggest that for every one unit increase in social studies score, there is a 0.039 decrease in the ses., 
##### and this is significant (Coefficient).
####female
#### The results suggest that for every one unit increase in female, there is an increase in the ses. by 0.82.,
####  and this is significant (Coefficient).
##########################################################################relative risk ratios (RRR)- easier
multi1.rrr = exp(coef(multi1))
stargazer(multi1, type="html", coef=list(multi1.rrr),  p.auto=FALSE, out="multi1rrr.htm")

##interpretation 1
#Keeping all other variables constant, if your science score increases one unit, 
#you are 0.97 times more likely to stay in the low ses category as compared to the middle ses category (the risk or odds is 3% (100% - 97.7%) lower). 
#The coefficient, however, is not significant.= because there is no star

#interpretation 2
#Keeping all other variables constant, if your science score increases one unit, 
#you are 1.02 times more likely to stay in the high ses category as compared to the middle ses category (the risk or odds is 2% (102% - 100%) higher). 
#The coefficient, however, is not significant.

#### Home Work

##socst
#Keeping all other variables constant, if your social studies score increases one unit, 
#you are 0.96 times more likely to stay in the low ses category as compared to the middle ses category (the risk or odds is 3.8% (100% - 96.2%) lower). 
#The coefficient is significant.= because there are 2 stars.

#interpretation 2
#Keeping all other variables constant, if your social studies score increases one unit, 
#you are 1.04 times more likely to stay in the high ses category as compared to the middle ses category (the risk or odds is 4% (104% - 100%) higher). 
#The coefficient is significant.= because there are 2 stars.

##female
#Keeping all other variables constant, if the number of females increases by one unit, 
#you are 2.26 times more likely to stay in the low ses category as compared to the middle ses category (the risk or odds is 126.3% (226.3% - 100%) lower). 
#The coefficient is significant.= because there are 2 stars.

#interpretation 2
#Keeping all other variables constant, if the number of females increases by one unit, 
#you are 0.97 times more likely to stay in the high ses category as compared to the middle ses category (the risk or odds is 3.2% (100% - 96.8%) higher). 
#The coefficient, however, is not significant.









































