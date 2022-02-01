#########principal component analysis
#########################################################Goal1: dimension reduction
###################################################################################
#The principal components method of extraction begins by finding a linear combination of variables 
#(a component) that accounts for as much variation in the original variables as possible. 
#It then finds another component that accounts for as much of the remaining variation as possible and is 
#uncorrelated with the previous component, continuing in this way until there are as many components as original variables. 
#Usually, a few components will account for most of the variation, and these components can be used to replace the original variables.
#This method is most often used to reduce the number of variables in the data file.

#############################################################Goal2: structure detection
#Other Factor Analysis extraction methods go one step further by adding the assumption that some of the 
#variability in the data cannot be explained by the components (usually called factors in other extraction methods). 
#As a result, the total variance explained by the solution is smaller; 
#however, the addition of this structure to the factor model makes these methods ideal for examining relationships 
#between the variables. 

#########################example1 = goal 1
library(rio)
mydat<- import("wdbc.csv")
head(mydat)

features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
names(mydat) <- c("id", "diagnosis", paste0(features,"_mean"), paste0(features,"_se"), paste0(features,"_worst"))

###pca
wdbc.pr <- prcomp(mydat[c(3:32)], center = TRUE, scale = TRUE)
summary(wdbc.pr)

##findings 1
#Standard deviation: This is simply the eigenvalues in our case since the data has been centered and scaled (standardized)
#Proportion of Variance: This is the amount of variance the component accounts for in the data, ie. PC1 accounts for >44% of total variance in the data alone!
#Cumulative Proportion: This is simply the accumulated amount of explained variance, ie. if we used the first 10 components we would be able to account for >95% of total variance in the data.

library(rio)
library(tidyverse)
library(pls)

screeplot(wdbc.pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"), col=c("red"), lty=5, cex=0.6)
##6 components explain 88.8% of the variances, so the 30 variables have been reduced to 6

cumpro <- cumsum(wdbc.pr$sdev^2 / sum(wdbc.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

#########################example2 = goal 2
library("factoextra")

fviz_pca_ind(wdbc.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = mydat$diagnosis, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))
##################################################################################################more examples
#############data reduction: car sales
library(foreign)
car<- import("car_sales.sav")
#car1 <- read.spss("car_sales.sav",to.data.frame=TRUE,use.value.labels = FALSE)

#An industry analyst would like to predict automobile sales from a set of predictors. 
#However, many of the predictors are correlated, and the analyst fears that this might adversely affect her results.

#find missing values
apply(is.na(car),2,which)

car1 <- car %>% select(type:mpg) %>% drop_na()
apply(is.na(car1),2,which)

car.pr <- prcomp(car1, center = TRUE, scale = TRUE)
summary(car.pr)

screeplot(car.pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
#only 3 PC are above 1, and they account for 87.71% of the variance

##########################structure detection
car2 <- car %>% select(manufact,type:mpg) %>% drop_na()

fviz_pca_ind(car.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 3, 
             fill.ind = car2$manufact, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))

#######Kaiser-Meyer-Olkin (KMO) measure of sampling adequacy and Bartlett’s test of sphericity
if(!require(pacman)){install.packages("pacman");
  library(pacman)}
p_load(ltm,lattice,psych,car,pastecs,scales,ggplot2,arules,Rmisc,GPArotation,gdata,MASS,qpcR,gtools,Hmisc,rio,tidyverse,pls,factoextra)

# Bartlett's= can we do PCA with this data? p-value should be less than 0.05
cortest.bartlett(car1)

#We reject the null hypothesis at the .1% level(p-value < 0.001)= so we can do the PCA

# KMO (Kaiser-Meyer-Olkin)= if 0.6+ = good
KMO(car1)
#since KMO is 0.83, PCA results are good.
##############################################################################################################
############ 1 dimension reduction
mydat<- import("telco.sav") %>% select(longmon,tollmon,equipmon,cardmon,wiremon,multline,voice,ebill,pager,internet,callid,callwait,forward,confer) %>% drop_na()

wdbc.pr <- prcomp(mydat, center = TRUE, scale = TRUE)
summary(wdbc.pr)
##3 components explain 65% of the variances, so the 14 variables have been reduced to 3

screeplot(wdbc.pr, type = "l", npcs = 14, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"), col=c("red"), lty=5, cex=0.6)

#######Kaiser-Meyer-Olkin (KMO) measure of sampling adequacy and Bartlett’s test of sphericity
# Bartlett's= can we do PCA with this data? p-value should be less than 0.05
cortest.bartlett(mydat)

#We reject the null hypothesis at the .1% level(p-value < 0.001)= so we can do the PCA

# KMO (Kaiser-Meyer-Olkin)= if 0.6+ = good
KMO(mydat)
#since KMO is 0.89, PCA results are good.
#####################################################################################################################
############ 2 structure detection: telcos
mydat<- import("telco.sav") %>% select(longmon,tollmon,equipmon,cardmon,wiremon,multline,voice,ebill,pager,internet,callid,callwait,forward,confer) %>% drop_na()
mydat1<- import("telco.sav") %>% select(longmon,tollmon,equipmon,cardmon,wiremon,multline,voice,ebill,pager,internet,callid,callwait,forward,confer,custcat) %>% drop_na()

mydat1 <- mydat1 %>%
  mutate(cust =case_when(
    custcat == 1 ~ "basic",
    custcat == 2 ~ "electronic",
    custcat == 3 ~ "plus",
    custcat == 4 ~ "total"))

wdbc.pr <- prcomp(mydat, center = TRUE, scale = TRUE)

fviz_pca_ind(wdbc.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 3, 
             fill.ind = mydat1$cust, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))

##############################Finding the best model with PCR or PLS
######### principal component analysis (PCA), regression (PCR)
set.seed(123)
cv_model_pcr <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 100
)

cv_model_pcr$bestTune #lowest rmse

cv_model_pcr$results %>%
  dplyr::filter(ncomp == pull(cv_model_pcr$bestTune))

ggplot(cv_model_pcr)
###using PCR, the cross-validated RMSE reduced from about $37,000 to nearly $30,000.

##partial least squares (PLS)
set.seed(123)
cv_model_pls <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 30
)

cv_model_pls$results %>%
  dplyr::filter(ncomp == pull(cv_model_pls$bestTune))
ggplot(cv_model_pls)
#$30,000 to $27,000 

