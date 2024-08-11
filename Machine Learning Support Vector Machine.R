# set pseudorandom number generator
set.seed(10)
#ortp code
if(!require(pacman)){install.packages("pacman");
  library(pacman)}
p_load(tidyverse,kernlab,e1071,ISLR,RColorBrewer)

#####linear
# Construct sample data set - completely separated
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 3/2
dat <- data.frame(x=x, y=as.factor(y))

# Plot data
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data = dat, kernel = "linear", scale = FALSE)
# Plot Results
plot(svmfit, dat)

##“X” are the support vectors, or the points that directly affect the classification line. 

#####non-linear
# construct larger random data set
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] <- x[1:100,] + 2.5
x[101:150,] <- x[101:150,] - 2.5
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x,y=as.factor(y))
head(dat)
# Plot data
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

# set pseudorandom number generator
#set.seed(123)
# sample training data and fit model
train <- base::sample(200,100, replace = FALSE)
svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
# plot classifier
plot(svmfit, dat)

# tune model to find optimal cost, gamma values
tune.out <- tune(svm, y~., data = dat[train,], kernel = "radial",
                 ranges = list(cost = c(0.1,1,10,100,1000),
                               gamma = c(0.5,1,2,3,4)))
# show best model
tune.out$best.model

# validate model performance
(valid <- table(true = dat[-train,"y"], pred = predict(tune.out$best.model,newx = dat[-train,])))



