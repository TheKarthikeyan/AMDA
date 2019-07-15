library(readr)
library(MASS)
library(forecast)
library(ggplot2)
library(car)
OLSON <- read_csv("OLSON.csv")
# ========================================================
# 2.2 Frequency distribution and histograms for number of days for bill preparation and for the dollar value of client's bill
# 2.3.a Percentage of time company reach its goal of preparing bill in 45 days
# 2.3.b distribution of number of days for bill preparation
# 2.3.c distribution of size of client bills
# ========================================================

# 2.2
d <- ggplot(OLSON,aes(OLSON$DAYS)) 
c <- ggplot(OLSON,aes(OLSON$CHARGES))

d + geom_histogram()
c + geom_histogram()

# To be Completed
# ========================================================
set.seed(7)

# Training 70% Validation 20% Test 10% 
train.rows <- sample(rownames(OLSON),dim(OLSON)[1]*0.7)
valid.rows <- sample(setdiff(rownames(OLSON),train.rows),dim(OLSON)[1]*0.2)
test.rows <- sample(setdiff(rownames(OLSON),union(train.rows,valid.rows)))

train.data <- OLSON[train.rows,]
valid.data <- OLSON[valid.rows,]
test.data <- OLSON[test.rows,]

# ========================================================
# Begin Utility functions

getStdErr <- function(model) {
  err <- summary(model)$coefficients[, 2][2]
  return(err)
}

getInterval <- function(coeff, se) {
  int <- list(coeff-3*se,coeff+3*se)
  return(int)
}

validationCheck <- function(trainint, valint) {
  if ((trainint[0] >= valint[0]) & (trainint[1]<=valint[1])) {
    return("PASS")
  } else {
    return("FAIL")
  }
}

# ========================================================
# End Utility functions
# ========================================================

# ========================================================
# Being Box Cox transformation
# ========================================================
model <- lm(train.data$DAYS ~ train.data$CHARGES)
plot(model)

# ========================================================
# Box Cox Plot:
# Y^lambda = (Y^lambda -1)/lambda if lambda != 0
# Y^lambda = log y if lambda = 0
# ========================================================
bc <- boxcox(model)
best.lam <- bc$x[bc$y==max(bc$y)]

train.data <- transform(train.data,TRANSFORMED_DAYS=(DAYS)^best.lam)
trainModel <- lm(train.data$TRANSFORMED_DAYS ~ train.data$CHARGES)
#plot(trainModel)
summary(trainModel)

# ========================================================
# Linear regression diagnostics
# Cook's Distance < 1
plot(cooks.distance(trainModel))
train.data <- train.data[cooks.distance(trainModel)<1,]
train.data$rstd <- rstandard(trainModel)
# R-standard < 2 
#plot(train.data$rstd)

row = nrow(train.data)

up_lim <- max(train.data$rstd) # Maximum of R-standard
dn_lim <- min(train.data$rstd) # Minimum of R-standard

while((up_lim>2 | (dn_lim<(-2))) && (nrow(train.data)/row) > 0.95){
  if(up_lim > abs(dn_lim)) {
    cat("Removing Observation ",train.data[train.data$rstd==up_lim,1]," with rstandard: ",up_lim,"\n")
    train.data <- train.data[train.data$rstd<up_lim,]
  }
  else { 
    cat("Removing Observation ",train.data[train.data$rstd==dn_lim,1]," with rstandard: ",dn_lim,"\n")
    train.data <- train.data[train.data$rstd>dn_lim,]  
  }
  # Model again and re-calculate r-standard
  trainModel <- lm(train.data$TRANSFORMED_DAYS ~ train.data$CHARGES)
  bc <- boxcox(trainModel)
  best.lam <- bc$x[bc$y==max(bc$y)]
  train.data <- transform(train.data,TRANSFORMED_DAYS=(DAYS)^best.lam)
  trainModel <- lm(train.data$TRANSFORMED_DAYS ~ train.data$CHARGES)
  train.data$rstd <- rstandard(trainModel)
  up_lim <- max(train.data$rstd)
  dn_lim <- min(train.data$rstd)
}

sum(train.data$rstd>2 | train.data$rstd < (-2))
summary(trainModel)
# ========================================================

traininterval <- getInterval(trainModel$coefficients[2],getStdErr(trainModel))

valid.data <- transform(valid.data,TRANSFORMED_DAYS=(DAYS)^best.lam)
validModel <- lm(valid.data$TRANSFORMED_DAYS ~ valid.data$CHARGES)
plot(validModel)
summary(validModel)
validInterval <- getInterval(validModel$coefficients[2],getStdErr(validModel))

# validationCheck(traininterval,validInterval) # PASS

test.data <- transform(test.data,TEST_DAYS_FITTED=trainModel$coefficients[1]+(trainModel$coefficients[2]*test.data$CHARGES))
test.data <- transform(test.data,TEST_TRANSFORMED_DAYS=((DAYS)^best.lam-1)/best.lam)
accuracy(test.data$TEST_TRANSFORMED_DAYS,test.data$TEST_DAYS_FITTED)

# ========================================================
# Box Cox was not useful
# ========================================================

# ========================================================
# Box Tidwell Beginning
# ========================================================

boxTidwell(train.data$DAYS~train.data$CHARGES)
best.bt.lambda <- 0.33144

train.data <- transform(train.data,TRANSFORMED_CHARGES=(CHARGES)^best.bt.lambda)
trainModel <- lm(train.data$DAYS ~ train.data$TRANSFORMED_CHARGES)
summary(trainModel)
plot(trainModel)
# ========================================================
# Linear regression diagnostics
# Cook's Distance < 1
train.data <- train.data[cooks.distance(trainModel)<1,]
train.data$rstd <- rstandard(trainModel)
# R-standard < 2 
#plot(train.data$rstd)

row = nrow(train.data)

up_lim <- max(train.data$rstd) # Maximum of R-standard
dn_lim <- min(train.data$rstd) # Minimum of R-standard

while((up_lim>2 | (dn_lim<(-2))) && (nrow(train.data)/row) > 0.95){
  if(up_lim > abs(dn_lim)) {
    cat("Removing Observation ",train.data[train.data$rstd==up_lim,1]," with rstandard: ",up_lim,"\n")
    train.data <- train.data[train.data$rstd<up_lim,]
  }
  else { 
    cat("Removing Observation ",train.data[train.data$rstd==dn_lim,1]," with rstandard: ",dn_lim,"\n")
    train.data <- train.data[train.data$rstd>dn_lim,]  
  }
  # Model again and re-calculate r-standard
  trainModel <- lm(train.data$DAYS ~ train.data$TRANSFORMED_CHARGES)
  train.data$rstd <- rstandard(trainModel)
  up_lim <- max(train.data$rstd)
  dn_lim <- min(train.data$rstd)
}

sum(train.data$rstd>2 | train.data$rstd < (-2))
summary(trainModel)

traininterval <- getInterval(trainModel$coefficients[2],getStdErr(trainModel))
valid.data <- transform(valid.data,TRANSFORMED_CHARGES=(CHARGES)^best.bt.lambda)
validModel <- lm(valid.data$DAYS ~ valid.data$TRANSFORMED_CHARGES)
plot(validModel)
summary(validModel)
validInterval <- getInterval(validModel$coefficients[2],getStdErr(validModel))

# validationCheck(traininterval,validInterval) # PASS

test.data <- transform(test.data,TEST_DAYS_FITTED=trainModel$coefficients[1]+(trainModel$coefficients[2]*(test.data$CHARGES)^best.bt.lambda))
accuracy(test.data$DAYS,test.data$TEST_DAYS_FITTED)

# ========================================================

