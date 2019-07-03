library(readr)
library(MASS)
OLSON <- read_csv("OLSON.csv")

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
  if ((trainint[0] >= valInt[0]) & (trainint[1]<=valint[1])) {
    return("PASS")
  } else {
    return("FAIL")
  }
}

  
# End Utility functions
# ========================================================

# ========================================================
# TRAIN, TEST, VALIDATION split with 0.7,0.1,0.2 proportion

spec = c(train = .7, test = .1, validate = .2)

g = sample(cut(
  seq(nrow(OLSON)), 
  nrow(OLSON)*cumsum(c(0,spec)),
  labels = names(spec)
))


res = split(OLSON, g)

OlSON.TRAIN <- res$train
OLSON.VALIDATION <- res$validate
OLSON.TEST <- res$test
# END OF TRAIN, TEST, VALIDATION split
# ========================================================


model <- lm(OlSON.TRAIN$CHARGES ~ OlSON.TRAIN$DAYS)
#plot(model)
bc <- boxcox(model)
best.lam <- bc$x[bc$y==max(bc$y)]

OLSON.TRAIN <- transform(OlSON.TRAIN,TRANSFORMED=(CHARGES)^best.lam)

newModel <- lm(OLSON.TRAIN$TRANSFORMED ~ OLSON.TRAIN$CHARGES)
plot(newModel)
summary(newModel)
