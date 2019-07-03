library(readr)
library(MASS)
OLSON <- read_csv("OLSON.csv")

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

model <- lm(OlSON.TRAIN$CHARGES ~ OlSON.TRAIN$DAYS)
plot(model)

bc <- boxcox(model,lambda=seq(-3,3))

best.lam <- bc$x[which(bc$y==max(bc$y))]

newModel <- lm((OlSON.TRAIN$CHARGES)^best.lam~OlSON.TRAIN$DAYS)
plot(newModel)

summary(newModel)
