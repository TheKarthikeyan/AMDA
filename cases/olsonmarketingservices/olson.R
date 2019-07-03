library(readr)
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

bc <- box(lm(OlSON.TRAIN$CHARGES ~ OlSON.TRAIN$DAYS),lambda=seq(0,1,by=.1))
