library(forecast)

housing.df <- read.csv("CSV/West Roxbury.csv",header=TRUE) # Load data

# Training 70% Validation 20% Test 10% - Another method
train.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.7)
valid.rows <- sample(setdiff(rownames(housing.df),train.rows),dim(housing.df)[1]*0.3)

train.data <- housing.df[train.rows,]
valid.data <- housing.df[valid.rows,]

model <- lm(TOTAL.VALUE ~ .-TAX,data=housing.df,subset = train.rows) #Removes TAX
tr.res <- data.frame(train.data$TOTAL.VALUE,model$fitted.values,model$residuals)

# Prediction
pred <- predict(model,newdata = valid.data)
val.res <- data.frame(valid.data$TOTAL.VALUE,pred,residuals=valid.data$TOTAL.VALUE-pred)
head(val.res)

#Accuracy on training set
accuracy(model$fitted.values,train.data$TOTAL.VALUE)

# Accuracy on validation set
pred <- predict(model,newdata = valid.data)
accuracy(pred,valid.data$TOTAL.VALUE)