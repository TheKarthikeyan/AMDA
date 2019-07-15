#Delete all loaded data and values

rm(list = ls())

#Set working directory
setwd("~/Documents/AMDA/cases/bankruptcy")
getwd()
library(generalhoslem)
library(readxl)
library(car)
library(ROCR)
library(Metrics)
library(InformationValue)
library(caret)

bankruptcy <- read_excel("Bankruptcy.xls", sheet = "data")
#View(bankruptcy)
bankruptcy[["NO"]] <- NULL

# some static definition
cook_distance_threshold <- 1
vif_threshold <- 4
p_val_threshold <- 0.1
# define training control
train_control <- trainControl(method = "cv", number = 10)
#========================================================================#
# Data separation - Begin                                                #
#========================================================================#
# Training = 70% Validation = 20% Test = 10%

set.seed(7)

train.rows <- sample(rownames(bankruptcy),dim(bankruptcy)[1]*0.7)
valid.rows <- sample(setdiff(rownames(bankruptcy),train.rows),dim(bankruptcy)[1]*0.2)
test.rows <- sample(setdiff(rownames(bankruptcy),union(train.rows,valid.rows)))

train.data <- bankruptcy[train.rows,]
valid.data <- bankruptcy[valid.rows,]
test.data <- bankruptcy[test.rows,]

#========================================================================#
# Data separation - End                                                  #
#========================================================================#

#========================================================================#
# Multi-collinearity - Begin                                             #
#========================================================================#

# del_v_coloumns contains coloumns to be removed

del_v_coloumns <- vector('character')

# Looping to find del_v_coloumns by iteratively calculating VIF and subsequently removing them in the tmodel


while(TRUE) {
  vfit <- vif(train(D ~ .,
                    data = train.data[setdiff(names(train.data),del_v_coloumns)],
                    trControl = train_control,
                    method = "glm",
                    family=binomial()))
  if(max(vfit)>vif_threshold) {
    del_v_coloumns <- append(del_v_coloumns,names(which(vfit == max(vfit)))) 
    cat("Removing coloumn ",tail(del_v_coloumns,1)," with VIF:",max(vfit),"\n")
  } else {
    break;
  }
}

# Removing multi-collinear rows
train.data <- subset(train.data,select = names(train.data)[!names(train.data) %in% del_v_coloumns])

#========================================================================#
# Multi-collinearity - End                                               #
#========================================================================#


#========================================================================#
# Training tmodel fit - Begin                                             #
#========================================================================#

tmodel <- train(D ~ .,
                data = train.data,
                trControl = train_control,
                method = "glm",
                family=binomial())

del_p_coloumns <- vector('character')

while(TRUE) {
  li <- summary(tmodel)$coefficients[,4]
  if(max(li[2:length(li)])>p_val_threshold) {
    del_p_coloumns <- append(del_p_coloumns,names(which(li == max(li[2:length(li)])))) 
    cat("Removing coloumn ",names(which(li == max(li[2:length(li)])))," with P-value:",max(li[2:length(li)]),"\n")
    # Recalculating tmodel
    tmodel <- train(D ~ .,
                    data = train.data[setdiff(names(train.data),del_p_coloumns)],
                    trControl = train_control,
                    method = "glm",
                    family=binomial())
  } else {
    break;
  }
}
# Removing insignificant coloumns
train.data <- subset(train.data,select = names(train.data)[!names(train.data) %in% del_p_coloumns])

# All p-values should be greater than 0.1 Check
summary(tmodel)


# Cook's distance check
print(max(cooks.distance(tmodel)))
plot(cooks.distance(tmodel))
# Remove values above threshold (>1)
train.data <- train.data[cooks.distance(tmodel)<cook_distance_threshold,]

# Hosmer - Lemeshow test - P = 0.2645
h <- logitgof(exp = tmodel$fitted.values,obs = train.data$D)
print(h)

# ROC with plot
pred <- predict(tmodel, type='response')
ROCRpred <- prediction(pred, train.data$D)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

# AUC - 0.8652381
auc(actual = train.data$D,predicted = tmodel$fitted.values)

summary(tmodel)$coefficients

train_left <- vector('numeric')
train_right <- vector('numeric')

t_est <- summary(tmodel)$coefficients[,1]
t_se <- summary(tmodel)$coefficients[,2]
for(i in 2:length(summary(tmodel)$coefficients[,1])) {
  train_left <- append(train_left,t_est[i]-3*t_se[i])
  train_right <- append(train_right,t_est[i]+3*t_se[i])
}

#========================================================================#
# Training tmodel fit - End                                               #
#========================================================================#

#========================================================================#
# Validation tmodel fit - Begin                                           #
#========================================================================#

# Preparation of validation data - with same variables as training data

# Removing multi-collinear rows
valid.data <- subset(valid.data,select = names(valid.data)[!names(valid.data) %in% del_v_coloumns])
# Removing insignificant coloumns
valid.data <- subset(valid.data,select = names(valid.data)[!names(valid.data) %in% del_p_coloumns])

vmodel <- glm(D ~ .,data=valid.data,family = "binomial")

summary(vmodel)

valid_left <- vector('numeric')
valid_right <- vector('numeric')

v_est <- summary(vmodel)$coefficients[,1]
v_se <- summary(vmodel)$coefficients[,2]

for(i in 2:length(summary(vmodel)$coefficients[,1])) {
  valid_left <- append(valid_left,v_est[i]-3*v_se[i])
  valid_right <- append(valid_right,v_est[i]+3*v_se[i])
}

for(i in 1:length(train_left)) {
  if(train_left[i]<valid_left[i] | train_right[i]>valid_right[i]) {
    print("FAIL")
    break
  } else {
    cat("Pass for variable ",i,"\n")
  }
}

# ROC with plot
pred <- predict(vmodel, type='response')
optCutOff <- optimalCutoff(valid.data$D, pred)[1]
table(valid.data$D,pred>optCutOff)

summary(pred)

ROCRpred <- prediction(pred, valid.data$D)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

# AUC - 0.8787
auc(actual = valid.data$D,predicted = pred)

#========================================================================#
# Validation tmodel fit - End                                            #
#========================================================================#

#========================================================================#
# Test - begin                                                           #
#========================================================================#

pred <- predict(vmodel,type='response', newdata=test.data)
confMatrix <- table(test.data$D,pred>optCutOff)

print(confMatrix)

accuracy <- sum(diag(confMatrix))/sum(confMatrix) # 0.9285
sens <- sensitivity(test.data$D,pred,threshold = 0.4) # 0.8
spec <- specificity(test.data$D,pred,threshold = 0.4) # 0.1

cat("Accuracy: ",accuracy,"\nSensitivity: ",sens,"\nSpecificity: ",spec)

#========================================================================#
# Test - End                                                             #
#========================================================================#
