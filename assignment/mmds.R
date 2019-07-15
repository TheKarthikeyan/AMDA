library(readr)
library(MASS)
library(forecast)
library(ggplot2)
library(car)
mmds <- read_csv("mmds.csv")
View(mmds)
summary(mmds)

# =====================================================
#     Converted Market size character variables
#       into integer variables
# =====================================================
mmds$MarketSize[mmds$MarketSize=="Small"]=1
mmds$MarketSize[mmds$MarketSize=="Medium"]=2
mmds$MarketSize[mmds$MarketSize=="Large"]=3
summary(mmds)
transform(mmds,MarketSize=as.numeric(MarketSize))

# =====================================================
#     Q1. Which promotion type yields highest sales
#         after accounting for Market Size and age
# =====================================================

#mmds.small <- mmds[mmds$MarketSize=1,]
#mmds.medium <- mmds[mmds$MarketSize==2,]
#mmds.large <- mmds[mmds$MarketSize==3,]

# =====================================================
#     Train, Valid, Test : 70%, 20%, 10% split
# =====================================================

train.rows <- sample(rownames(mmds),dim(mmds)[1]*0.7)
valid.rows <- sample(setdiff(rownames(mmds),train.rows),dim(mmds)[1]*0.2)
test.rows <- sample(setdiff(rownames(mmds),union(train.rows,valid.rows)))

train.data <- mmds[train.rows,]
valid.data <- mmds[valid.rows,]
test.data <- mmds[test.rows,]


# =====================================================
#   Let's try to fit simple multiple linear regression
# =====================================================

trainModel <- lm(SalesInThousands ~ MarketSize + LocationID + AgeOfStore + Promotion + week,data=train.data)
vif(mod)
plot(mod)
# ===================================================================
#   Multicollinearity, Linearity, Normality assumptions are satisfied
# ===================================================================

plot(cooks.distance(mod))

# ===================================================================
#   No Outliers
# ===================================================================

train.data$rstd <- rstandard(mod)
plot(train.data$rstd)

row = nrow(train.data)

up_lim <- max(train.data$rstd) # Maximum of R-standard
dn_lim <- min(train.data$rstd) # Minimum of R-standard

while((up_lim>2 | (dn_lim<(-2))) && (nrow(train.data)/row) > 0.95){
  if(up_lim > abs(dn_lim)) {
    #cat("Removing Observation ",train.data[train.data$rstd==up_lim,1]," with rstandard: ",up_lim,"\n")
    train.data <- train.data[train.data$rstd<up_lim,]
  }
  else { 
    #cat("Removing Observation ",train.data[train.data$rstd==dn_lim,1]," with rstandard: ",dn_lim,"\n")
    train.data <- train.data[train.data$rstd>dn_lim,]  
  }
  # Model again and re-calculate r-standard
  trainModel <- lm(SalesInThousands ~ MarketSize + LocationID + AgeOfStore + Promotion + week,data=train.data)
  train.data$rstd <- rstandard(trainModel)
  up_lim <- max(train.data$rstd)
  dn_lim <- min(train.data$rstd)
}

summary(trainModel)
pred <- predict(trainModel,test.data)

accuracy(pred,test.data$SalesInThousands)



