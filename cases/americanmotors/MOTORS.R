library(car)
motors.df <- read.csv("MOTORS.csv",header = TRUE)
View(motors.df)
motors.df <- na.omit(motors.df) # Omitting NA rows

boxplot(motors.df$MILPGAL~motors.df$COMPANY) # Boxplot of MILPGAL
 
# 1. Multi Collinearity :: Condition VIF < 4

model <- lm(motors.df$MILPGAL ~ motors.df$CYLINDER + motors.df$DISPLACE + motors.df$HORSPWR + motors.df$ACCEL + motors.df$WEIGHT + motors.df$YEAR, data=motors.df)
car::vif(model)

#Removing Displace to build the model again 
model <- lm(motors.df$MILPGAL ~ motors.df$CYLINDER + motors.df$HORSPWR + motors.df$ACCEL + motors.df$WEIGHT + motors.df$YEAR)
car::vif(model)

#Removing horsepower to build the model again
model <- lm(motors.df$MILPGAL ~ motors.df$CYLINDER + motors.df$ACCEL + motors.df$WEIGHT +motors.df$YEAR)
car::vif(model)

# Influential observations :: Cook's distance <1 
plot(cooks.distance(model))

# Creating a new coloumn to track r-standard values
motors.df$rstd <- rstandard(model)
plot(rstandard(model))


row = nrow(motors.df)

up_lim <- max(motors.df$rstd) # Maximum of R-standard
dn_lim <- min(motors.df$rstd) # Minimum of R-standard

while((up_lim>2 | (dn_lim<(-2))) && (nrow(motors.df)/row) > 0.95){
      if(up_lim > abs(dn_lim)) {
          cat("Removing Observation ",motors.df[motors.df$rstd==up_lim,1]," with rstandard: ",up_lim,"\n")
          motors.df <- motors.df[motors.df$rstd<up_lim,]
      }
      else { 
        cat("Removing Observation ",motors.df[motors.df$rstd==dn_lim,1]," with rstandard: ",dn_lim,"\n")
        motors.df <- motors.df[motors.df$rstd>dn_lim,]  
      }
  # Model again and re-calculate r-standard
  model <- lm(motors.df$MILPGAL ~ motors.df$CYLINDER + motors.df$ACCEL + motors.df$WEIGHT +motors.df$YEAR)
  motors.df$rstd <- rstandard(model)
  up_lim <- max(motors.df$rstd)
  dn_lim <- min(motors.df$rstd)
}

plot(rstandard(model))
plot(cooks.distance(model))
plot(model)
summary(model)
