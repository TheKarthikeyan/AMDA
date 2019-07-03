housing.df <- read.csv("CSV/West Roxbury.csv",header=TRUE) # Load data
dim(housing.df)
head(housing.df)
View(housing.df)

# Practice to find subsets of data
housing.df[1:10,1]
housing.df[1:10,]
housing.df[5,1:10]
housing.df[,1]
housing.df[5,c(1:2,4,8:10)]
housing.df$TOTAL.VALUE
housing.df$TOTAL.VALUE[1:10]
length(housing.df$TOTAL.VALUE)
mean(housing.df$TOTAL.VALUE)
summary(housing.df)

s <- sample(row.names(housing.df),5)
housing.df[s,]
# Oversample house with over 10 rooms
s <- sample(row.names(housing.df),5,prob = ifelse(housing.df$ROOMS>10,0.9,0.01))
housing.df[s,]

names(housing.df)
t(t(names(housing.df)))
colnames(housing.df)[1] <- c("TOTAL_VALUE") # Change first coloumn name
class(housing.df$REMODEL)
class(housing.df[,14])
levels(housing.df[,14]) # None, Old, Recent
class(housing.df$BEDROOMS) # integer
class(housing.df[,1]) # numeric

# Convert all categorical variables in the data frame into a set of dummy variables

xtotal <- model.matrix(~0 + BEDROOMS + REMODEL, data=housing.df)
xtotal <- as.data.frame(xtotal) # converting matrix back into data frame
t(t(names(xtotal)))
xtotal <- xtotal[,-4] # Dropping fourth variable

# Imputting missing data with median

# Convert few entires of bedrooms to NA's then we impute these missing values using the median of the remaining values
rows.to.missing <- sample(row.names(housing.df),10)
housing.df[rows.to.missing,]$BEDROOMS <- NA
summary(housing.df$BEDROOMS)

# replace missing value with median
housing.df[rows.to.missing,]$BEDROOMS <- median(housing.df$BEDROOMS,na.rm = TRUE)
summary(housing.df$BEDROOMS)

# TO get same partition while re-running R code
set.seed(1)

# Training 60%, Validation - 40%
train.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.6)
train.data <- housing.df[train.rows,]

valid.rows <- setdiff(rownames(housing.df),train.rows)
valid.data <- housing.df[valid.rows,]

# Training 70% Validation 20% Test 10% - Another method
train.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.7)
valid.rows <- sample(setdiff(rownames(housing.df),train.rows),dim(housing.df)[1]*0.2)
test.rows <- setdiff(rownames(housing.df),union(train.rows,valid.rows))

train.data <- housing.df[train.rows,]
valid.data <- housing.df[valid.rows,]
test.data <- housing.df[test.rows,]


