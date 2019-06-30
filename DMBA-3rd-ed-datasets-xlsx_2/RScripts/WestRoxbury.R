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

