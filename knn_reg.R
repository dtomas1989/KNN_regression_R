#rmse
setwd("~/Escritorio/files/udemy_work/r-course")
data <- read.csv('data/tema4/rmse.csv')

rmse <- function(actual, predicho){
  value = sqrt(mean((actual-predicho)^2))
  return(value)
}

rmse(data$price, data$pred)


library(dummies)
library(FNN)
library(scales)
library(caret)

data <- read.csv('data/tema4/education.csv')

#dummies
dms <- dummy(data$region, sep = '_')
data <- cbind(data,dms)
data$region <- NULL


#scale
data$urban.s <- rescale(data$urban)
data$income.s <- rescale(data$income)
data$under18.s <- rescale(data$under18)

data$urban <- NULL
data$income <- NULL
data$under18 <- NULL

#test, train and val
id <- createDataPartition(data$expense, p = 0.6, list = F)

train <- data[id,]
temp <- data[-id,]

nid <- createDataPartition(temp$expense, p = 0.5, list = F)

val <- temp[nid,]
test <- temp[-nid,]


###regression
reg <- knn.reg(train[,c(3:9)], val[,c(3:9)], train$expense, k =1, algorithm = 'brute')

rmse(val$expense, reg$pred)
