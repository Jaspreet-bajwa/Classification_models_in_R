#decision tree C4.5

# load the package
library(RWeka)
library(C50)

# load data
#importing data set into R
library(readxl)
DataSet <- read_excel("~/Downloads/DataSetIDM.xlsx")
View(DataSet)

#checking the data
str(DataSet)
Dataset1 <- as.data.frame(DataSet[,c(3,4,5,6)])
str(Dataset1)

#which continent an entity is based on the life expectancy value
table(Dataset1$Continent)
head(Dataset1)

#mix up the rows to get the best possible results

#use set seed to produce same results since we are using random number generator
set.seed(9850)

#to select random numbers for all the rows
gp <- runif(nrow(Dataset1))
Dataset1 <- Dataset1[order(gp),]
str(Dataset1)
head(Dataset1)

#create training and test data set to apply to KNN
#sample set 1
sampleset1 <- sample(2, nrow(Dataset1), replace = TRUE, prob = c(0.8, 0.2))
trainx1 <- Dataset1[sampleset1==1,]
testx1 <- Dataset1[sampleset1==2, c(2,3,4)]
testy1 <- Dataset1[sampleset1==2,1]

#sample set 2
trainx2 <- Dataset1[41:223,]
testx2 <- Dataset1[1:40,c(2,3,4)]
testy2 <- Dataset1[1:40,1]

#sample set 3
trainx3 <- Dataset1[1:185,]
testx3 <- Dataset1[186:223,c(2,3,4)]
testy3 <- Dataset1[186:223,1]

#sample set 4
trainx4 <- Dataset1[61:223,]
testx4 <- Dataset1[1:60,c(2,3,4)]
testy4 <- Dataset1[1:60,1]


#sample set 5
trainx5 <- Dataset1[101:223,]
testx5 <- Dataset1[1:100,c(2,3,4)]
testy5 <- Dataset1[1:100,1]


# fit model
mod1 <- train(Continent ~ ., data = trainx1, method = "C5.0")
mod2 <- train(Continent ~ ., data = trainx2, method = "C5.0")
mod3 <- train(Continent ~ ., data = trainx3, method = "C5.0")
mod4 <- train(Continent ~ ., data = trainx4, method = "C5.0")
mod5 <- train(Continent ~ ., data = trainx5, method = "C5.0")

# summarize the fit
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
# make predictions, 
predictions1 <- predict(mod1, testx1, type="raw")
predictions2 <- predict(mod2, testx2, type="raw")
predictions3 <- predict(mod3, testx3, type="raw")
predictions4 <- predict(mod4, testx4, type="raw")
predictions5 <- predict(mod5, testx5, type="raw")

# summarize accuracy
mean(predictions1 == testy1)
mean(predictions2 == testy2)
mean(predictions3 == testy3)
mean(predictions4 == testy4)
mean(predictions5 == testy5)