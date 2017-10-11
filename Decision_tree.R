#classification method ripper using caret's Jrip method
#install.packages("caret")
#install.packages("rJava")
#install.packages("RWeka")
library(caret)
library(RWeka)

#importing data set into R
library(readxl)
DataSet <- read_excel("~/Downloads/DataSetIDM.xlsx")
View(DataSet)

#checking the data
str(DataSet)
dataset <- as.data.frame(DataSet[,c(3,4,5,6)])
str(dataset)

#which continent an entity is based on the life expectancy value
table(dataset$Continent)
head(dataset)

#use set seed to produce same results since we are using random number generator
set.seed(9850)

#to select random numbers for all the rows
gp <- runif(nrow(dataset))
dataset <- dataset[order(gp),]
str(dataset)
head(dataset)

#sample set 1
sampleset1 <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.8, 0.2))
trainingdata1 <- dataset[sampleset1==1,c(2,3,4)]
trainclasses1 <- dataset[sampleset1==1,1]

#sample set 2
sampleset2 <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.8, 0.2))
trainingdata2 <- dataset[sampleset1==1,c(2,3,4)]
trainclasses2 <- dataset[sampleset1==1,1]

#sample set 3
sampleset3 <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.8, 0.2))
trainingdata3 <- dataset[sampleset1==1,c(2,3,4)]
trainclasses3 <- dataset[sampleset1==1,1]

#sample set 4
sampleset4 <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.8, 0.2))
trainingdata4 <- dataset[sampleset1==1,c(2,3,4)]
trainclasses4 <- dataset[sampleset1==1,1]

#sample set 5
sampleset5 <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.8, 0.2))
trainingdata5 <- dataset[sampleset1==1,c(2,3,4)]
trainclasses5 <- dataset[sampleset1==1,1]

#ripper algo
fit1 <- train(trainingdata1, trainclasses1, method="JRip")
summary(fit1)

fit2 <- train(trainingdata2, trainclasses2, method="JRip")
summary(fit2)

fit3 <- train(trainingdata3, trainclasses3, method="JRip")
summary(fit3)

fit4 <- train(trainingdata4, trainclasses4, method="JRip")
summary(fit4)

fit5 <- train(trainingdata5, trainclasses5, method="JRip")
summary(fit5)