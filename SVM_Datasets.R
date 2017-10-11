#importing data set into R
library(readxl)
DataSetIDM <- read_excel("~/Downloads/DataSetIDM.xlsx")
View(DataSetIDM)

#checking the data
str(DataSetIDM)
dataSetIDM <- as.data.frame(DataSetIDM[,c(3,4,5,6)])
str(dataSetIDM)

#which continent an entity is based on the life expectancy value
table(dataSetIDM$Continent)
head(dataSetIDM)

#use set seed to produce same results since we are using random number generator
set.seed(9850)

#sample set 1
sampleset1 <- sample(2, nrow(dataSetIDM), replace = TRUE, prob = c(0.8, 0.2))
trainingdata1 <- dataSetIDM[sampleset1==1,]
testdata1 <- dataSetIDM[sampleset1==2,]

#sample set 2 
sampleset2 <- sample(2, nrow(dataSetIDM), replace = TRUE, prob = c(0.8, 0.2))
trainingdata2 <- dataSetIDM[sampleset2==1,]
testdata2 <- dataSetIDM[sampleset2==2,]

#sample set 3
sampleset3 <- sample(2, nrow(dataSetIDM), replace = TRUE, prob = c(0.8, 0.2))
trainingdata3 <- dataSetIDM[sampleset3==1,]
testdata3 <- dataSetIDM[sampleset3==2,]

#sample set 4
sampleset4 <- sample(2, nrow(dataSetIDM), replace = TRUE, prob = c(0.8, 0.2))
trainingdata4 <- dataSetIDM[sampleset4==1,]
testdata4 <- dataSetIDM[sampleset4==2,]

#sample set 5
sampleset5 <- sample(2, nrow(dataSetIDM), replace = TRUE, prob = c(0.8, 0.2))
trainingdata5 <- dataSetIDM[sampleset5==1,]
testdata5 <- dataSetIDM[sampleset5==2,]

#support vector machine for each of the five sample sets
#install.packages("e1071")
library(e1071)
x1 <- subset(trainingdata1, select=-Continent)
x2 <- subset(trainingdata2, select=-Continent)
x3 <- subset(trainingdata3, select=-Continent)
x4 <- subset(trainingdata4, select=-Continent)
x5 <- subset(trainingdata5, select=-Continent)

y1 <- trainingdata1$Continent
y2 <- trainingdata2$Continent
y3 <- trainingdata3$Continent
y4 <- trainingdata4$Continent
y5 <- trainingdata5$Continent

#model generation for each of the 5 sample sets
modelsvm1 <- svm(x1, y1, type = "C-classification")
modelsvm2 <- svm(x2, y2, type = "C-classification")
modelsvm3 <- svm(x3, y3, type = "C-classification")
modelsvm4 <- svm(x4, y4, type = "C-classification")
modelsvm5 <- svm(x5, y5, type = "C-classification")

#summary of each model
summary(modelsvm1)
summary(modelsvm2)
summary(modelsvm3)
summary(modelsvm4)
summary(modelsvm5)

#prediction to predict and later create the confusion matrix
pred1 <- predict(modelsvm1, x1)
pred2 <- predict(modelsvm2, x2)
pred3 <- predict(modelsvm3, x3)
pred4 <- predict(modelsvm4, x4)
pred5 <- predict(modelsvm5, x5)

#confusion matrix for each of the sample set
table(pred1, y1)
table(pred2, y2)
table(pred3, y3)
table(pred4, y4)
table(pred5, y5)

#accuracy percentage based on mean
mean(pred1 == y1)
mean(pred2 == y2)
mean(pred3 == y3)
mean(pred4 == y4)
mean(pred5 == y5)