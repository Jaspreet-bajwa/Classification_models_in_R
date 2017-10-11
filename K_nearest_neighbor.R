#importing data set into R
library(readxl)
DataSet <- read_excel("~/Downloads/DataSetIDM.xlsx")
View(DataSet)

#checking the data and selecting only the needed columns
head(DataSet)
Dataset1 <- as.data.frame(DataSet[,c(3,4,5,6)])
str(Dataset1)

#which continent an entity is based on the life expectancy value - classification rule
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

#normalize the data so that features with higher values should not undue influence the results
summary(Dataset1[, c(2,3,4)])
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}
dataset <- as.data.frame(lapply(Dataset1[,c(2,3,4)], normalize))
str(dataset)
summary(dataset)


#create training and test data set to apply to KNN
#sample set 1
trainingdata1 <- dataset[1:178,]
testdata1 <- dataset[179:223,]
datasettd1 <- Dataset1[1:178,1]
datasettest1 <- Dataset1[179:223,1]

#sample set 2
trainingdata2 <- dataset[41:223,]
testdata2 <- dataset[1:40,]
datasettd2 <- Dataset1[41:223,1]
datasettest2 <- Dataset1[1:40,1]

#sample set 3
trainingdata3 <- dataset[1:185,]
testdata3 <- dataset[186:223,]
datasettd3 <- Dataset1[1:185,1]
datasettest3 <- Dataset1[186:223,1]

#sample set 4
trainingdata4 <- dataset[61:223,]
testdata4 <- dataset[1:60,]
datasettd4 <- Dataset1[61:223,1]
datasettest4 <- Dataset1[1:60,1]

#sample set 5
trainingdata5 <- dataset[1:200,]
testdata5 <- dataset[201:223,]
datasettd5 <- Dataset1[1:200,1]
datasettest5 <- Dataset1[201:223,1]

#KNN method in class package
require(class)

#k = sqrt(223) and odd number, checked with other higher values if k as well
m1 <- knn(trainingdata1, testdata1, datasettd1, k = 15)
m2 <- knn(trainingdata2, testdata2, datasettd2, k = 15)
m3 <- knn(trainingdata3, testdata3, datasettd3, k = 15)
m4 <- knn(trainingdata4, testdata4, datasettd4, k = 40)
m5 <- knn(trainingdata5, testdata5, datasettd5, k = 20)

#confusion matrix for each of the five training set.
table(datasettest1, m1)
table(datasettest2, m2)
table(datasettest3, m3)
table(datasettest4, m4)
table(datasettest5, m5)

#accuracy percentage using mean for each of the five training set.
mean(m1 == datasettest1)
mean(m2 == datasettest2)
mean(m3 == datasettest3)
mean(m4 == datasettest4)
mean(m5 == datasettest5)
