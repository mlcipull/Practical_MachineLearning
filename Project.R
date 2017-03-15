rm(list=ls())
library(caret)
library(randomForest)
workdir = "/Users/michellecipullo/Desktop/Coursera_DataScience/PracticalMachineLearning/Project/"
setwd(workdir)

training.Dataset = read.csv("pml-training.csv",na.strings = c("NA"," "))
testing.Dataset  = read.csv("pml-testing.csv",na.strings = c("NA"," "))

#vars 1 through 7 are about the time stamp, person name, etc. None of these will help with
# prediction so get rid of them. Also after 60 of the original are stdevs, etc. and other
# things that wont be useful.
vars.to.keep = names(testing.Dataset[,colSums(is.na(testing.Dataset)) == 0])[8:59]

train.data  = training.Dataset[,c(vars.to.keep,"classe")]
test.data   = testing.Dataset[,c(vars.to.keep,"problem_id")]

#---------------------
# guidelines from lecture:
# Large dataset use:
#   60% for training
#   20% for testing
#   20% for validation
# Medium dataset use:
#   60% for training
#   40% for testing
#------------------------------

#Create training and test set 
inTrain = createDataPartition(y=train.data$classe,p=0.6,list=FALSE)
training = train.data[inTrain,]
testing  = train.data[-inTrain,]

classe.opts= unique(training$classe)
#[1] A B C D E
#Levels: A B C D E
#They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.
# So A,B,C,D,E so 5 options of whether correct or incorrect.


#Using random forest model shown in class. This is a classification/regression technique
randomForest.fit = randomForest(classe ~ ., data = training, ntree = 700)

#Use this method to predict outcomes for the testing portion of your training set
rF.fit.predict = predict(randomForest.fit,testing,type = "class")
#check performance
confusionMatrix(rF.fit.predict,testing$classe)

#Now test on the validation testing data read in at the beginning
predict.validTest = predict(randomForest.fit,test.data,type="class")

#Print the results of what was predicted for the validation/test data read in at the beginning
predict.validTest
