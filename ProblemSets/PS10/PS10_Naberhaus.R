library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(tidyverse)
library(mlr)
library(magrittr)

adult <- read.csv("C:/Users/Grant/Downloads/adult.data", header=FALSE)
View(adult)

#Change Income to Numeric
adult$V15 <- as.numeric(adult$V15)
# 1: <=50k, 2: >50K

#Break up data
n <- nrow(adult)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
adult.train <- adult[train,]
adult.test  <- adult[test, ]

#Define Task
#For LOGIT
theTask <- makeRegrTask(id = "taskname", data = adult.train, target = "V15")
print(theTask)

#Everything Else
theTask <- makeClassifTask(id = "taskname", data = adult.train, target = "V15")
print(theTask)

#CrossValidation
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

#TuningMethod
tuneMethod <- makeTuneControlRandom(maxit = 10L)


#Tree 
#Algo
predAlg <- makeLearner("classif.rpart", predict.type = "response")

#Setup Parameters
modelParams <- makeParamSet(makeNumericParam("minsplit",lower=10,upper=50)
                            ,makeNumericParam("minbucket",lower=5,upper=50)
                            ,makeNumericParam("cp",lower=0.001,upper=0.2))
#ActualTuning
tunedModel <- tuneParams(learner = predAlg, 
                         task = theTask, 
                         resampling = resampleStrat, 
                         measures = list(f1,gmean), 
                         par.set = modelParams, 
                         control = tuneMethod, 
                         show.info = TRUE)
# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(f1, gmean))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = adult.test)
print(head(prediction$data))


#LOGIT
#Algo
predAlg <- makeLearner("regr.glmnet", predict.type = "response")

#Set up parameters
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=3),makeNumericParam("alpha",lower=0,upper=1))

#ActualTuning
tunedModel <- tuneParams(learner = predAlg, 
                         task = theTask, 
                         resampling = resampleStrat, 
                         measures = list(f1,gmean), 
                         par.set = modelParams, 
                         control = tuneMethod, 
                         show.info = TRUE)


# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(f1, gmean))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = adult.test)
print(head(prediction$data))

#Nueral Net
#Algo
predAlg <- makeLearner("classif.nnet", predict.type = "response")

#Setup Parameters
modelParams <- makeParamSet(makeNumericParam("size",lower=1,upper=10)
                            ,makeNumericParam("decay",lower=0.1,upper=0.5)
                            ,makeNumericParam("maxit",lower=1000,upper=1000))
#ActualTuning
tunedModel <- tuneParams(learner = predAlg, 
                         task = theTask, 
                         resampling = resampleStrat, 
                         measures = list(f1, gmean), 
                         par.set = modelParams, 
                         control = tuneMethod, 
                         show.info = TRUE)
# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(f1, gmean))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = adult.test)
print(head(prediction$data))

#NaiveBayes
#Algo
predAlg <- makeLearner("classif.naiveBayes", predict.type = "response")
# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(f1, gmean))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = adult.test)
print(head(prediction$data))

#kNN
#Algo
predAlg <- makeLearner("classif.kknn", predict.type = "response")

#Setup Parameters
modelParams <- makeParamSet(makeNumericParam("k",lower=1,upper=30))

#ActualTuning
tunedModel <- tuneParams(learner = predAlg, 
                         task = theTask, 
                         resampling = resampleStrat, 
                         measures = list(f1,gmean), 
                         par.set = modelParams, 
                         control = tuneMethod, 
                         show.info = TRUE)
# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(f1, gmean))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = adult.test)
print(head(prediction$data))

#SVM
predAlg <- makeLearner("classif.svm", predict.type = "response")

#Setup Parameters
modelParams <- makeParamSet( makeNumericParam("cost",lower= 0.02,upper=20000000000)
                            ,makeNumericParam("gamma",lower=0.02,upper=20000000000))
#ActualTuning
tunedModel <- tuneParams(learner = predAlg, 
                         task = theTask, 
                         resampling = resampleStrat, 
                         measures = list(f1,gmean), 
                         par.set = modelParams, 
                         control = tuneMethod, 
                         show.info = TRUE)
# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(f1, gmean))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = adult.test)
print(head(prediction$data))



