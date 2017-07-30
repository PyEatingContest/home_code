library(caret)
library(mlbench)
library(data.table)
library(ggplot2)
data(Sonar)

set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class ## the outcome data are needed
                               ,p = .75 ## The percentage of data in the training set
                               ,list = FALSE )## The format of the results

## The output is a set of integers for the rows of Sonar
## that belong in the training set.
str(inTrain)

#non-data table way
training <- Sonar[inTrain,]
testing <- Sonar[-inTrain,]

ctrl <- trainControl(method = "repeatedcv"
                     , repeats = 10
                     , classProbs = TRUE #forces calculation of predicted class probabilities to metrics like ROC
                     , summaryFunction = twoClassSummary) #computes measures specific to 2 class problems

#Partial least squares analysis - is not scale invariant method so scaling is needed
plsFit <- train(Class ~ . # Dot means to use all variables but the outcome
                , data = training
                , method = "pls"
                , tuneLength = 30 #tries integers 1 to 15 for tuning
                , metric = "ROC"
                , trControl = ctrl #Defaults to 'boot' if you don't explicitly note this
                , preProc = c("center", "scale")) ## Center and scale the predictors for the training set and all future samples.

#Penalized logistic regression
plrGrid <- expand.grid(cp = "bic", lambda = seq(.25,10,by=.25))
plrFit <- train(Class ~ .
                , data = training
                , method = "plr"
                , preProc = c("center", "scale")
                , trControl = ctrl
                , metric = "ROC"
                , tuneGrid = plrGrid)

##Ada boost model
adaGrid <- expand.grid(mfinal = 100, maxdepth = 5, coeflearn = 'Freund')
adaFit <- train(Class ~ .
                , data = training
                , method = "AdaBoost.M1"
                , trControl = ctrl
                , metric = "ROC"
                , tuneGrid = adaGrid)

##Ada bag model
adabagGrid <- expand.grid(mfinal = 10, maxdepth = 3)
adabagFit <- train(Class ~ .
                , data = training
                , method = "AdaBag"
                , trControl = ctrl
                , metric = "ROC"
                , tuneGrid = adabagGrid)

##Regularized random forest
rrfFit <- train(Class ~ .
                , data = training
                , method = "RRF"
                , preProc = c("center", "scale")
                , trControl = ctrl
                , metric = "ROC")

##Modeled average neural network
mannFit <- train(Class ~ .
                , data = training
                , method = "avNNet"
                , preProc = c("center", "scale")
                , trControl = ctrl
                , metric = "ROC")

##Logit Boost
logitBoostFit <- train(Class ~ .
                 , data = training
                 , method = "LogitBoost"
                 , preProc = c("center", "scale")
                 , trControl = ctrl
                 , metric = "ROC")

#GLM elastic net model
glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))
glmnet_fit <- train(Class ~ ., data = training,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid,
                    trControl = ctrl)

plr_class <- predict.train(plrFit, newdata = testing)
pls_class <- predict(plsFit, newdata = testing)
net_class <- predict(glmnet_fit, newdata = testing)
ada_class <- predict.train(adaFit, newdata = testing)
rrf_class <- predict.train(rrfFit, newdata = testing)
adabag_class <- predict.train(adabagFit, newdata = testing)
mannFit_class <- predict.train(mannFit, newdata = testing)
lbFit_class <- predict.train(logitBoostFit, newdata = testing)

confusionMatrix(plr_class, testing$Class)
confusionMatrix(pls_class, testing$Class)
confusionMatrix(net_class, testing$Class)
confusionMatrix(ada_class, testing$Class)
confusionMatrix(rrf_class, testing$Class)
confusionMatrix(adabag_class, testing$Class)
confusionMatrix(mannFit_class, testing$Class)

ggplot(plrFit)
ggplot(plsFit)
ggplot(glmnet_fit)
ggplot(adaFit)

##Compare performance on resamples between the tested models. Works because
##each fold used the same seed.
resamps <- resamples(list(pls = plsFit, plr = plrFit
                          , net = glmnet_fit, rrf = rrfFit
                          , ada = adaFit, adabag = adabagFit
                          , mann = mannFit, logitboost = logitBoostFit))
bwplot(resamps)
summary(resamps)


inTrain <- createDataPartition(y = Glass$Type ## the outcome data are needed
                               ,p = .75 ## The percentage of data in the training set
                               ,list = FALSE )##

training <- Glass[inTrain,]
testing <- Glass[-inTrain,]

ctrl <- trainControl(method = "repeatedcv"
                     , repeats = 15) #computes measures specific to 2 class problems

knnFit <- train(Type ~ . # Dot means to use all variables but the outcome
                , data = training
                , method = "kknn"
                , tuneLength = 15 #tries integers 1 to 15 for tuning
                , trControl = ctrl #Defaults to 'boot' if you don't explicitly note this
                , preProcess = c("center", "scale")) ## Center and scale the predictors for the training set and all future samples.

knn_class <- predict.train(knnFit, newdata = testing)
confusionMatrix(knn_class, testing$Type)
