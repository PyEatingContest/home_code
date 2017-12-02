library(tidyverse)
library(caret)

prostate <- read_tsv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data',
                 col_names = T, na = '?') %>% select(-X1)
prostate$train <- as.factor(prostate$train)
pairs(prostate)

# Scale all numeric variables and boxplot their distributions
pre.prostate <- prostate %>% mutate_if(is.numeric, scale)

temp <- pre.prostate %>% 
    select(-train) %>% 
    gather(key = variable, value = measurement)

ggplot(temp, aes(variable, measurement)) + geom_boxplot()

# Run through common classifcation models
train <- createDataPartition(y = prostate$train, p = 0.75, list = F)
training <- prostate[train, ]
testing <- prostate[-train, ]

control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)

glmnet.mod <- train(lpsa ~ ., data = prostate, 
                    method = 'glmnet', 
                    trControl = control)

gbm.mod <- train(lpsa ~ ., data = prostate, 
                    method = 'gbm', 
                    trControl = control)

ridge.mod <- train(lpsa ~ ., data = prostate, 
                     method = 'ridge', 
                     trControl = control)

linStep.mod <- train(lpsa ~ ., data = prostate, 
                   method = 'lmStepAIC', 
                   trControl = control)

pls.mod <- train(lpsa ~ ., data = prostate, 
                 method = 'pls', 
                 trControl = control)

randomforest.mod <- train(lpsa ~ ., data = prostate,
                          method = 'RRF',
                          trControl = control)

results <- resamples(list(RIDGE=ridge.mod,
                          GBM=gbm.mod,
                          GLM=glmnet.mod,
                          STEP=linStep.mod,
                          PLS=pls.mod,
                          RRF=randomforest.mod))
summary(results)
bwplot(results)
dotplot(results)
