## XGBOOST model training
## This model focus on classifying the tophalf. Thus target is binary.

rm(list=ls())

library(utils)   # for read.csv
library(dplyr)   # for piping %>%
library(caret)   # for training model
library(Matrix)  # for sparse.model.matrix
library(xgboost) # for xgboost model training
library(scales)

## Source Setups ###################################################################################
## For set.seed()
## For variables: predictors, tophalf_quant, predprob_quant
## For functions: rescale_df(), merging_df()
## For datasets: df_merge88 for model training
source("./xgboost model common setup.R")

## Prep the train & test dataset, & subset to relevant variables. predictors from common setup
ixtrain = createDataPartition(df_merge88$tophalf, p=0.8, list=FALSE)
dftrain = df_merge88[ixtrain, c(predictors, targetclass)]
dftest  = df_merge88[-ixtrain, c(predictors, targetclass)]
## Convert predictors as sparse matrix. -1 to remove (Intercept) variable automatically created
sparse_data_train = sparse.model.matrix(object= (tophalf)~., data = dftrain)[,-1]
sparse_data_test = sparse.model.matrix(object= (tophalf)~., data = dftest)[,-1]
## Convert label as numeric. Target variable is also expected as a discrete numeric {0,1} and not factor.
labeltrn <- as.numeric(dftrain$tophalf)
labeltst = as.numeric(dftest$tophalf)
## Create a new xgb.DMatrix
dtrain = xgb.DMatrix(data=sparse_data_train, label=labeltrn)
dtest = xgb.DMatrix(data=sparse_data_test, label=labeltst)


## Training the XGBOOST model ######################################################################

## Cross Validation. Learn about this more later...

## Advanced version using xgb.train. Simpler xgboost() avoided as it lack hyper parameters setting
xgbModel <- xgb.train(data = dtrain
                      , max_depth = 7
                      , min_child_weight = 1
                      , gamma = 5
                      , eta = 0.1
                      , nrounds = 700
                      , early_stopping_rounds = 50
                      , subsample = 0.3
                      , colsample_bytree = 0.3
                      , watchlist = list(train=dtrain, test=dtest)
                      , verbose = 1
                      , print_every_n = 10
                      , objective = "binary:logistic"
                      , eval_metric = "error"
                      )

## Predict on Test dataset #########################################################################
xgbpred <- predict(object=xgbModel, newdata=sparse_data_test)

## classification testing
## Split by population quantile instead of prob or confidence. Here we choose to be top 1/3
xgbprediction = as.numeric(xgbpred>quantile(xgbpred, predprob_quant))
err = mean(xgbprediction!=labeltst)
paste0("Top 1/3 chosen, i.e. ", sum(xgbprediction), " out of ", length(xgbpred)) %>% print
paste0("The error rate is ", round(err,3)*100, "%") %>% print
data.frame(labeltst, xgbprediction) %>% table %>% print

## View feature importance/influence from the learnt model
## Feature importance is similar to R gbm package’s relative influence (rel.inf).
importance_matrix <- xgb.importance(colnames(sparse_data_train)
                                    , model = xgbModel
                                    )
# print(importance_matrix)
par(mar=c(4,2,2,2))  # bottom,left,top,right
xgb.plot.importance(importance_matrix = importance_matrix[1:20,])

## View the trees from a model
## You can dump the tree you learned using xgb.dump into a text file.
# xgb.dump(xgbModel, with.stats = T)

## You can plot the trees from your model using ```xgb.plot.tree``
# xgb.plot.tree(model = xgbModel)

## Save the model
xgb.save(xgbModel, "./Output/xgbclass.model")

## Week06 Prediction ###############################################################################
predict_tophalf = function(dfcurrweek, xgbModel){
  
  ## Supply data frame of the current week
  dfcurrweek = mutate(dfcurrweek
                      , special = as.factor(special)
                      , event_points.x = event_points
  )
  ## Subset to only relevant variables
  dfcurrweek = dfcurrweek[,predictors]
  
  ## Convert predictors as sparse matrix. -1 to remove (Intercept) variable automatically created
  sparse_newdata = sparse.model.matrix(object= ~., data = dfcurrweek)[,-1]
  ## Convert label as numeric. Target variable is also expected as a discrete numeric {0,1} and not factor.
  
  ## Prediction. Use median(currweekpred) to ensure I always get half of current week pop
  currweekpred = predict(object=xgbModel, newdata=sparse_newdata)
  currweekprediction = as.numeric(currweekpred>quantile(currweekpred, predprob_quant))
  currweekprediction %>% sum %>% print
  
  ## cbind prediction outcome as dataframe
  dfcurrweekpred = cbind(tophalf_pred=currweekprediction, tophalf_conf=currweekpred) %>% as.data.frame
  return(dfcurrweekpred)
}

## Get prediction

## Read & Clean Summary & Fixture Data
df_week05 = read.csv("./Data/week05_01summary.csv")  # current week
df_week06 = read.csv("./Data/week06_01summary.csv")  # future week
df_fixt05 = read.csv("./Data/week05_04fixture.csv")  # current fixture
df_fixt06 = read.csv("./Data/week06_04fixture.csv")  # next fixture
## Rescale them all. Do this for each week separately to ensure scale is not skewed by cumulative
df_week05 = rescale_df(df_week05, df_fixt05)
## Feed current week data into prediction
dfcurrweekpred = predict_tophalf(df_week05, xgbModel)

## cbind current week with the prediction
df_week05 = cbind(df_week05, dfcurrweekpred)
## Merge current week with next week outcome
df_merge06 = merging_df(df_week05,df_week06)

## Print out table of points for tophalf_pred==0 and tophalf_pred==1
table(df_merge06[,c('event_points.y', 'tophalf_pred', 'element_type')])[,c('0'),] %>% print
table(df_merge06[,c('event_points.y', 'tophalf_pred', 'element_type')])[,c('1'),] %>% print

## Optional, plot to see how it fares regressively
par(mfrow=c(2,1), mar=c(5,5,2,2))  # bottom, left, top, right
with(df_merge06[df_merge06$tophalf_pred==1,]
     , plot(event_points.y, tophalf_conf, main="Tophalf, Yes")
     )
with(df_merge06[df_merge06$tophalf_pred==0,]
     , plot(event_points.y, tophalf_conf, main="Tophalf, No")
     )
