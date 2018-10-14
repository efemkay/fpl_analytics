## XGBOOST model training
## This model focus on regression for event_points.

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
ixtrain = createDataPartition(df_merge88$event_points.y, p=0.8, list=FALSE)
dftrain = df_merge88[ixtrain, c(predictors, targetregress)]
dftest  = df_merge88[-ixtrain, c(predictors, targetregress)]
## Convert predictors as sparse matrix. -1 to remove (Intercept) variable automatically created
sparse_data_train = sparse.model.matrix(object= (event_points.y)~., data = dftrain)[,-1]
sparse_data_test = sparse.model.matrix(object= (event_points.y)~., data = dftest)[,-1]
## Convert label as numeric. Target variable is also expected as a discrete numeric {0,1} and not factor.
labeltrn = as.numeric(dftrain$event_points.y)
labeltst = as.numeric(dftest$event_points.y)
## Create a new xgb.DMatrix
dtrain = xgb.DMatrix(data=sparse_data_train, label=labeltrn)
dtest = xgb.DMatrix(data=sparse_data_test, label=labeltst)


## Training the XGBOOST model ######################################################################

## Cross Validation. Learn about this more later...

## Simple Version using xgboost. Here we use the advanced version using xgb.train
xgbModel <- xgb.train(data = dtrain
                      , max_depth = 7
                      , min_child_weight = 1
                      , gamma = 5
                      , eta = 0.1
                      , nrounds = 700
                      , early_stopping_rounds = 50
                      , subsample = 0.5
                      , colsample_bytree = 2/3
                      , watchlist = list(train=dtrain, test=dtest)
                      , verbose = 1
                      , print_every_n = 10
                      , booster = "gbtree"
                      , objective = "reg:linear"
                      , eval_metric = "rmse"
                      )

## Predict on Test dataset #########################################################################
xgbpred <- predict(object=xgbModel, newdata=sparse_data_test)

# par(mfrow=c(2,1), mar=c(4,5,1,2)) # bottom left top right
# plot(labeltst, xgbpred)
# lines(c(0,5), c(0,5), col="blue")
# plot(labeltst, xgbpred-labeltst)

caret::RMSE(xgbpred, labeltst) %>% print
caret::MAE(xgbpred, labeltst) %>% print

df_mergetst = cbind(dftest, xgbpred)
ix = sapply(df_mergetst, FUN=function(x){is.numeric(x)}) %>% which
corm = df_mergetst[df_mergetst$event_points.y>quantile(df_mergetst$event_points.y, tophalf_quant),ix] %>% cor
corm[,23] %>% sort(decreasing = TRUE) %>% as.data.frame %>% head(20) %>% print

# with(df_mergetst[df_mergetst$event_points.y<=quantile(df_mergetst$event_points.y, tophalf_quant),],
#      plot(event_points.y, xgbpred, main="Tophalf, No")
# )
# with(df_mergetst[df_mergetst$event_points.y>quantile(df_mergetst$event_points.y, tophalf_quant),],
#      plot(event_points.y, xgbpred, main="Tophalf, Yes")
# )


## View feature importance/influence from the learnt model
## Feature importance is similar to R gbm package’s relative influence (rel.inf).
importance_matrix = xgb.importance(colnames(sparse_data_train)
                                    , model = xgbModel
                                    )
# xgb.plot.importance(importance_matrix = importance_matrix)
## View the trees from a model
## You can dump the tree you learned using xgb.dump into a text file.
## You can plot the trees from your model using ```xgb.plot.tree``
# xgb.dump(xgbModel, with.stats = T)
# xgb.plot.tree(model = xgbModel)

## Save the model
xgb.save(xgbModel, "./Output/xgbregress.model")

## Week05 Prediction ###############################################################################
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
  # currweekpred = currweekpred %>% exp %>% rescale(to=c(0,15))

  dfcurrweekpred = data.frame(event_points_pred=currweekpred)
  return(dfcurrweekpred)
}

## Get prediction

## Read & Clean Summary & Fixture Data
df_week05 = read.csv("./Data/week05_01summary.csv")  # current week
df_week06 = read.csv("./Data/week06_01summary.csv")  # future week
df_fixt05 = read.csv("./Data/week05_04fixture.csv")
df_fixt06 = read.csv("./Data/week06_04fixture.csv")
## rescale them all. Do this for each week separately to ensure scale is not skewed by cumulative
df_week05 = rescale_df(df_week05, df_fixt05)
## Feed current week data into prediction
dfcurrweekpred = predict_tophalf(df_week05, xgbModel)

## cbind current week with the prediction
df_week05 = cbind(df_week05, dfcurrweekpred)
## Merge current week with next week outcome
df_merge06 = merging_df(df_week05,df_week06)

## Plot how it fares regressively
par(mfrow=c(1,1),mar=c(5,4,3,2))  # bottom, left, top, right
with(df_merge06
     , plot(event_points.y, event_points_pred)
)

## NOTES ###########################################################################################

## References
# https://www.hackerearth.com/practice/machine-learning/..
# machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/

## Parameters for Tree Booster
# nrounds[default=100] // should be tuned using cv
# eta[default=0.3][range: (0,1)] // typically 0.01 - 0.3
# gamma[default=0][range: (0,Inf)] // use 5 and see the performance
# max_depth[default=6][range: (0,Inf)] // should be tuned using cv, larger datasets more depth
# min_child_weight[default=1][range:(0,Inf)] // should be tuned using cv
# subsample[default=1][range: (0,1)] // typically 0.5 - 0.8
# colsample_bytree[default=1][range: (0,1)] // typically 0.5 - 0.9
# lambda[default=0] // control L2 regularisation (Ridge)
# alpha[default=1] // control L1 regularisation (Lasso)

## Parameters for Linear Booster. Using linear booster has relatively lesser parameters to tune, 
## hence it computes much faster than gbtree booster.
# nrounds[default=100]
# lambda[default=0]
# alpha[default=1]

## Learning Task Parameters. These parameters specify methods for the loss function and model
## evaluation. In addition to the parameters listed below, you are free to use a customized 
## objective / evaluation function.
# Objective[default=reg:linear]
# - reg:linear - for linear regression
# - binary:logistic - logistic regression for binary classification.
# - multi:softmax - multiclassification using softmax objective.
# - multi:softprob - multiclassification using softmax objective.
# eval_metric [no default, depends on objective selected] // These metrics are used to evaluate a
# // model's accuracy on validation data. For regression, default metric is RMSE. For classification, default metric is error.
# - mae - Mean Absolute Error (used in regression)
# - Logloss - Negative loglikelihood (used in classification)
# - AUC - Area under curve (used in classification)
# - RMSE - Root mean square error (used in regression)
# - error - Binary classification error rate [#wrong cases/#all cases]
# - mlogloss - multiclass logloss (used in classification)