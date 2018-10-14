rm(list=ls())
## Load libraries
library(utils)   # for read.csv
library(xgboost) # for xgb.load
library(Matrix)  # for sparse.model.matrix
library(scales)  # for rescale
library(dplyr)   # for mutate and pipeline %>%

## Prediction Test Setups ##########################################################################

## For variables: predictors, tophalf_quant, predprob_quant
## For functions: rescale_df()
source("./xgboost 00 model training classification.R")
source("./xgboost 00 model training regression.R")
source("./xgboost model common setup.R")
currweek = 7

## Main Code for Current Week Prediction ###########################################################

## Load current week dataset, from saved csv
## Week02-03
if(currweek==2){
  df_currsumm = read.csv("./Data/week02_01summary.csv")
  df_currfixt = read.csv("./Data/week02_04fixture.csv")
  df_currteam = read.csv("./Output/faizmk_enhanced_team_wk02.csv")
  df_nextsumm = read.csv("./Data/week03_01summary.csv")
}
## Week03-04
if(currweek==3){
  df_currsumm = read.csv("./Data/week03_01summary.csv")
  df_currfixt = read.csv("./Data/week03_04fixture.csv")
  df_currteam = read.csv("./Output/faizmk_enhanced_team_wk03.csv")
  df_nextsumm = read.csv("./Data/week04_01summary.csv")
}
## Week04-05
if(currweek==4){
  df_currsumm = read.csv("./Data/week04_01summary.csv")
  df_currfixt = read.csv("./Data/week04_04fixture.csv")
  df_currteam = read.csv("./Output/faizmk_enhanced_team_wk04.csv")
  df_nextsumm = read.csv("./Data/week05_01summary.csv")
}
## Week05-06
if(currweek==5){
  df_currsumm = read.csv("./Data/week05_01summary.csv")
  df_currfixt = read.csv("./Data/week05_04fixture.csv")
  df_currteam = read.csv("./Output/faizmk_enhanced_team_wk05.csv")
  df_nextsumm = read.csv("./Data/week06_01summary.csv")
}
## Week06-07
if(currweek==6){
  df_currsumm = read.csv("./Data/week06_01summary.csv")
  df_currfixt = read.csv("./Data/week06_04fixture.csv")
  df_currteam = read.csv("./Output/faizmk_enhanced_team_wk06.csv")
  df_nextsumm = read.csv("./Data/week07_01summary.csv")
}
## Week07-08
if(currweek==7){
  df_currsumm = read.csv("./Data/week07_01summary.csv")
  df_currfixt = read.csv("./Data/week07_04fixture.csv")
  df_currteam = read.csv("./Output/faizmk_enhanced_team_wk07.csv")
  df_nextsumm = read.csv("./Data/week08_01summary.csv")
}

## Load xgbmodel for class and regression
xgbModelClass = xgb.load("./Output/xgbclass.model")
xgbModelRegress = xgb.load("./Output/xgbregress.model")
# xgbModelClass = xgb.load("./Output/xgbclass_wk05.model")
# xgbModelRegress = xgb.load("./Output/xgbregress_wk05.model")
# xgbModelClass = xgb.load("./Output/old_xgbclass_wk05.model")
# xgbModelRegress = xgb.load("./Output/old_xgbregress_wk05.model")

## Function to predict (based on current week) and append to dataframe
predict_nextweek = function(dfcurrweek, xgbModelClass, xgbModelRegress){
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
  
  ## Prediction
  tophalf_pred_conf = predict(object=xgbModelClass, newdata=sparse_newdata)
  tophalf_pred_bin = as.numeric(tophalf_pred_conf>predprob_quant)
  event_points_pred = predict(object=xgbModelRegress, newdata=sparse_newdata)
  # event_points_pred = event_points_pred %>% exp %>% rescale(to=c(0,15))
  currweekpred = cbind(tophalf_pred_conf, tophalf_pred_bin, 
                       event_points_pred) %>% as.data.frame
  return(currweekpred)
}

## Rescale and feed df to prediction function
df_currentweek = rescale_df(df_currsumm, df_currfixt)
df_nextpredict = predict_nextweek(df_currentweek, xgbModelClass, xgbModelRegress)
df_merge = cbind(df_currsumm, df_nextpredict)

## Test predictability on 'next' week outcome ######################################################

## Merge current week + prediction with next week outcome
df_mergenext = merging_df(df_merge, df_nextsumm)

## See how it fares (i.e. the tophalf binning)
par(mfrow=c(2,1), mar=c(5,5,2,2)) # bottom, left, top, right
with(df_mergenext[df_mergenext$tophalf_pred_bin==0,],
     plot(event_points.y, event_points_pred, main="Tophalf, No")
)
with(df_mergenext[df_mergenext$tophalf_pred_bin==1,],
     plot(event_points.y, event_points_pred, main="Tophalf, Yes")
)

ix = sapply(df_mergenext, FUN=function(x){is.numeric(x)}) %>% which
corm = df_mergenext[df_mergenext$tophalf_pred_bin==1,ix] %>% cor
corm[,51] %>% sort(decreasing = TRUE) %>% as.data.frame %>% head(20) %>% print

with(df_mergenext[df_mergenext$tophalf_pred_bin==0,],
     plot(event_points.y, ep_next, main="Tophalf, No")
)
with(df_mergenext[df_mergenext$tophalf_pred_bin==1,],
     plot(event_points.y, ep_next, main="Tophalf, Yes")
)
