# fpl_analytics
Testing optimisation and advanced analytics to get the best points in Fantasy Premier League

## Predictive Model
I've prepared a classification and regression model using XGBOOST. This is first cut of the model training, which include some hyper parameter tuning but will benefit more through better thought process in managing data, and approach in modelling the regression model. Currently I trained the regression with all data sets, top half & bottom half. However, the application is that the regression is applied after I filter out the bottom half.

For classification & regression codes, you can refer to link below. Also note to run the common setup (for data aggregation part) before running the model training
* Model common setup. [xgboost model common setup](https://github.com/efemkay/fpl_analytics/blob/master/xgboost%20model%20common%20setup.R)
* Classification model training. [xgboost 00 model training classification](https://github.com/efemkay/fpl_analytics/blob/master/xgboost%2000%20model%20training%20classification.R)
* Regression model training. [xgboost 00 model training regression](https://github.com/efemkay/fpl_analytics/blob/master/xgboost%2000%20model%20training%20regression.R)

Then to use the trained model to predict for next Game Week, you need to run these codes
* [Run prediction and append to data frame](https://github.com/efemkay/fpl_analytics/blob/master/xgboost%2001%20model%20currweek%20prediction.R)
* [Run LP optimisation based on prediction data](https://github.com/efemkay/fpl_analytics/blob/master/xgboost%2002%20model%20finetune%20for%20gameweek.R)
