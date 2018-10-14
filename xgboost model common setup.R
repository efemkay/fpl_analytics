
## General Setups & Global Var #####################################################################
targetclass = c('tophalf')
targetregress = c('event_points.y')
predictors = c('status','now_cost',
               'value_form','value_season','in_dreamteam', 'dreamteam_count',
               'selected_by_percent','form',
               'transfers_out', 'transfers_in',
               'transfers_out_event','transfers_in_event',
               'total_points', 'points_per_game', 'event_points.x','ep_this','ep_next',
               'minutes','bonus','influence','creativity','threat','ict_index',
               'element_type','team', 'is_home', 'difficulty')
set.seed(1234)         # to ensure replicability, choices of 1234, 1511, 9007, 9013, 9137
tophalf_quant = 0.66   # how to split event_points, what percentile
predprob_quant = 0.66  # which quantile of pred confidence to take as 1. This ensure top quantile is alway chosen

## General Functions ###############################################################################
## Function. Combine difficulty, and rescale & transform data to normalize and normally distribute
rescale_df = function(dfx, df_fixt){
  dfx = merge(dfx, 
              df_fixt[,c('player_id', 'opponent_short_name', 'is_home', 'difficulty')], 
              by.x=c('id'), by.y=c('player_id'), sort=FALSE)
  ix = sapply(dfx, FUN=function(x)is.numeric(x)) %>% which()
  ix = ix[-ix[c('id')]]
  dfx[,ix] = dfx[,ix] %>% apply(2, FUN=function(x){round(rescale(x, to=c(1,100)),1)}) %>% as.data.frame
  dfx[,ix] = dfx[,ix] %>% apply(2, FUN=function(x){log(x)}) %>% as.data.frame
  return(dfx)
}
## Function. Merging current week with 'future' week to have outcome variable event_points.y
## Mutate for tophalf, which is more aptly top 33 percentile
merging_df = function(dfcurrx, dfnextx){
  df_mergex = merge(dfcurrx, dfnextx[,c('id', 'event_points')], by=c('id'), sort=FALSE)
  df_mergex = mutate(df_mergex
                     , special = as.factor(special)
                     , tophalf = as.logical(event_points.y>quantile(event_points.y,tophalf_quant))
  )
}

## Prep and Cleanup Data ###########################################################################
## Read & Clean Data
df_week01 = read.csv("./Data/week01_01summary.csv")
df_week02 = read.csv("./Data/week02_01summary.csv")
df_week03 = read.csv("./Data/week03_01summary.csv")
df_week04 = read.csv("./Data/week04_01summary.csv")
df_week05 = read.csv("./Data/week05_01summary.csv")
df_week06 = read.csv("./Data/week06_01summary.csv")
df_week07 = read.csv("./Data/week07_01summary.csv")
df_week08 = read.csv("./Data/week08_01summary.csv")
## Read team summary data
df_fixt01 = read.csv("./Data/week01_04fixture.csv")
df_fixt02 = read.csv("./Data/week02_04fixture.csv")
df_fixt03 = read.csv("./Data/week03_04fixture.csv")
df_fixt04 = read.csv("./Data/week04_04fixture.csv")
df_fixt05 = read.csv("./Data/week05_04fixture.csv")
df_fixt06 = read.csv("./Data/week06_04fixture.csv")
df_fixt07 = read.csv("./Data/week07_04fixture.csv")
df_fixt08 = read.csv("./Data/week08_04fixture.csv")

## rescale them all. Do this for each week separately to ensure scale is not skewed by cumulative
df_week01 = rescale_df(df_week01, df_fixt01)
df_week02 = rescale_df(df_week02, df_fixt02)
df_week03 = rescale_df(df_week03, df_fixt03)
df_week04 = rescale_df(df_week04, df_fixt04)
df_week05 = rescale_df(df_week05, df_fixt05)
df_week06 = rescale_df(df_week06, df_fixt06)
df_week07 = rescale_df(df_week07, df_fixt07)
df_week08 = rescale_df(df_week08, df_fixt08)
## merge current week with previous week to get "prediction"
df_merge02 = merging_df(df_week01, df_week02)
df_merge03 = merging_df(df_week02, df_week03)
df_merge04 = merging_df(df_week03, df_week04)
df_merge05 = merging_df(df_week04, df_week05)
df_merge06 = merging_df(df_week05, df_week06)
df_merge07 = merging_df(df_week06, df_week07)
df_merge08 = merging_df(df_week07, df_week08)

## df_merge88 is for model training purposes
df_merge88 = rbind(df_merge02
                   , df_merge03
                   , df_merge04
                   , df_merge05
                   , df_merge06
                   , df_merge07
                   # , df_merge08
                   )