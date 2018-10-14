## Improved prediction using XGBOOST classification
## This is testing script
## 1. Load current week 04, next week 05, and myteam for week 04
## 2. Train model using week 01-04, predict for next week 05
## 3. Feed model prediction week 05 together with curr week 04 and next week 05
## improved the selection by incorporating week1 points and now_cost, weighted with ICT

# rm(list=ls())

library(lpSolveAPI)
library(scales)
library(dplyr)

## Setups for Optimisation #########################################################################
nsameteam = 2          # max 3 player from same club
bankvalue = 994        # current team value + in bank
ntrade = 14             # 1-14 if mid season transfer, 0 if overall overhaul
formation = c(1,3,4,3) # if using same formation throughout

## GROUP OF FUNCTIONS ##############################################################################

## Starting team optimiser
optimise_chosen = function(dfsummary, nsameteam, bankvalue, ntrade=0) {
  ## Initialise LP model
  # Define decision variables as how to turn on/off (binary) 15 of the 470+ potential players
  decVarNo = nrow(dfsummary)
  lprec    = make.lp(0, decVarNo)
  lp.control(lprec,sense="max")
  set.type(lprec, seq(1,decVarNo), type = "binary")
  
  ## Set LP model objective function
  # Objective, by default, is to maximise player modified total points. This can be changed to ICT or 
  # other performance/index past few games
  decVar = dfsummary$ObjPoints
  set.objfn(lprec, decVar)
  
  ## Set LP model constraints
  # For Enhanced, formation is set for 4-4-2. Hence total team is GKP==2, DEF==5, MID==5 and FWD==3.
  # Total of 15
  add.constraint(lprec, as.numeric(dfsummary$element_type=="GKP"), type="=", 2)
  add.constraint(lprec, as.numeric(dfsummary$element_type=="DEF"), type="=", 5)
  add.constraint(lprec, as.numeric(dfsummary$element_type=="MID"), type="=", 5)
  add.constraint(lprec, as.numeric(dfsummary$element_type=="FWD"), type="=", 3)
  if(ntrade>0){
    add.constraint(lprec, as.numeric(dfsummary$myteam), type=">=", 15-ntrade)
    tophalfcount = sum(as.numeric(dfsummary$myteam==TRUE)*(dfsummary$tophalf==1))+1
    add.constraint(lprec, as.numeric(dfsummary$tophalf==1), type=">=", tophalfcount)
  } else{
    add.constraint(lprec, as.numeric(dfsummary$tophalf==1), type=">=", 15)
  }
  # no more than 2 from the same club. this is constrained further for diversification
  for (i in unique(dfsummary$team)) {
    add.constraint(lprec, as.numeric(dfsummary$team==i), type="<=", nsameteam)
  }
  # constraint for cost must be less than 1000 or so. we use current cost of week1.
  add.constraint(lprec, dfsummary$now_cost, type="<=", rhs=bankvalue)
  # custom constraint to exclude non-availabel players, set to 0 to exclude them
  add.constraint(lprec, as.numeric(!grepl("a|s|d", dfsummary$status)), type="=", 0)
  
  ## Solve the LP model
  solve.lpExtPtr(lprec)
  results = get.variables(lprec)
  dfchosen = dfsummary[which(results==1),]
}
optimise_first11 = function(dfsummary) {
  ## Initialise LP model
  # Define decision variables as how to turn on/off (binary) 15 of the 470+ potential players
  decVarNo = nrow(dfsummary)
  lprec    = make.lp(0, decVarNo)
  lp.control(lprec,sense="max")
  set.type(lprec, seq(1,decVarNo), type = "binary")
  
  ## Set LP model objective function
  # Objective, by default, is to maximise player modified total points. This can be changed to ICT or 
  # other performance/index past few games
  decVar = dfsummary$ObjPoints
  set.objfn(lprec, decVar)
  
  ## Set LP model constraints
  # Minimum formation. GKP==1, DEF>=3, MID>=0 and FWD>=1. Total of 11
  add.constraint(lprec, as.numeric(dfsummary$element_type=="GKP"), type="=", 1)
  add.constraint(lprec, as.numeric(dfsummary$element_type=="DEF"), type=">=", 3)
  add.constraint(lprec, as.numeric(dfsummary$element_type=="MID"), type=">=", 0)
  add.constraint(lprec, as.numeric(dfsummary$element_type=="FWD"), type=">=", 1)
  add.constraint(lprec, rep(1, nrow(dfsummary)), type="=", 11)

  ## Solve the LP model
  solve.lpExtPtr(lprec)
  results = get.variables(lprec)
  df_11s = dfsummary[which(results==1),]
  df_11s = arrange(df_11s, element_type, desc(ObjPoints))
  df_11s = rbind(df_11s[df_11s$element_type=="GKP",]
                 , df_11s[df_11s$element_type=="DEF",]
                 , df_11s[df_11s$element_type=="MID",]
                 , df_11s[df_11s$element_type=="FWD",]
                 )
  df_11s = mutate(df_11s, squad="first")
  df_rsv = dfsummary[which(results==0),]
  df_rsv = arrange(df_rsv, desc(ObjPoints))
  df_rsv = rbind(df_rsv[df_rsv$element_type=="GKP",]
                 , df_rsv[df_rsv$element_type!="GKP",]
                 )
  df_rsv = mutate(df_rsv, squad="reserves")
  dfchosen = rbind(df_11s, df_rsv) %>% mutate(squad_no=seq(1,15,1))
}
form_formation = function(dfchosen, formation){
  dfchosen  = arrange(dfchosen, element_type, desc(ObjPoints))
  chosen_11s = rbind(head(dfchosen[dfchosen$element_type=="GKP",],formation[1]),
                     head(dfchosen[dfchosen$element_type=="DEF",],formation[2]),
                     head(dfchosen[dfchosen$element_type=="MID",],formation[3]),
                     head(dfchosen[dfchosen$element_type=="FWD",],formation[4]))
  chosen_11s = mutate(chosen_11s, squad="first")
  chosen_rsv = rbind(tail(dfchosen[dfchosen$element_type=="GKP",],2-formation[1]),
                     tail(dfchosen[dfchosen$element_type=="DEF",],5-formation[2]),
                     tail(dfchosen[dfchosen$element_type=="MID",],5-formation[3]),
                     tail(dfchosen[dfchosen$element_type=="FWD",],3-formation[4]))
  chosen_rsv = mutate(chosen_rsv, squad="reserves")
  dfchosen  = rbind(chosen_11s, chosen_rsv) %>% mutate(squad_no=seq(1,15,1))
}

## Finetuning player transfer function
finetune_chosen = function(dfcurrweek, dfchosen){
  ## Prep Input for 1-Player Transfer
  ## Load my current team selection, and merge with currentweek (week02)
  dfchosen = mutate(dfchosen, myteam=TRUE)
  dfcurrweek = merge(dfcurrweek, 
                     dfchosen[,c('id', 'myteam')], 
                     by.x=c('id'), by.y=c('id'), all.x=TRUE, sort=FALSE)
  ## Cleanup myteam, rescale ict_index and define ObjPoints
  dfcurrweek = mutate(dfcurrweek
                      , myteam    = ifelse(is.na(myteam), FALSE, TRUE)
                      , tophalf   = tophalf_pred_bin
                      , ict_mult  = as.numeric(ict_index) %>% rescale(to=c(1.00,1.20))
                      , ObjPoints = event_points_pred*ep_next
                      )
  
  ## Call function to get dataframe of 15-player team
  chosen_df = optimise_chosen(dfsummary=dfcurrweek, nsameteam=nsameteam, bankvalue=bankvalue, ntrade=ntrade)
  ## Arrange by element_type and then ObjPoints. Then split by first11s and reserves
  chosen_df = optimise_first11(chosen_df)
  # chosen_df = form_formation(chosen_df, formation)
}

## Print outcome function
print_finetune_outcome = function(dfchosen){
  ## Print out my finalized team & the points
  dfchosen %>% subset(select=c(web_name, element_type, status, myteam,
                               now_cost, ict_index, points_per_game, 
                               event_points.x, ep_next, ObjPoints, event_points.y)) %>% print
  with(dfchosen[1:11,], data.frame(ep_next, points_per_game, event_points.x, event_points.y, now_cost)) %>% 
    colSums %>% round(digits=1) %>% print
  with(dfchosen, data.frame(ep_next, points_per_game, event_points.x, event_points.y, now_cost)) %>% 
    colSums %>% round(digits=1) %>% print
}
print_finetune_outcome_realtime = function(dfchosen){
  ## Print out my finalized team & the points
  dfchosen %>% subset(select=c(web_name, element_type, status, myteam,
                               now_cost, ict_index, points_per_game, 
                               event_points, ep_next, ObjPoints)) %>% print
  with(dfchosen[1:11,], data.frame(ep_next, points_per_game, event_points, ObjPoints, now_cost)) %>% 
    colSums %>% round(digits=1) %>% print
  with(dfchosen, data.frame(ep_next, points_per_game, event_points, ObjPoints, now_cost)) %>% 
    colSums %>% round(digits=1) %>% print
}
print_finetune_summary = function(dfchosen){
  ## Print out my finalized team & the points
  with(dfchosen[1:11,], data.frame(ep_next, points_per_game, event_points.x, event_points.y, now_cost)) %>% 
    colSums %>% round(digits=1) %>% print
}


## RUN THE CODES ###################################################################################

## A) Finetune 1-player transfer testing past
## df_mergenext is coming from xgboost model currweek prediction.R
my_nextweek = finetune_chosen(dfcurrweek=df_mergenext, dfchosen=df_currteam)
print_finetune_outcome(my_nextweek)   ## Print out the results
print_finetune_summary(my_nextweek)   ## Print out the results

## B) Finetune 1-player transfer real time
# my_nextweek = finetune_chosen(dfcurrweek=df_merge, dfchosen=df_currteam)
# print_finetune_outcome_realtime(my_nextweek)
# write.csv(my_nextweek, file="./Output/faizmk_enhanced_team.csv", row.names=FALSE)