## ENHANCED version. updated 17 Aug 2018 20:51 MYT
## improved the selection by incorporating week1 points and now_cost, weighted with ICT

rm(list=ls())

library(readr)
library(jsonlite)
library(lpSolveAPI)
library(scales)
library(dplyr)

## Read data from pre-downloaded stats #############################################################
# This is ending stats for 2017/18 season, used as start. Credit to Panchajanya Banerjee (Pancham)
# https://www.kaggle.com/delayedkarma/fantasy-premier-league-20182019
player_df <- read_csv("Data/FPL_2018_19_Wk0.csv")

## Read data from FPL JSON file ####################################################################
# getting the latest player stats, some cleanup required.
player_df_wk1  = fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements
player_pos_wk1 = fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$element_type
team_name_wk1  = fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$teams
# replace player_details element_type from integer to shortname/abbr
player_df_wk1$element_type = player_pos_wk1$singular_name_short[match(player_df_wk1$element_type, player_pos_wk1$id)]
# replace club id with shortname/abbr
player_df_wk1$team = team_name_wk1$short_name[match(player_df_wk1$team, team_name_wk1$id)]

## Merge week1 key data into main df ###############################################################
# Merge now_cost into week0 player database
# Mutate ICT_mult, a multiplier/weightage for ICT. Subject to scale 1.00 to 1.15
player_df = merge(player_df, 
                  player_df_wk1[,c("web_name","status","now_cost","total_points","ict_index","element_type")],
                  by.x=c("Name","Position"),
                  by.y=c("web_name", "element_type"),
                  all.y=TRUE)
gameweek  = fromJSON("https://fantasy.premierleague.com/drf/element-summary/1")$fixtures_summary[1,3]
gameweek  = (gameweek %>% strsplit(" ") %>% unlist())[2] %>% as.numeric()
player_df = mutate(player_df, 
                   ICT_mult0 = rescale(ICT, to=c(1.00,1.15)),
                   ICT_mult1 = rescale(as.numeric(ict_index), to=c(1.00,1.15)),
                   ObjPoints = (38-gameweek)/38*Points*ICT_mult0 + gameweek/38*total_points*ICT_mult1)
player_df = player_df[player_df$status=="a",]   # subset to only non-injured player

## Initialise LP model #############################################################################
# Define decision variables as how to turn on/off (binary) 15 of the 470+ potential players
decVarNo = nrow(player_df)
lprec    = make.lp(0, decVarNo)
lp.control(lprec,sense="max")
set.type(lprec, seq(1,decVarNo), type = "binary")

## Set LP model objective function #################################################################
# Objective, by default, is to maximise player modified total points. This can be changed to ICT or 
# other performance/index past few games
decVar = player_df$ObjPoints
set.objfn(lprec, decVar)

## Set LP model constraints ########################################################################
# For Enhanced, formation is set for 3-4-3. Hence total team is GKP==2, DEF==4, MID==5 and FWD==4.
# Total of 15
add.constraint(lprec, as.numeric(player_df$Position=="GKP"), type="=", 2)
add.constraint(lprec, as.numeric(player_df$Position=="DEF"), type="=", 4)
add.constraint(lprec, as.numeric(player_df$Position=="MID"), type="=", 5)
add.constraint(lprec, as.numeric(player_df$Position=="FWD"), type="=", 4)
# no more than 2 from the same club. this is constrained further for diversification
for (i in unique(player_df$Team)) {
  add.constraint(lprec, as.numeric(player_df$Team==i), type="<=", 2)
}
# constraint for cost must be less than 1000 or so. we use current cost of week1.
add.constraint(lprec, player_df$now_cost, type="<=", rhs=1000)
# custom constraint as these players are suspects from first iteration, set to 0 to exclude them
add.constraint(lprec, as.numeric(player_df$Name=="Monreal"), type="=", 0)
add.constraint(lprec, as.numeric(player_df$Name=="Gross"), type="=", 0)

## Solve the LP model ##############################################################################
solve.lpExtPtr(lprec)
get.objective(lprec)    # printing out objective
get.variables(lprec)    # printing out decision variables
get.constraints(lprec)  # printing out constraints

## Prep the output data frame ######################################################################
chosen_ones <- which(get.variables(lprec)==1)   # index (from the 470+ players) of the chosen ones
chosen_df <- player_df[chosen_ones,]            # subset the main df to only the chosen ones
chosen_df = arrange(chosen_df, Position, desc(ObjPoints))
chosen_11s = rbind(head(chosen_df[chosen_df$Position=="GKP",],-1),
                   head(chosen_df[chosen_df$Position=="DEF",],-1),
                   head(chosen_df[chosen_df$Position=="MID",],-1),
                   head(chosen_df[chosen_df$Position=="FWD",],-1))
chosen_11s = mutate(chosen_11s, squad="first")
chosen_rsv = rbind(tail(chosen_df[chosen_df$Position=="GKP",],1),
                   tail(chosen_df[chosen_df$Position=="DEF",],1),
                   tail(chosen_df[chosen_df$Position=="MID",],1),
                   tail(chosen_df[chosen_df$Position=="FWD",],1))
chosen_rsv = mutate(chosen_rsv, squad="reserves")
chosen_df  = rbind(chosen_11s, chosen_rsv) %>% mutate(squad_no=seq(1,15,1))

rm(list=c("i", "team_name_wk1", "player_pos_wk1", "chosen_11s", "chosen_rsv"))
