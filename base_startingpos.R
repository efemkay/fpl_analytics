
rm(list=ls())

library(readr)
library(lpSolveAPI)

## Read data from pre-downloaded stats #############################################################
# This is ending stats for 2017/18 season, used as start. Credit to Panchajanya Banerjee (Pancham)
# https://www.kaggle.com/delayedkarma/fantasy-premier-league-20182019
player_df <- read_csv("Data/FPL_2018_19_Wk0.csv")

## Creating the lp model with decision variable ####################################################
# Decision is how to turn on/off (binary) 15 of the 470+ potential players
decVarNo = nrow(player_df)
lprec <- make.lp(0, decVarNo)
lp.control(lprec,sense="max")
set.type(lprec, seq(1,decVarNo), type = "binary")

## Set the constraints #############################################################################
# as a starting pos, constraints for GKP==2, DEF==5, MID==5 and FWD==3. Total of 15
add.constraint(lprec, as.numeric(player_df$Position=="GKP"), type="=", 2)
add.constraint(lprec, as.numeric(player_df$Position=="DEF"), type="=", 5)
add.constraint(lprec, as.numeric(player_df$Position=="MID"), type="=", 5)
add.constraint(lprec, as.numeric(player_df$Position=="FWD"), type="=", 3)
# no more than 3 from the same club
for (i in unique(player_df$Team)) {
  add.constraint(lprec, as.numeric(player_df$Team==i), type="<=", 3)
}
# constraint for total player must be 15, redundant as each pos is fixed
add.constraint(lprec, rep(1,decVarNo), type="=", rhs=15)
# constraint for cost must be less than 1000 or so
add.constraint(lprec, player_df$Cost, type="<=", rhs=1000)
# custom constraint as these players are suspects from first iteration, set to 0 to exclude them
add.constraint(lprec, as.numeric(player_df$Name=="Monreal"), type="=", 0)
add.constraint(lprec, as.numeric(player_df$Name=="Gross"), type="=", 0)

## Setting up the objective function with the defined coefficient ##################################
# objective is by default set to maximise player total points. This can be changed to include
# ICT index or performance past few games
decVar = player_df$Points
# decVar = player_df$ICT
set.objfn(lprec, decVar)

## Solve it & show the results #####################################################################
solve.lpExtPtr(lprec)
get.objective(lprec)    # printing out objective
get.variables(lprec)    # printing out decision variables
get.constraints(lprec)  # printing out constraints

chosen_ones <- which(get.variables(lprec)==1)   # index (from the 470+ players) of the chosen ones
chosen_df <- player_df[chosen_ones,]            # subset the main df to only the chosen ones
