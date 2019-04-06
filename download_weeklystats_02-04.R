## This script is meant to load player weekly stats

rm(list=ls())
library(jsonlite)
library(dplyr)

# "https://fantasy.premierleague.com/drf/element-summary/{player_id}" is player stats of 6 data frames
# a) history_past, b) fixtures_summary, c) explain, d) history_summary, e) fixtures, and f) history
# if we are to model player performance in the season, focus on item c, d & f.

# Download current player stats to get the player id. Expected 520+ obs 58+ columns
# Alternate source... fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements
player_summary <- fromJSON("https://fantasy.premierleague.com/drf/elements")

# pull the first player to initialise the player_hist data frame
# for now, only choose the most recent completed season
player_names = player_summary[player_summary$id==1,grep("name", names(player_summary), value=TRUE)]

player_history  = fromJSON("https://fantasy.premierleague.com/drf/element-summary/1")$history
player_histsumm = fromJSON("https://fantasy.premierleague.com/drf/element-summary/1")$history_summary
player_fixture  = fromJSON("https://fantasy.premierleague.com/drf/element-summary/1")$fixtures
player_history  = cbind(player_id=1, player_names, player_history[nrow(player_history),])
player_histsumm = cbind(player_id=1, player_names, player_histsumm[nrow(player_histsumm),])  ## nrow for latest obs
player_fixture  = cbind(player_id=1, player_names, player_fixture[1,])  ## 1 for immediate

for (i in player_summary$id) {
  print(paste("extracting historical for player",i))
  json_url   = paste0("https://fantasy.premierleague.com/drf/element-summary/",i)
  dfhistory  = fromJSON(json_url)$history %>% tail(1)         # tail, for latest history (of the season)
  dfhistsumm = fromJSON(json_url)$history_summary %>% tail(1) # tail, for latest hist summary (of latest 3)
  dffixture  = fromJSON(json_url)$fixtures %>% head(1)        # head, for next immediate fixture (of next 3)
  
  player_names = player_summary[player_summary$id==i,grep("name", names(player_summary), value=TRUE)]
  
  if(!is.null(dim(dfhistory))){
    player_history[i,] = cbind(player_id=i, player_names, dfhistory)
  }
  if(!is.null(dim(dfhistsumm))){
    player_histsumm[i,] = cbind(player_id=i, player_names, dfhistsumm)
  }
  if(!is.null(dim(dffixture))){
    player_fixture[i,] = cbind(player_id=i, player_names, dffixture)
  }
}

## Save the historical data frame into a csv for easy reference later
# write.csv(player_histsumm, file="./Data/weekly_02historysumm.csv", row.names=FALSE)
