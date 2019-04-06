## This script is meant to load player weekly stats

rm(list=ls())
library(jsonlite)

# "https://fantasy.premierleague.com/drf/element-summary/{player_id}" is player stats of 6 data frames
# a) history_past, b) fixtures_summary, c) explain, d) history_summary, e) fixtures, and f) history
# if we are to model player performance in the season, focus on item c, d & f.

# Download current player stats to get the player id. Expected 520+ obs 58+ columns
# Alternate source... fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements
player_details <- fromJSON("https://fantasy.premierleague.com/drf/elements")

# pull the first player to initialise the player_hist data frame
# for now, only choose the most recent completed season
player_weekly <- fromJSON("https://fantasy.premierleague.com/drf/element-summary/1")$history_summary
player_names = player_details[player_details$id==1,grep("name", names(player_details), value=TRUE)]
player_weekly <- cbind(player_id=1, player_names, player_weekly[nrow(player_weekly),])

for (i in player_details$id) {
  print(paste("extracting historical for player",i))
  json_url = paste0("https://fantasy.premierleague.com/drf/element-summary/",i)
  dftemp = fromJSON(json_url)$history_summary
  if(!is.null(dim(dftemp))){
    dftemp = dftemp[nrow(dftemp),]   # take the latest (bottom) obs
    player_names = player_details[player_details$id==i,grep("name", names(player_details), value=TRUE)]
    player_weekly[i,] = cbind(player_id=i, player_names, dftemp)
  }
}

## Save the historical data frame into a csv for easy reference later
write.csv(player_weekly, file="./Data/weekly_02historysumm.csv", row.names=FALSE)
