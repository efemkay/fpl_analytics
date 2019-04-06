## This script is meant to load player historical stats

rm(list=ls())
library(jsonlite)

# "https://fantasy.premierleague.com/drf/element-summary/{player_id}" is player stats of 6 data frames
# a) history_past, b) fixtures_summary, c) explain, d) history_summary, e) fixtures, and f) history
# if we are to model player performance in the season, focus on item c, d & f.

# Download current player stats to get the player id. Expected 520+ obs 58+ columns
# Alternate source... fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements

player_details = fromJSON("https://fantasy.premierleague.com/drf/elements")

# pull the first player to initialise the player_hist data frame
# for now, only choose the most recent completed season
player_hist  = fromJSON("https://fantasy.premierleague.com/drf/element-summary/1")$history_past
player_names = player_details[player_details$id==1,grep("name", names(player_details), value=TRUE)]
player_hist  = cbind(player_id=1, player_names, player_hist)
player_hist  = player_hist[0,]   # empty out the data frame

for (i in player_details$id) {
  print(paste("extracting historical for player",i))
  json_url = paste0("https://fantasy.premierleague.com/drf/element-summary/",i)
  dftemp   = fromJSON(json_url)$history_past  # take data from all past seasons
  if(!is.null(dim(dftemp))){
    player_names = player_details[player_details$id==i,grep("name", names(player_details), value=TRUE)]
    player_hist  = rbind(player_hist,
                         cbind(player_id=i, player_names, dftemp),
                         make.row.names=FALSE)
  }
}

## Save the historical data frame into a csv for easy reference later
write.csv(player_hist, file="./Data/player_history_allseasons.csv", row.names=FALSE)
