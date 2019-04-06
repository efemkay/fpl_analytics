
rm(list=ls())
library(jsonlite)

## Read data from FPL JSON file ####################################################################
# getting the latest player stats, some cleanup required.
player_df = fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements
player_pos = fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$element_type
team_name = fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$teams
# replace player_details element_type from integer to shortname/abbr
player_df$element_type = player_pos$singular_name_short[match(player_df$element_type, player_pos$id)]
# replace club id with shortname/abbr
player_df$team = team_name$short_name[match(player_df$team, team_name$id)]

write.csv(player_df, file="./Data/weekly_01summary.csv", row.names=FALSE)

## NOTES ###########################################################################################
# player_details <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")
# a) phases, define 10 phases of a season
# b) elements, player stats similar to /drf/elements/
# c) stats, list of column headings and category
# d) game_settings, game and element_type
# e) current-events, single value, skip
# f) total-players, no of fpl active player/user
# g) teams, dataframe of clubs
# h) element_types, element/position abbr & full name
# i) last-entry-event, single value, skip
# j) stats_options, columns short & long name
# k) next_event_fixtures
# l) events
# m) next-event