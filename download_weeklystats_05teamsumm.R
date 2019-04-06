
rm(list=ls())
library(jsonlite)
library(tidyr)    # for unnest

## Read data from FPL JSON file ####################################################################
# getting the latest player stats, some cleanup required.
team_name = fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$teams

# team_name_curr      = unnest(team_name, current_event_fixture)
# team_name_next      = unnest(team_name, next_event_fixture)
# merge(team_name_curr, team_name_next)
team_name           = unnest(team_name)
team_name$opponent  = team_name$short_name[match(team_name$opponent, team_name$id)]
team_name$opponent1 = team_name$short_name[match(team_name$opponent1, team_name$id)]

write.csv(team_name, file="./Data/weekly_05teamsummary.csv", row.names=FALSE)

