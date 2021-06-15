source(here::here('workshop-r-files',"convert_log_data.R"))
library(odbc)
library(DBI)
library(tidyverse)
library(dbplyr)
con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "overwatch",
                      username    = 'admin',
                      password    = "laguerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)
# ## check games in db
# tbl(con, "STATS") %>%
#   group_by(game_id) %>%
#   count() %>% pull(game_id) ->
#   games_already_in

# append on new rows
DBI::dbWriteTable(con,"STATS", STATS %>%
                    # filter(!game_id %in% games_already_in) %>%
                    mutate(player_hero = recode(player_hero,
                                                "Lúcio" = "Lucio",
                                                "Torbjörn" = "Torbjorn")),
                  overwrite = F, append = T, row.names = F)

DBI::dbWriteTable(con,"TEAM_GAME", team_game,
                  overwrite = F, append = T, row.names = F)


data %>%
  filter(event == "player_stat") %>%
  mutate(time = as.numeric(time)) %>%
  separate(log, into = c(
    'Round_no',
    'Player Team',
    'Player ID',
    'Player Hero',
    'Eliminations',
    'Final Blows',
    'Deaths',
    'All Damage Dealt',
    'Barrier Damage Dealt',
    'Hero Damage Dealt',
    'Healing Dealt',
    'Healing Received',
    'Self Healing',
    'Damage Taken',
    'Damage Blocked',
    'Defensive Assists',
    'Offensive Assists',
    'Ultimates Earned',
    'Ultimates Used',
    'Multikill Best',
    'Multikills',	
    'Solo Kills',
    'Objective Kills',
    'Environmental Kills',
    'Environmental Deaths',
    'Critical Hits',
    'Critical Hit Accuracy',
    'Scoped Accuracy',
    'Scoped Critical Hit Accuracy',
    'Scoped Critical Hit Kills',
    'Shots Fired',
    'Shots Hit',
    'Shots Missed',	
    'Scoped Shots Fired',
    'Scoped Shots Hit',
    'Weapon Accuracy',
    'Hero Time Played'
  ), sep = ',', extra = 'drop') %>% 
  janitor::clean_names() %>%
  # filter(game_id == "060120210601160643") %>%
  # select(game_id, time, round_no, player_team, player_id, player_hero, hero_time_played) %>%
  # filter(player_id == 'SPACE', round_no == 1)
  mutate(hero_time_played = parse_number(hero_time_played),
         player_id = tolower(player_id)) %>%
  filter( hero_time_played > 30) %>%
  group_by(game_id, player_team, player_id, player_hero) %>%
  mutate(hero_time_played = hero_time_played - lag(hero_time_played, default = 0)) %>%
  # arrange(desc(round_no)) %>%
  slice(n()) %>%
  ungroup() ->
  STATS
