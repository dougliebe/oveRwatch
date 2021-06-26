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

data %>%
  filter(event %in% c("hero_swap", 'hero_spawn')) %>% 
  separate(log, into = c(
    "Player Team","Player Name",	"Player Hero",
    "Previous Hero",	"Hero Time Played"
  ),
  sep = ",") %>% 
  bind_rows(
    data %>%
      filter(event %in% c("round_start", 'round_end', 'setup_complete')) %>%
      separate(log, into = c("round_no"),
               sep  = ",",
               extra = 'drop'
      )
  ) %>%
  janitor::clean_names() %>% 
  mutate(time = as.numeric(time),
         player_name = tolower(player_name),
         hero_time_played = parse_number(hero_time_played)) %>% 
  group_by(game_id) %>%
  arrange(time) %>%
  # filter(game_id == "062120210621100359") %>%
  # slice(1:100) %>% view
  mutate(round_end_time = max(time_s*(event == 'round_end'))) %>% 
  fill(round_no, round_end_time, .direction = "up") %>%
  filter(event %in% c("hero_swap", 'hero_spawn')) %>%
  group_by(game_id, player_name) %>%
  mutate(hero_time_played = lead(time_s, default = max(round_end_time)) - time_s) %>%
  # slice(1:100) %>% view
  filter(hero_time_played > 0) %>% 
  group_by(game_id,player_team, player_name, player_hero) %>%
  summarise(hero_time_played = sum(hero_time_played)) ->
  hero_times


data %>%
  filter(event == "player_stat") %>%
  mutate(time = as.numeric(time)) %>% 
  # filter(game_id == "062320210623145242") %>% view
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
    'Weapon Accuracy'
  ), sep = ',', extra = 'drop') %>% 
  janitor::clean_names() %>%
  # filter(game_id == "060120210601160643") %>%
  # select(game_id, time, round_no, player_team, player_id, player_hero, hero_time_played) %>%
  # filter(player_id == 'SPACE', round_no == 1)
  mutate(player_id = tolower(player_id)) %>%
  group_by(game_id, player_team, player_id, player_hero) %>%
  arrange((round_no)) %>% 
  # mutate(hero_time_played = hero_time_played - lag(hero_time_played, default = 0),
  #        hero_total_time = sum(hero_time_played)) %>% view
    # filter(hero_time_played != 0) %>% 
  # arrange(desc(round_no)) %>%
  slice(n()) %>% 
  left_join(hero_times, c('game_id', 'player_id' = 'player_name', 
                          'player_hero', 'player_team')) %>% 
  filter( hero_time_played > 30) %>% 
    ungroup() ->
  STATS

STATS %>% 
  filter(player_id == "hou001") %>% view
  summarise(dmg = sum(parse_number(hero_damage_dealt)),
            time = sum(hero_time_played))

# append on new rows
DBI::dbWriteTable(con,"STATS", STATS %>%
                    # filter(!game_id %in% games_already_in) %>%
                    mutate(player_hero = recode(player_hero,
                                                "Lúcio" = "Lucio",
                                                "Torbjörn" = "Torbjorn")),
                  overwrite = F, append = T, row.names = F)

DBI::dbWriteTable(con,"TEAM_GAME", team_game,
                  overwrite = F, append = T, row.names = F)
