
library(tidyverse)
library(dbplyr)
library(odbc)
library(DBI)
library(googlesheets4)

con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "overwatch",
                      username    = 'admin',
                      password    = "laguerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)

stats_q <- tbl(con, "STATS")
tf_stats_q <- tbl(con, "TF_STATS")
team_game_q <- tbl(con, "TEAM_GAME")
aliases_q <- read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",
                        sheet = 'aliases',
                        col_types = 'cccc') %>%
  mutate(player_alias = tolower(player_alias),
         player_name = tolower(player_name))
games_q <-  read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",
                       sheet = 'Fixed GameScores') %>% filter(!is.na(game_id))

patch_list <- unique(games_q$patch) 
region_list <- unique(games_q$region)
opponent_list <- unique(games_q$opponent) 
map_list <- unique(games_q$map)
winpct <- tibble(adv = seq(-6, 6),
                 wpa = c(0,0,0,6.2,
                         15.3-6.2, 44.1-15.3,
                         50 - 44.1, 
                         79.2-50, 92.1-79.2,
                         95.4-92.1,97.5-95.4,
                         0.6,0))


## first look at hero and player lineups most used by time
map_filter <-  "Busan"

stats_q %>% 
  left_join(team_game_q, by = c('game_id', 'player_team'='team_in_game_name')) %>%
  collect() %>%
  left_join(games_q %>%
              mutate(result = result == 1) %>% select(game_id, date, map, type, result), by = 'game_id') %>%
  filter(map == !!map_filter) %>%
  left_join(aliases_q, by = c('player_id' = 'player_name_lower')) %>%
  mutate(player_name = coalesce('player_alias_lower', 'player_id'))  ->
  data

## look at team comps
data %>%
  mutate(result = ifelse(team_name == "gladiators", result, !result)) %>%
  # filter(date >= "2021-03-20") %>%
  group_by(game_id, team_name, result,map,  date) %>%
  arrange(desc(hero_time_played)) %>%
  slice(1:6) %>%
  summarise(heroes = paste0(player_hero[order(player_hero)], collapse = "-")) %>%
  group_by(game_id) %>%
  mutate(opp_comp = ifelse(1:n() == 1, lead(heroes), lag(heroes)),
         opp = ifelse(1:n() == 1, lead(team_name), lag(team_name))) %>%
  # filter(heroes == "Ana-Brigitte-Echo-Tracer-Winston-Zarya") %>%
  # filter(grepl(".*(Mei).*(Reinhardt).*", opp_comp)) %>%
  # filter(grepl("Baptiste-D.Va-Lucio-McCree-Mei-Reinhardt", opp_comp)) %>%
  group_by(heroes) %>%
  summarise(n = n(),
            win_pct = mean(result, na.rm = T)) %>%
  # count(heroes) %>%
  arrange(desc(n))

# all opposing comps
data %>%
  mutate(result = ifelse(team_name == "gladiators", result == 1, result != 1)) %>%
  filter(date >= "2021-03-20", team_name != 'gladiators') %>%
  group_by(game_id, team_name, result, date) %>%
  arrange(desc(hero_time_played)) %>%
  slice(1:6) %>%
  summarise(heroes = paste0(player_hero[order(player_hero)], collapse = "-")) %>%
  group_by(team_name, heroes) %>%
  summarise(n = n(),
            win_pct = mean(result, na.rm = T)) %>%
  # count(heroes) %>%
  # filter(heroes == "Baptiste-D.Va-Lucio-McCree-Mei-Reinhardt") %>%
  arrange(desc(n))


data %>%
  mutate(result = ifelse(team_name == "gladiators", result == 1, result != 1)) %>%
  group_by(game_id, team_name, result, date) %>%
  arrange(desc(hero_time_played)) %>%
  slice(1:6) %>%
  summarise(heroes = paste0(player_hero[order(player_hero)], collapse = "-")) %>%
  filter(team_name == "gladiators") %>%
  arrange(desc(date))
