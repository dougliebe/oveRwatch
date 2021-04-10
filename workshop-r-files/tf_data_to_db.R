## get output with a line for each player 
# in each teamfight and the resulting stats
source(here::here('workshop-r-files',"convert_log_data.R"))

head(data)
head(teamfight_index)
# add to game info
data %>% filter(event == "match_start") %>%
  separate(log, into = c('map', 'mode'),sep ="," ,extra = 'drop') %>%
  select(game_id, map, mode) %>%
  right_join(game_info) %>%
  arrange(game_id) ->
  GAME

## get kills
data %>%
  filter(event == "kill") %>%
  group_by(game_id) %>%
  mutate(kill_id = 1:n(),
         time = as.numeric(time)) %>%
  separate(log, into = c(
    'Attacker Team',
    'Attacker Name',
    'Attacker Hero',
    'Victim Team',
    'Victim Name',
    'Victim Hero',
    'Event Ability',
    'Event Damage',	
    'Is Critical Hit',
    'Is Environmental'
  ), sep = ",", extra = 'drop') %>%
  janitor::clean_names() %>%
  mutate(across(c(attacker_name, victim_name), tolower) ) ->
  KILLS

## expand the kill times to 6 for each kill, per team 
kill_times <-
  data %>%
  group_by(game_id) %>%
  filter(event == 'kill') %>%
  mutate(kill_id = 1:n()) %>%
  separate(log, into = c(
    'Attacker Team',
    'Attacker Name',
    'Attacker Hero',
    'Victim Team',
    'Victim Name',
    'Victim Hero',
    'Event Ability',
    'Event Damage',	
    'Is Critical Hit',
    'Is Environmental'
  ), sep = ",", extra = 'drop') %>%
  janitor::clean_names() %>%
  mutate(time = as.numeric(time)) %>%
  group_by(game_id) %>%
  expand(game_id,team = unique(attacker_team, victim_team), time, p_num = c(1:6))

# which players actually play the round?
source(here::here('workshop-r-files','get_player_stats.R'))
head(player_round_max)
player_round_max %>%
  select(game_id,player_team, player_id) %>%
  group_by(game_id, player_team) %>%
  mutate(p_num = 1:n()) ->
  players_filter
## we first need times of all hero switches
data %>%
  filter(event %in% c('hero_spawn', 'hero_swap')) %>%
  separate(log, into = c(
    "player_team",
    'player_name',
    'player_hero',
    'previous_hero',
    'hero_time_played'
  ), sep = ',',extra = 'drop') %>%
  mutate(time = as.numeric(time),
         player_name =tolower(player_name)) %>%
  inner_join(players_filter, by = c('game_id','player_name' = 'player_id', "player_team")) %>%
  arrange(game_id, time_s) %>%
  # filter(player_team == "Team 1", p_num == 4, game_id == "031020210310160547")
  select(time, game_id,player_name, p_num, player_hero, player_team) %>%
  group_by(time, game_id,player_team, p_num) %>%
  # remove multiple switches in same second
  slice(n()) %>%
  arrange(game_id,player_team, time) %>%
  # bind_rows(kill_times) %>%
  full_join(kill_times, by = c('game_id', 'time','player_team' = "team", "p_num")) %>%
  arrange(game_id, player_team,p_num, time) %>%
  group_by(game_id, player_team, p_num) %>%
  fill(player_hero,player_name, .direction = 'down') %>%
  group_by(game_id, time) %>%
  
  # fill(P1, P2, P3, P4, P5, P6, .direction = "down") %>%
  # pivot_longer(cols = c(P1,P2,P3,P4, P5, P6),
  #              values_to = 'hero') %>%
  # pivot_wider(id_cols = c(game_id, time),
  #             names_from = c(player_team, name),
  #             values_from = hero,
  # ) %>%
  # janitor::clean_names() %>%
  left_join(KILLS %>% select(game_id, time,attacker_name, attacker_team,victim_team,victim_name,victim_hero,kill_id ),
            by = c("time", 'game_id')) %>%
  filter(!is.na(kill_id)) ->
  kill_context

##  add indicators for TF #s
source(here::here('workshop-r-files','get_state_full_data.R'))
kill_context %>%
  ungroup() %>%
  left_join(state_by_events) %>%
  left_join(teamfight_index) %>%
  filter(!is.na(tf_no)) ->
  tf_kill_context

tf_kill_context %>%
  group_by(game_id, tf_no, player_team) %>%
  mutate(game_id = as.character(game_id),
    player_hero = recode(player_hero, "Lúcio"="Lucio", "Torbjörn" = "Torbjorn"),
         player_kill = (player_name == attacker_name)*1,
         player_death = (player_name == victim_name)*1,
         tf_win = ((sum(player_team != victim_team)/6)/mean(kills) > 0.5)*1) %>% 
  select(game_id, tf_no, kill_id, player_name, player_hero, player_kill, player_death, adv, state, tf_length, tf_win) ->
  tf_stats

# tf_stats %>% write_csv('test_tf_stats.csv')  
library(odbc)
library(DBI)
library(tidyverse)

con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "overwatch",
                      username    = 'admin',
                      password    = "guerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)

## check games in db
tbl(con, "TF_STATS") %>%
  group_by(game_id) %>%
  count() %>% pull(game_id) ->
  games_already_in

# append on new rows
DBI::dbWriteTable(con,"TF_STATS", tf_stats %>%
                    filter(!game_id %in% games_already_in) %>%
                    mutate(player_hero = recode(player_hero, "Lúcio" = "Lucio",
                                                "Torbjörn" = "Torbjorn")),
                  overwrite = F, append = T, row.names = F)



# pivot_wider(id_cols = c(game_id, tf_no, tf_win, player_name, player_hero, tf_length),
  #             names_from = c(state, player_event), values_from = player_event, values_fn = length, values_fill = 0) %>%
  janitor::clean_names()
  group_by(game_id, tf_no, player_name, player_hero, tf_length) %>%
  summarise()
  group_by(game_id, tf_no, player_team, kills, tf_length) %>%
  summarise(comp = paste0(sort(unique(player_hero)), collapse = "-"),
            win = (sum(player_team != victim_team)/6)/mean(kills) > 0.5) %>%
  group_by(game_id, tf_no, kills, tf_length) %>%
  summarise(comps = paste0(comp, collapse = "//"),
            win_team = win[1]) ->
  comps_tf_context