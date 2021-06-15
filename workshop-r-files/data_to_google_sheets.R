

source(here::here('workshop-r-files',"convert_log_data.R"))
library(odbc)
library(DBI)
library(tidyverse)
library(dbplyr)
library(googlesheets4)
# 
# con <- DBI::dbConnect(drv = RMySQL::MySQL(),
#                       dbname = "overwatch",
#                       username    = 'admin',
#                       password    = "guerrillas",
#                       host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
#                       port = 3306)

# aliases_q <- tbl(con, "ALIASES")
aliases_q <- read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",sheet = 'aliases',col_types = 'cccc') %>%
  mutate(player_alias = tolower(player_alias),
         player_name = tolower(player_name))


# add to game info
data %>% filter(event == "match_start") %>%
  separate(log, into = c('map', 'mode'),sep ="," ,extra = 'drop') %>%
  select(game_id, map, mode) %>%
  right_join(game_info) %>%
  arrange(game_id) ->
  GAME

# edit team games for any games that switched sides
team_game %>%
  group_by(game_id) %>%
  mutate(team_name = case_when(
    game_id %in% c("030820210308223612",
                   '040720210407132737',
                   '040720210407134526',
                   '040720210407141258',
                   '040720210407142937',
                   "052220210522143602",
                   "052220210522151045") & team_num == 1 ~ lead(team_name),
    game_id  %in% c("030820210308223612",
                    '040720210407132737',
                    '040720210407134526',
                    '040720210407141258',
                    '040720210407142937',
                    "052220210522143602",
                    "052220210522151045") ~ lag(team_name),
    TRUE ~ team_name
  )) ->
  team_game

# data %>% filter(event == "match_end") %>%
#   separate(log, into = c('round', 'team_1_score', 'team_2_score'),
#            sep ="," ,
#            extra = 'drop') %>%
#   pivot_longer(c(team_1_score, team_2_score),
#                names_to= 'team_in_game',
#                values_to = "score") %>%
#   mutate(score = as.integer(score)) %>%
#   group_by(game_id, round) %>%
#   mutate(team_num = as.character(1:n()), 
#          result = case_when(
#     score > sum(score)-score ~ "win",
#     score == sum(score)-score ~ "draw",
#     TRUE ~ "loss"
#   )) %>%
#   right_join(team_game) ->
#   results

## get actual wins based on max score in rounds

data %>%
  # filter(game_id == "20210219174106") %>%
  filter(event == "round_start") %>%
  separate(log, into = c("round_no",'attacker_team',
                         "team_1_score", "team_2_score",
                         'objective_index'),
           sep  = ",",
           extra = 'drop'
  ) %>%
  bind_rows(data %>%
              filter(event == "round_end") %>%
              # filter(game_id == "20210219174106") %>%
              separate(log, into = c("round_no",'attacker_team',
                                     "team_1_score", "team_2_score",
                                     'objective_index',
                                     "team_1_progress", "team_2_progress",
                                     'match_time_remaining'),
                       sep  = ",",
                       extra = 'drop'
              )) %>%
  arrange(game_id, round_no) %>%
  mutate(across(c(team_1_score, team_2_score, round_no, team_1_progress, team_2_progress),
                parse_number)) %>%
  replace_na(list(team_1_progress = 0, team_2_progress = 0)) %>%
  pivot_longer(c('team_1_score', 'team_2_score'),
               names_to = 'team_num',
               values_to  = 'score') %>%
  mutate(team_num = ifelse(team_num == "team_1_score", '1', '2')) %>%
  left_join(team_game) %>%
  group_by(game_id , team_name) %>%
  summarise(
    score = max(score*(attacker_team == team_in_game_name | attacker_team == 0))
  ) %>%
  group_by(game_id) %>%
  mutate(total_score = sum(score),
         opponent = ifelse(1:n() == 1, lead(team_name), lag(team_name)),
         result = case_when(
           score > (sum(score)- score) ~ 1,
           score == (sum(score) - score) ~ NA_real_,
           score < (sum(score)- score) ~ 0
         ),
         ) %>%
  filter(total_score > 0, team_name == "gladiators") %>%
  select(-total_score) ->
  game_results

## Now add GAME and results together to mimic sam's table
GAME %>%
  inner_join(game_results) %>%
  mutate(patch = "",
         region = "",
         week = as.numeric(ceiling(difftime(date, "2021-02-14", units = 'weeks')))) %>%
  select(game_id, date, week, map, type = mode, result, opponent,team_name, patch, region) ->
  game_result_combined

## For control games only, add scores by point


data %>%
  filter(event == "round_start",
         game_id %in% (GAME %>%
           filter(mode == "Control") %>%
           pull(game_id))) %>%
  separate(log, into = c("round_no",'attacker_team',
                         "team_1_score", "team_2_score",
                         'objective_index'),
           sep  = ",",
           extra = 'drop'
  ) %>%
  bind_rows(data %>%
              filter(event == "round_end",
                     game_id %in% (GAME %>%
                                     filter(mode == "Control") %>%
                                     pull(game_id))) %>%
              separate(log, into = c("round_no",'attacker_team',
                                     "team_1_score", "team_2_score",
                                     'objective_index',
                                     "team_1_progress", "team_2_progress"),
                       sep  = ",",
                       extra = 'drop'
              )) %>%
  arrange(game_id, round_no) %>%
  mutate(across(c(team_1_score, team_2_score, round_no, team_1_progress, team_2_progress),
                parse_number)) %>%
  replace_na(list(team_1_progress = 0, team_2_progress = 0)) %>%
  group_by(game_id, round_no, objective_index) %>%
  summarise(duration_s = max(time_s)-min(time_s),
            objective_index = objective_index[1],
            team_1_gain = max(team_1_score) - min(team_1_score),
            team_2_gain = max(team_2_score) - min(team_2_score),
            # team_1_progress = max(team_1_progress),
            # team_2_progress = max(team_2_progress)
  ) %>%
  pivot_longer(c('team_1_gain', 'team_2_gain'),
             names_to = 'team_num',
             values_to  = 'score') %>%
  mutate(team_num = ifelse(team_num == "team_1_gain", '1', '2')) %>%
  left_join(team_game) %>%
  group_by(game_id, objective_index) %>%
  select(game_id, objective_index, team_name, score) %>%
  filter(team_name == "gladiators") %>%
  arrange(game_id, objective_index) ->
  control_scores

game_result_combined %>%
  left_join(control_scores, by = c('game_id', 'team_name')) %>%
  replace_na(list(objective_index = 0)) %>%
  pivot_wider(names_from = c(objective_index),
              values_from = score,
              values_fn = sum,
              names_glue = "point_{objective_index}") ->
  sams_output

data %>%
  # count(event)
  filter(event == "player_stat") %>%
  mutate(time = as.numeric(time)) %>%
  ## hide this
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
  select(game_id, time, round_no, player_team, player_id, player_hero, hero_time_played) %>%
  # filter(player_id == 'SPACE', round_no == 1)
  mutate(hero_time_played = parse_number(hero_time_played),
         player_id = tolower(player_id)) %>%
  group_by(game_id, player_team, player_id, player_hero) %>%
  mutate(hero_time_played = hero_time_played - lag(hero_time_played, default = 0)) %>%
  group_by(game_id, player_team, player_id) %>%
  summarise(hero_time_played = max(hero_time_played)) %>%
  filter(hero_time_played > 50) ->
  player_hero_time_map

## add new names to alias table
# gs4_auth()
player_hero_time_map %>%
  anti_join(aliases_q, by = c('player_id'='player_name')) %>%
  ungroup() %>%
  select(player_id) %>%
  distinct() %>%
  sheet_append(ss = "1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",sheet = 'aliases')

player_hero_time_map %>%
  left_join(aliases_q, by = c('player_id' = 'player_name')) %>%
  mutate(player_name = coalesce(player_alias, player_id)) %>%
  select(game_id, team_in_game_name = player_team, player_name) %>%
  left_join(team_game) %>%
  group_by(game_id, team_name) %>%
  count(player_name) %>%
  select(-n) %>%
  filter(!is.na(player_name)) %>%
  arrange(player_name) %>%
  mutate(player_num = 1:n()) %>%
  filter(max(player_num) == 6) %>%
  pivot_wider(names_from = "player_num",
              values_from = 'player_name',
              names_glue = "p_{player_num}") ->
  team_comps
  
sams_output %>%
  left_join(team_comps) %>% 
  view

## now write to sams copy
library(googlesheets4)
sams_output %>%
  left_join(team_comps) %>%
  sheet_append(ss = "1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",
              sheet = "GameScores")
# googlesheets4::sheet_append(ss = "1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",
#                             sheet = "teamscores")




## teamfight stats by round

koth_maps <- read_csv(here::here("koth_maps.csv"))

## need round times first
data %>%
  filter(event == "round_end") %>%
  separate(log, into = c("round_no",'attacker_team',
                         "team_1_score", "team_2_score",
                         'objective_index',
                         "team_1_progress", "team_2_progress"),
           sep  = ",",
           extra = 'drop'
  ) %>%
  mutate(time = as.numeric(time)) ->
  round_ends

## add heros to each round and team
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
  select(game_id, time, round_no, player_team, player_id, player_hero, hero_time_played) %>%
  # filter(player_id == 'SPACE', round_no == 1)
  mutate(hero_time_played = parse_number(hero_time_played),
         player_id = tolower(player_id)) %>%
  group_by(game_id, time, round_no, player_team, player_id) %>%
  filter(hero_time_played == max(hero_time_played)) %>%
  left_join(aliases_q %>% collect(), by = c('player_id' = 'player_name')) %>%
  mutate(player_name = coalesce(player_alias, player_id)) %>%
  group_by(game_id, round_no, team_in_game_name = player_team) %>%
  mutate(player_hero = recode(player_hero, "LC:cio"="Lucio", "TorbjC6rn" = "Torbjorn")) %>%
  arrange(player_id) %>%
  summarise(players = paste0(sort(unique(player_name)), collapse = "-"),
            heros = paste0(sort(unique(player_hero)), collapse = "-")) ->
  team_round_comps
  # ->
  # team_list

teamfight_index %>%
  group_by(game_id) %>%
  bind_rows(round_ends %>%
              select(game_id, time, round_no,
                     attacking_team = attacker_team,
                     objective_index)) %>%
  arrange(game_id, time) %>%
  # fill in rounds by time
  fill(round_no, attacking_team, objective_index,.direction = 'up') %>%
  # filter out the rows you just bound in
  filter(!is.na(kill)) %>%
  group_by(game_id, round_no, tf_no) %>%
  mutate(tf_no = cur_group_id()) %>%
  mutate(tf_no = ifelse(tf_length < 16 | kills < 3, -10, tf_no))  %>%
  inner_join(data %>%
              filter(event == 'kill') %>%
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
              mutate(time = as.numeric(time)),
            by = c("game_id", 'time')) %>%
  # filter(game_id == "20210222174827") %>%
  group_by(game_id, round_no, objective_index, attacking_team) %>%
  mutate(tfs = length(unique(tf_no)[unique(tf_no) > 0]),
         is_tf = tf_no > 0) %>%
  group_by(is_tf, tf_no, .add = T) %>%
  mutate(kill_no = cumsum(kill)) %>%
  group_by(team_in_game_name = attacker_team,tfs, .add = T) %>%
  summarise(kills = n(),
            first_kill = any(kill_no == 1)) %>%
  group_by(game_id,round_no, objective_index, attacking_team,is_tf, tf_no) %>%
  mutate(win = (kills > (sum(kills)-kills)) & is_tf,
         draw = (kills == (sum(kills)-kills)) & is_tf,
  ) %>%
  group_by(game_id, round_no, objective_index, attacking_team, team_in_game_name) %>%
  summarise(wins = sum(win),
            draws = sum(draw),
            losses = tfs[1] - draws - wins,
            extra_kills = sum(kills*!is_tf)) %>%
  group_by(game_id, round_no, objective_index) %>%
  mutate(extra_deaths = sum(extra_kills)-extra_kills) %>%
  left_join(team_game %>% select(-team_num), by = c("game_id", 'team_in_game_name')) %>%
  left_join(team_round_comps) %>%
  mutate(attacking = team_in_game_name == attacking_team,
         objective_index = as.numeric(objective_index)) %>%
  select(-attacking_team, -team_in_game_name) %>%
  left_join(GAME) %>%
  left_join(koth_maps, by = c('map', 'objective_index')) ->
  tf_output

library(googlesheets4)

# output %>%
#   anti_join(
#     read_sheet(ss = "15EZ6F5GGcPJrHITTWoiyCCSn67064wJDDPTsO3UhvX4",
#                sheet = "teamscores"),
#     by = 'game_id'
#   ) ->
#   new_output

tf_output %>%
  anti_join(read_sheet(ss = "15EZ6F5GGcPJrHITTWoiyCCSn67064wJDDPTsO3UhvX4",
           sheet = "teamfightscores"),
   by = c('game_id', 'round_no')) ->
  new_tf_output

# output %>%
#   googlesheets4::sheet_append(ss = "15EZ6F5GGcPJrHITTWoiyCCSn67064wJDDPTsO3UhvX4",
#               sheet = "teamscores")
new_tf_output %>%
  googlesheets4::sheet_append(ss = "15EZ6F5GGcPJrHITTWoiyCCSn67064wJDDPTsO3UhvX4",
                              sheet = "teamfightscores")

# tf_output %>%
#   write_sheet(ss = "15EZ6F5GGcPJrHITTWoiyCCSn67064wJDDPTsO3UhvX4",
#               sheet = "teamfightscores")
# 

### read aliases and change names in gamescore table










