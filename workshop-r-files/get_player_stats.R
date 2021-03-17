library(tidyverse)
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
  # filter(game_id == "030220210302153522") %>%
  # select(game_id, time, round_no, player_team, player_id, player_hero, hero_time_played) %>%
  # filter(player_id == 'SPACE', round_no == 1)
  mutate(hero_time_played = parse_number(hero_time_played),
         player_id = tolower(player_id)) %>%
  filter( hero_time_played > 30) %>%
  group_by(game_id, player_team, player_id) %>%
  filter(hero_time_played == max(hero_time_played)) %>%
  arrange(desc(round_no)) %>%
  slice(1) %>%
  group_by(game_id, player_team) %>%
  slice(1:6) %>%
  ungroup() ->
  player_round_max
