## trying to get a picture of tfs based on heros in comps, 
# not players specifically
# should probably break down by map and side too
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

## we first need times of all hero switches
kill_times <-
  data %>%
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
  mutate(time = as.numeric(time)) %>%
  group_by(game_id) %>%
  expand(game_id,attacker_team, time, p_num = c(1:6))

# which players actually play the round?
source(here::here('workshop-r-files','get_player_stats.R'))
head(player_round_max)
player_round_max %>%
  select(game_id,player_team, player_id) %>%
  group_by(game_id, player_team) %>%
  mutate(p_num = 1:n()) ->
  players_filter

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
  select(time, game_id, p_num, player_hero, player_team) %>%
  group_by(time, game_id,player_team, p_num) %>%
  slice(n()) %>%
  # pivot_wider(
  #   id_cols = c(game_id, time, player_team),
  #   names_from = c("p_num"),
  #   names_glue = "P{p_num}",
  #   values_from = 'player_hero'
  # ) %>%
  # ungroup() %>%
  arrange(game_id,player_team, time) %>%
  full_join(kill_times, by = c('game_id', 'time','player_team' = "attacker_team", "p_num")) %>%
  arrange(game_id, player_team,p_num, time) %>%
  group_by(game_id, player_team, p_num) %>%
  fill(player_hero, .direction = 'down') %>%
  group_by(game_id, time) %>%
  
  # fill(P1, P2, P3, P4, P5, P6, .direction = "down") %>%
  # pivot_longer(cols = c(P1,P2,P3,P4, P5, P6),
  #              values_to = 'hero') %>%
  # pivot_wider(id_cols = c(game_id, time),
  #             names_from = c(player_team, name),
  #             values_from = hero,
  # ) %>%
  # janitor::clean_names() %>%
  left_join(KILLS %>% select(game_id, time,victim_team,victim_hero,kill_id ), by = c("time", 'game_id')) %>%
  filter(!is.na(kill_id)) ->
  kill_context

##  add indicators for TF #s
source(here::here('workshop-r-files','get_state_full_data.R'))
kill_context %>%
  left_join(teamfight_index) %>%
  # left_join(state_by_events) %>%
  filter(!is.na(tf_no)) ->
  tf_kill_context

tf_kill_context %>%
  mutate(player_hero = recode(player_hero, "Lúcio"="Lucio", "Torbjörn" = "Torbjorn")) %>%
  group_by(game_id, tf_no, player_team, kills, tf_length) %>%
  summarise(comp = paste0(sort(unique(player_hero)), collapse = "-"),
            win = (sum(player_team != victim_team)/6)/mean(kills) > 0.5) %>%
  group_by(game_id, tf_no, kills, tf_length) %>%
  summarise(comps = paste0(comp, collapse = "//"),
            win_team = win[1]) ->
  comps_tf_context

tf_kill_context %>%
  group_by(game_id, tf_no, player_team, kills) %>%
  mutate(kills_team = sum(player_team != victim_team)/6) %>%
  mutate(team_win = kills_team > kills/2) %>%
  group_by(game_id, tf_no,player_team, team_win) %>%
  filter(victim_team == player_team) %>%
  summarise(first_death = victim_hero[1]) %>%
  filter(!team_win) %>%
  ungroup() %>%
  select(game_id, tf_no, first_death) ->
  first_dead

# comp v comp win%
comps_tf_context %>%
  left_join(first_dead) %>%
  filter(kills < 8) %>%
  group_by(game_id, comps, first_death) %>%
  summarise(wins = sum(win_team))

## look at comp v comp win% total
comps_tf_context %>%
  left_join(first_dead) %>%
  filter(kills < 8) %>%
  group_by(game_id, comps, first_death) %>%
  summarise(wins = sum(win_team),
            fights = n()) %>%
  left_join(GAME) %>%
  group_by(map, mode, comps, first_death) %>%
  summarise(wins = sum(wins),
            fights = sum(fights)) %>%
  separate(comps, into = c('team1', 'team2'), sep = "//") ->
  teamfight_heros
  
teamfight_heros %>%
  bind_rows(teamfight_heros %>%
              rename(team2 = team1, team1 = team2) %>%
              mutate(wins = fights - wins)) %>%
  write_csv("teamfight_heros.csv")
