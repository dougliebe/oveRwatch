

source(here::here('workshop-r-files',"convert_log_data.R"))


team_game
# add to game info
data %>% filter(event == "match_start") %>%
  separate(log, into = c('map', 'mode'),sep ="," ,extra = 'drop') %>%
  select(game_id, map, mode) %>%
  right_join(game_info) ->
  GAMES


data %>%
  filter(event == "round_start") %>%
  separate(log, into = c("round_no",'attacker_team', "team_1_score", "team_2_score"),
           sep  = ",",
           extra = 'drop'
           ) %>%
  bind_rows(data %>%
              filter(event == "round_end") %>%
              separate(log, into = c("round_no",'attacker_team',
                                     "team_1_score", "team_2_score",
                                     "team_1_progress", "team_2_progress"),
                       sep  = ",",
                       extra = 'drop'
              )) %>%
  arrange(game_id, round_no) %>%
  mutate(across(c(team_1_score, team_2_score, round_no, team_1_progress, team_2_progress),
                as.numeric)) %>%
  replace_na(list(team_1_progress = 0, team_1_progress = 0)) %>%
  left_join(team_game, by = c('game_id', 'attacker_team'='team_in_game_name')) %>%
  group_by(game_id, round_no, attacker_team, team_name) %>%
  summarise(duration_s = max(time_s)-min(time_s),
            team_1_gain = max(team_1_score) - min(team_1_score),
            team_2_gain = max(team_2_score) - min(team_2_score)
  ) ->
  ROUNDS

## boxscore stats for each round?
data %>%
  filter(event == "player_stat") %>%
  group_by(game_id) %>%
  mutate(stat_no = 1:n()) %>%
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
  mutate(across(c(player_id), tolower))->
  STATS

data %>%
  filter(event %in% c(
    "kill",
    "offensive_assist",
    "defensive_assist",
    "ultimate_start",
    "ultimate_end"
  )) %>%
  separate(log, into = c('team', "player_id", "player_hero", 'log'),
           sep = ",", extra = 'merge', fill = 'right') %>%
  mutate(across(c(player_id), tolower)) ->
  kills_only

data %>%
  filter(event == "kill") %>%
  group_by(game_id) %>%
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
  mutate(across(c(attacker_name, victim_name), tolower) ) ->
  KILLS

DAMAGE <-
  data %>%
  filter(event == "damage") %>%
  separate(log, into = c(
    "Attacker Team",
    "Attacker Name",
    "Attacker Hero",
    'Victim Team',
    "Victim Name",
    "Victim Hero",
    "Event Ability",
    'Event Damage',
    'Is Critical Hit',
    'Is Environmental'
  ), sep = ',') %>%
  janitor::clean_names() %>%
  group_by(game_id) %>%
  mutate(damage_id = 1:n()) %>%
  mutate(across(c(attacker_name, victim_name), tolower) )

HEALTH <-
  data %>%
  filter(event == "healing") %>%
  separate(log, into = c(
    "Healer Team",
    "Healer Name",
    "Healer Hero",
    'Healee Team',
    "Healee Name",
    "Healee Hero",
    "Event Ability",
    'Event Healing',
    'Is Health Pack'
  ), sep = ',') %>%
  janitor::clean_names() %>%
  group_by(game_id) %>%
  mutate(health_id = 1:n()) %>%
  mutate(across(c(healer_name, healee_name), tolower) )

DAMAGE %>% 
  bind_rows(HEALTH) %>% 
  bind_rows(KILLS) %>%
  mutate(player_id = coalesce(attacker_name, healee_name),
         player_hero = coalesce(attacker_hero, healee_hero),
         player_team = coalesce(attacker_team, healee_team),
         event_id = coalesce(damage_id, kill_id, health_id)) %>%
  mutate(player_id = tolower(player_id)) ->
  ALL_EVENTS

source(here::here('workshop-r-files',"get_state_by_time.R"))
