## get output with a line for each player 
# in each teamfight and the resulting stats
source(here::here('workshop-r-files',"convert_log_data.R"))
library(googlesheets4)
aliases_q <- read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",sheet = 'aliases',col_types = 'cccc') %>%
  mutate(player_alias = tolower(player_alias),
         player_name = tolower(player_name))

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
  group_by(game_id, player_team, player_id, player_hero) %>%
  filter(hero_time_played == max(hero_time_played)) %>%
  arrange(desc(round_no)) %>%
  slice(1) %>%
  ungroup() ->
  STATS
## separate context per player, hero
# filter out ashe gameplay only
## want to see 

KILLS %>%
  filter(attacker_hero == "Ashe") %>%
  group_by(game_id, attacker_name, attacker_hero) %>%
  summarise(dynamite_kills = sum(event_ability == "Ability 2", na.rm = T),
            bob_kills = sum(event_ability == "Ultimate", na.rm = T)) %>%
  left_join(aliases_q, by = c("attacker_name" = "player_name_lower")) %>%
  mutate(player_name = coalesce(player_alias_lower, attacker_name)) %>%
  group_by(player_name, attacker_hero) %>%
  summarise(
    dynamite_kills = sum(dynamite_kills),
    bob_kills = sum(bob_kills)
  ) ->
  ashe_kills

STATS %>%
  left_join(aliases_q, by = c("player_id" = "player_name_lower")) %>%
  mutate(player_name = coalesce(player_alias_lower, player_id)) %>%
  group_by(player_name, player_hero) %>%
  summarise(
    hero_time_played = sum(hero_time_played, na.rm = T)
  ) %>%
  filter(player_hero == "Ashe") ->
  ashe_time

ashe_kills %>%
  left_join(ashe_time, by = c("player_name",'attacker_hero'= 'player_hero')) %>%
  mutate(
    dynamite_10 = dynamite_kills/hero_time_played*600,
    bob_10 = bob_kills/hero_time_played*600,
  ) %>%
  select(player_name, attacker_hero, hero_time_played, dynamite_10, bob_10) %>%
  filter(hero_time_played > 30*60) %>%
  mutate(hero_time_played = hms::as_hms(hero_time_played)) %>%
  arrange(desc(dynamite_10))


## now look at brig pulse deaths
KILLS %>%
  filter(attacker_hero == "Tracer", victim_hero == "Brigitte", event_ability == "Ultimate") %>%
  group_by(game_id, victim_name, victim_hero) %>%
  count() %>%
  left_join(aliases_q, by = c("victim_name" = "player_name_lower")) %>%
  mutate(player_name = coalesce(player_alias_lower, victim_name)) %>%
  group_by(player_name, victim_hero) %>%
  summarise(
    pulse_deaths = sum(n)
  ) ->
  brig_deaths

STATS %>%
  left_join(aliases_q, by = c("player_id" = "player_name_lower")) %>%
  mutate(player_name = coalesce(player_alias_lower, player_id)) %>%
  group_by(player_name, player_hero) %>%
  summarise(
    hero_time_played = sum(hero_time_played, na.rm = T)
  ) %>%
  filter(player_hero == "Brigitte") ->
  brig_time

brig_deaths %>%
  left_join(brig_time, by = c("player_name",'victim_hero'= 'player_hero')) %>%
  mutate(
    pulse_10 = pulse_deaths/hero_time_played*600
  ) %>%
  select(player_name, victim_hero, hero_time_played, pulse_10) %>%
  filter(hero_time_played > 30*60) %>%
  mutate(hero_time_played = hms::as_hms(hero_time_played)) %>%
  arrange(desc(hero_time_played))













