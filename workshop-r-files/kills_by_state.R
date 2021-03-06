source(here::here('workshop-r-files',"convert_log_data.R"))

## need to combine kill data with tf data

kills <-
  data %>%
  filter(event == "kill") %>%
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
  mutate(across(c(attacker_name, victim_name), tolower))

kills %>%
  bind_rows(kills %>% rename(victim_name = attacker_name, 
                             attacker_name = victim_name,
                             victim_team = attacker_team,
                             attacker_team = victim_team)) %>%
  group_by(game_id, attacker_team, attacker_name) %>%
  count() %>% 
  group_by(game_id, attacker_team) %>%
  arrange(desc(n)) %>%
  slice(1:6) %>%
  select(game_id, team = attacker_team, player_name = attacker_name) ->
  rosters

kills %>%
  # filter(attacker_team != victim_team) %>%
  left_join(rosters, by = 'game_id') %>%
  mutate(type=case_when(team == attacker_team  ~ "FOR", TRUE ~ "AGAINST")) %>%
  select(-starts_with("team_")) ->
  pbp_full

## label each interaction with the time since
## last engagement for the two participating players
kills %>%
  pivot_longer(c(attacker_name, victim_name),
               names_to = "type",
               values_to = "player") ->
  pbp_long

pbp_full %>%
  # mutate(time = as.numeric(time)) %>%
  # left_join(teamfight_index %>% select(game_id, time, tf_no), by = c('game_id','time')) %>%
  # arrange(match_id, player_name) %>%
  group_by(game_id) %>%
  mutate(
    time_back = time_s + ((victim_name == player_name)*15)
  ) %>%
  group_by(game_id, player_name) %>%
  mutate(alive = (cummax(lag(time_back, default = 0)) < time_back)*1) %>% 
  select(-time_back) %>%
  tidyr::pivot_wider(
    id_cols = c(game_id, kill_id, time_s,time,  attacker_team),
    names_from = c(player_name, type),
    names_glue = "{player_name}_{type}",
    values_from = alive,
    values_fn = sum,
    values_fill = 0
  ) %>%
  arrange( game_id, time_s) ->
  pbp_data_wide

## add game state vars
pbp_data_wide %>%
  ungroup() %>%
  select(ends_with("FOR")) %>%
  rowSums() %>%
  as_tibble() %>%
  rename(for_alive = value) ->
  for_l

pbp_data_wide %>%
  ungroup() %>%
  select(ends_with("AGAINST")) %>%
  rowSums() %>%
  as_tibble %>%
  rename(against_alive = value) ->
  against_l

pbp_data_wide %>%
  # left_join(match_info %>% collect() %>% select(match_id, mode, map),
  #           by = "match_id") %>%
  bind_cols(for_l) %>%
  bind_cols(against_l) %>%
  group_by(game_id) %>%
  mutate(state = as.factor(paste0(for_alive, "v", against_alive)),
         adv = for_alive - against_alive) %>%
  select(game_id, kill_id, adv, state) ->
  state_by_events

kills %>%
  select(game_id, time, time_s,kill_id) %>%
  left_join(state_by_events, c('game_id', 'kill_id')) ->
  state_by_time

kills %>%
   left_join(state_by_time, by = c('game_id', 'kill_id', 'time_s')) %>%
  # filter(state == "0v1") %>%
  # view()
  group_by(state) %>%
  count() %>% arrange(desc(n))

kills %>%
  left_join(teamfight_index %>% select(game_id, time, tf_no), by = c('game_id','time')) %>%
  left_join(state_by_time, by = c('game_id', 'kill_id', 'time_s')) %>%
  left_join(aliases_q, by = c('victim_name' = "player_name")) %>%
  mutate(victim_name = coalesce(player_alias, victim_name)) %>%
  filter(# victim_name %in% (aliases_q %>% filter(player_alias == "skewed") %>% pull(player_name)),
         victim_hero == "Brigitte") %>%
  group_by(victim_name, adv) %>%
  count() %>%
  view()
  
#EV Kills
kills %>%
  left_join(teamfight_index %>% select(game_id, time, tf_no), by = c('game_id','time')) %>%
  left_join(state_by_time, by = c('game_id', 'kill_id', 'time_s')) %>%
  left_join(aliases_q, by = c('attacker_name' = "player_name")) %>%
  mutate(attacker_name = coalesce(player_alias, attacker_name)) %>%
  filter(# victim_name %in% (aliases_q %>% filter(player_alias == "skewed") %>% pull(player_name)),
    victim_hero == "Brigitte") %>%
  group_by(attacker_name, adv) %>%
  count() %>%
  view()

kills %>%
  left_join(teamfight_index %>% select(game_id, time, tf_no, total_kills =kills), by = c('game_id','time')) %>%
  group_by(game_id, attacker_team, tf_no, total_kills) %>%
  summarise(kills = n(),
            kill_pct = kills/mean(total_kills+1)) %>%
  ungroup() %>%
  arrange(game_id, tf_no)
  left_join(state_by_time, by = c('game_id', 'kill_id', 'time_s')) %>%
  filter(victim_name %in% (aliases_q %>% filter(player_alias == "skewed") %>% pull(player_name)),
         victim_hero == "Brigitte") 