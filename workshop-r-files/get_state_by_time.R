#### Get state by time data
library(odbc)
library(DBI)
library(tidyverse)
library(dbplyr)

roster_q <- STATS %>%
  group_by(game_id, player_team, player_id) %>%
  count() %>%
  select(-n)

kills_q <- KILLS

kills_q %>%
  left_join(roster_q, by = 'game_id') %>%
  mutate(type=case_when(player_team == attacker_team  ~ "FOR", TRUE ~ "AGAINST")) %>%
  select(-starts_with("team_")) ->
  pbp_full

## label each interaction with the time since

pbp_full %>%
  # mutate(time = as.numeric(time)) %>%
  # left_join(teamfight_index %>% select(game_id, time, tf_no), by = c('game_id','time')) %>%
  # arrange(match_id, player_id) %>%
  group_by(game_id) %>%
  mutate(
    time_back = time_s + ((victim_name == player_id)*20)
  ) %>%
  group_by(game_id, player_id) %>%
  mutate(alive = (cummax(lag(time_back, default = 0)) < time_back)*1) %>%
  select(-time_back) %>%
  tidyr::pivot_wider(
    id_cols = c(game_id, kill_id, time_s, attacker_team),
    names_from = c(player_id, type),
    names_glue = "{player_id}_{type}",
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

kills_q %>%
  collect()  %>%
  select(game_id, time_s, kill_id) %>%
  left_join(state_by_events, c('game_id', 'kill_id')) ->
  state_by_time

ALL_EVENTS %>%
  left_join(state_by_time %>% select(-kill_id),
          by = c('game_id', 'time_s')) %>%
  arrange(game_id, time_s) %>%
  fill(adv, state, .direction = 'up') %>%
  group_by(game_id) %>%
  mutate(state_change = cumsum(adv != lag(adv, default = 0))) %>%
  group_by(game_id, state_change) %>%
  mutate(duration = max(time_s)-min(time_s)) ->
  six_dhk


# six_dhk %>%
#   mutate(across(c(event_damage, event_healing), as.numeric)) %>%
#   mutate(player_id = coalesce(attacker_name, healee_name),
#          player_hero = coalesce(attacker_hero, healee_hero)) %>%
#   group_by(game_id, player_id,player_hero, state, state_change, duration) %>%
#   summarise(.groups = 'keep',
#             damage = sum(event_damage, na.rm = T),
#             first_kills = sum(event == "kill"),
#             pack_healing = sum(event_healing*(is_health_pack == "True"), na.rm = T),
#             team_healing = sum(event_healing*(is_health_pack == 0), na.rm = T)
#   ) %>%
#   filter(state %in% c("5v5", "6v6","5v6",'6v5')) %>%
#   group_by(player_id, player_hero) %>%
#   summarise(
#     .groups = "keep",
#     damage = sum(damage),
#     first_kills= sum(first_kills),
#     pack_healing = sum(pack_healing),
#     team_healing = sum(team_healing),
#     duration = sum(duration),
#     games = length(unique(game_id))
#   ) %>%
#   mutate(
#     dps = damage/duration,
#     fbp10 = first_kills/duration*600,
#     hps = (pack_healing+team_healing)/duration,
#     pack_healing_pct = pack_healing/(pack_healing+team_healing),
#     healing_to_damage_ratio = hps/dps
#   ) %>%
#   filter(duration > 10*60) %>%
#   select(-damage, -first_kills, -pack_healing, -team_healing) %>%
#   DT::datatable() %>%
#   formatRound(columns = c(5:7,9)) %>%
#   formatPercentage(columns = 'pack_healing_pct')