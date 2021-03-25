
library(odbc)
library(DBI)
library(tidyverse)
library(dbplyr)

# which players actually play the round?
source(here::here('workshop-r-files','get_player_stats.R'))
head(player_round_max)
player_round_max %>%
  select(game_id,player_team, player_id) %>%
  group_by(game_id, player_team) %>%
  mutate(p_num = 1:n()) ->
  players_filter

KILLS %>%
  left_join(players_filter, by = 'game_id') %>%
  mutate(type=case_when(player_team == attacker_team  ~ "FOR", TRUE ~ "AGAINST")) ->
  pbp_full

## label each interaction with the time since
## last engagement for the two participating players
KILLS %>%
  pivot_longer(c(attacker_name, victim_name),
               names_to = "type",
               values_to = "player") ->
  pbp_long

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

adv_tbl <-
  tibble(
    adv = seq(-6, 6),
    win_prob_added = c(0, #-6
                       0,
                       0,
                       6.2,
                       15.3-6.2, 
                       44.1-15.3, # -1 to +0
                       79.2-50, # 0 to +1
                       92.1-79.2,
                       95.4-92.1,
                       2.1,
                       0,
                       0,
                       0)
  )

pbp_data_wide %>%
  # left_join(match_info %>% collect() %>% select(match_id, mode, map),
  #           by = "match_id") %>%
  bind_cols(for_l) %>%
  bind_cols(against_l) %>%
  group_by(game_id) %>%
  mutate(state = as.factor(paste0(for_alive, "v", against_alive)),
         adv = for_alive - against_alive) %>%
  select(game_id, kill_id, adv, state) %>%
  left_join(adv_tbl, by = 'adv') ->
  state_by_events

## need to put kills into tfs by game_id


KILLS %>%
  select(game_id, time_s, kill_id) %>%
  left_join(state_by_events, c('game_id', 'kill_id')) ->
  state_by_time

# KILLS %>%
#   left_join(state_by_events, by = c('game_id', "kill_id")) %>%
#   # filter(state %in% c("6v6","6v5", '5v6')) %>%
#   pivot_longer(c(attacker_name, victim_name)) %>%
#   group_by(name, value, attacker_hero) %>%
#   summarise(n= n(),
#             wpa = sum(ifelse(name == "attacker_name", win_prob_added, -win_prob_added))) %>%
#   # filter(adv == 0) %>%
#   pivot_wider(id_cols = c(value, attacker_hero), names_from = name, values_from = c(n, wpa)) %>%
#   mutate(pm = attacker_name-victim_name) %>% arrange(desc(pm))
