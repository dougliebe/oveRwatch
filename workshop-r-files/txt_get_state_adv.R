
library(odbc)
library(DBI)
library(tidyverse)
library(dbplyr)

con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "overwatch",
                      username    = 'admin',
                      password    = "guerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)

roster_q <- tbl(con, "STATS") %>%
  group_by(game_id, player_team, player_id) %>%
  count() %>%
  select(-n)

kills_q <- tbl(con, "KILLS")


kills_q %>%
  left_join(roster_q, by = 'game_id') %>%
  collect() %>%
  mutate(type=case_when(player_team == attacker_team  ~ "FOR", TRUE ~ "AGAINST")) %>%
  select(-starts_with("team_")) ->
  pbp_full

## label each interaction with the time since
## last engagement for the two participating players
kills_q %>%
  collect() %>%
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

kills_q %>%
  collect() %>%
  left_join(state_by_events, by = c('game_id', "kill_id")) %>%
  filter(state %in% c("6v6","6v5", '5v6')) %>%
  pivot_longer(c(attacker_name, victim_name)) %>%
  group_by(name, value, attacker_hero) %>%
  count() %>%
  # filter(adv == 0) %>%
  pivot_wider(id_cols = c(value, attacker_hero), names_from = name, values_from = n) %>%
  mutate(pm = attacker_name-victim_name) %>% arrange(desc(pm))
