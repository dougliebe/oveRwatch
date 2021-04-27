## trying to get a picture of tfs based on heros in comps, 
# not players specifically
# should probably break down by map and side too
library(odbc)
library(DBI)
library(tidyverse)
library(googlesheets4)

con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "overwatch",
                      username    = 'admin',
                      password    = "guerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)

## check games in db
tbl(con, "TF_STATS") %>%
  collect() ->
  tf_data
team_game_q <- tbl(con, 'TEAM_GAME')
aliases_q <- read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",
                        sheet = 'aliases',
                        col_types = 'cccc') %>%
  mutate(player_alias = tolower(player_alias),
         player_name = tolower(player_name))
games_q <- read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",
                                  sheet = 'Fixed GameScores') %>% filter(!is.na(game_id))


tf_data %>%
  group_by(game_id, tf_no, player_team, kill_id) %>%
  mutate(suicide = any(player_kill == 1 & player_death == 1)) %>%
  filter(!suicide) %>%
  left_join(team_game_q %>% collect(), by = c('game_id','player_team' = 'team_in_game_name')) %>%
  group_by(game_id, tf_no, team_name, tf_win) %>%
  summarise(comp = paste0(sort(unique(player_hero)), collapse = "-")) %>%
  group_by(game_id, tf_no) %>%
  mutate(opp_comp = ifelse(1:n() == 1, lead(comp), lag(comp))) %>%
  left_join(games_q %>% select(game_id, date, map, patch, region)) %>%
  # filter(map == "Eichenwalde") %>%
  filter( team_name == 'gladiators', date > "2021-03-15") %>%
  group_by(comp, opp_comp) %>%
  summarise(win_pct = mean(tf_win),
            fights = n()) %>%
  filter(opp_comp == "Baptiste-D.Va-Lucio-McCree-Mei-Reinhardt") %>%
  arrange(desc(fights))

## look at players in comps
games_q %>%
  pivot_longer(c(p_1, p_2, p_3, p_4, p_5, p_6)) %>%
  left_join(aliases_q, by = c('value' = 'player_name')) %>%
  mutate(player_name = coalesce(player_alias, value)) %>%
  filter(map == "Hanamura") %>%
  group_by(player_name) %>%
  count()


## filter out a comp, then look at the order in which they die in wins vs losses
tf_data %>%
  group_by(game_id, tf_no, player_team, kill_id) %>%
  mutate(suicide = any(player_kill == 1 & player_death == 1)) %>%
  filter(!suicide, player_kill == 1 | player_death == 1) %>%
  left_join(team_game_q %>% collect(), by = c('game_id','player_team' = 'team_in_game_name')) %>%
  group_by(game_id, tf_no, team_name, tf_win) %>%
  arrange(time) %>%
  mutate(comp = paste0(sort(unique(player_hero)), collapse = "-")) %>%
  filter(player_death == 1, state == "6v6") %>%
  filter(comp == "Baptiste-D.Va-Lucio-McCree-Mei-Reinhardt") %>%
  group_by(game_id, tf_no, team_name, tf_win) %>%
  summarise(death_order = paste0(player_hero[1:(min(2,length(player_hero)))], collapse = "-"),
            first_death = player_hero[1],
            deaths = length(player_hero)) %>%
  # filter(deaths >= 2) %>%
  group_by(us = team_name == "gladiators", first_death) %>%
  summarise(win = mean(tf_win), n = n()) %>%
  group_by(us) %>%
  mutate(n = n/sum(n)) %>%
  # filter(n > 0.02) %>%
  arrange(desc(n)) %>%
  ggplot()+
  ggrepel::geom_label_repel(aes(n,win,label = first_death, color = us))+
  labs(x = "Occurance, %",
       y = "TF Win%",
       title = "TF W% by hero killed first in teamfight")+
  theme_bw()+
  scale_y_reverse(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  theme(axis.title = element_text(size = 16))


tf_data %>%
  group_by(game_id, tf_no, player_team, kill_id) %>%
  mutate(suicide = any(player_kill == 1 & player_death == 1)) %>%
  filter(!suicide, player_kill == 1 | player_death == 1) %>%
  left_join(team_game_q %>% collect(), by = c('game_id','player_team' = 'team_in_game_name')) %>%
  group_by(game_id, tf_no, team_name, tf_win) %>%
  arrange(time) %>%
  mutate(comp = paste0(sort(unique(player_hero)), collapse = "-")) %>%
  filter(player_death == 1) %>%
  filter(comp == "Baptiste-D.Va-Lucio-McCree-Mei-Reinhardt") %>%
  # group_by(game_id, tf_no, team_name, tf_win) %>%
  # summarise(death_order = paste0(player_hero[1:(min(2,length(player_hero)))], collapse = "-"),
  #           first_death = player_hero[1],
  #           deaths = length(player_hero)) %>%
  # filter(deaths >= 2) %>%
  group_by( player_hero) %>%
  summarise(win = mean(tf_win), n = n()) %>%
  mutate(n = n/sum(n)) %>%
  select(hero_dead = 1, win_tf_after_death = 2, occur_pct = 3) %>%
  arrange(desc(occur_pct))
  filter(n > 0.02) %>%
  arrange(desc(n)) %>%
  ggplot()+
  ggrepel::geom_label_repel(aes(n, win, label = player_hero))+
  labs(x = "Occurance, %",
       y = "TF Win%",
       title = "TF W% by hero killed first-second in teamfight")+
  theme_bw()+
  scale_y_reverse(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  theme(axis.title = element_text(size = 16))


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
