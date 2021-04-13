## look at stats by date
library(odbc)
library(DBI)
library(tidyverse)
library(dbplyr)
library(googlesheets4)
library(DT)
library(gt)
con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "overwatch",
                      username    = 'admin',
                      password    = "guerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)

stats_q <- tbl(con, "STATS")
tf_stats_q <- tbl(con, "TF_STATS")
team_game_q <- tbl(con, 'TEAM_GAME')
aliases_q <- read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",
                        sheet = 'aliases',
                        col_types = 'cccc') %>%
  mutate(player_alias = tolower(player_alias),
         player_name = tolower(player_name))
games_q <-  read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",
                       sheet = 'Fixed GameScores') %>% filter(!is.na(game_id))

## get eich games only
games_q %>% 
  filter(map == "Eichenwalde") %>%
  pull(game_id) ->
  games_filter_

stats_q %>%
  filter(game_id %in% games_filter_ ) %>%
  collect() %>%
  left_join(aliases_q, by = c('player_id' = 'player_name')) %>%
  mutate(player_name = coalesce(player_alias, player_id)) %>%
  mutate(date = paste0(substr(game_id, 5,8),
                       "-", substr(game_id, 1,2),
                       "-", substr(game_id, 3,4)),
         date = lubridate::as_date(date)) %>%
  group_by(player_name, recent = date >= "2021-04-09") %>%
  summarise(fb10 = sum(parse_number(final_blows))/sum((hero_time_played))*600,
            dmg10 = sum(parse_number(hero_damage_dealt))/sum((hero_time_played))*600,
            deaths10 = sum(parse_number(deaths))/sum((hero_time_played))*600,
  ) %>%
  pivot_longer(c(fb10, dmg10, deaths10)) %>%
  # pivot_wider(id_cols = player_name,
  #             names_from = c(recent, name),
  #             values_from = value) %>%
  filter(player_name %in% c("kev", 'bird','mirror','shu','space','skewed','muze','moth')) %>%
  group_by(player_name, name) %>%
  mutate(mult_change = value/(sum(value)-value)) %>%
  filter(recent) %>%
  pivot_wider(id_cols = c(player_name, recent),
              names_from = name, values_from = mult_change)
  ggplot(aes(name, value,color = recent))+
  geom_col(position = "dodge")+
  facet_wrap(~player_name)
  

data %>%
  mutate(date = paste0(substr(game_id, 5,8),
                       "-", substr(game_id, 1,2),
                       "-", substr(game_id, 3,4)),
         date = lubridate::as_date(date, "%m-%d-%Y")) %>%
  group_by(date) %>%
  summarise(dmg10 = sum(parse_number(hero_damage_dealt))/sum(hero_time_played),
            games = length(unique(game_id))) %>%
  ggplot(aes(date, dmg10))+
  geom_line()+
  geom_point(aes(size = games))+
  theme_bw()+
  labs(x = "Date")

data %>%
  filter(player_alias %in% c('kev','muze','space','bird','mirror')) %>%
  mutate(date = paste0("2021",
                       "-", substr(game_id, 1,2),
                       "-", substr(game_id, 3,4)),
         date = lubridate::as_date(date)) %>%
  # arrange(date) %>% head %>% view
  group_by(date, player_alias) %>%
  summarise(dmg10 = sum(parse_number(hero_damage_dealt))/sum(hero_time_played),
            games = length(unique(game_id))) %>%
  ggplot(aes(date, dmg10, color = player_alias))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  theme_bw()+
  labs(x = "Date", y = "Shield Dmg10")

data %>%
  group_by(game_id, player_alias, player_team) %>%
  summarise(shield_dmg = sum(parse_number(hero_damage_dealt))) %>%
  group_by(game_id, player_team) %>%
  mutate(dmg_pct = shield_dmg/sum(shield_dmg)) %>%
  group_by(player_alias) %>%
  summarise(teams_shield_dmg_pct = scales::percent(mean(dmg_pct, na.rm =T)),
            games = n()) %>%
  arrange(desc(games))

data %>%
  group_by(game_id, player_alias, player_team) %>%
  summarise(fbs = sum(parse_number(final_blows))) %>%
  group_by(game_id, player_team) %>%
  mutate(fb_pct = fbs/sum(fbs)) %>%
  group_by(player_alias) %>%
  summarise(teams_fb_pct = mean(fb_pct, na.rm =T),
            games = n()) %>%
  filter(games > 20) %>%
  arrange(desc(teams_fb_pct))

data %>%
  group_by(game_id, player_alias, player_team, player_hero) %>%
  filter(hero_time_played > 300, parse_number(healing_dealt ) < 100) %>%
  mutate(fb10 = parse_number(final_blows)/hero_time_played*600) %>%
  group_by(game_id, player_team) %>%
  mutate(fb_pct = fb10/mean(fb10)) %>%
  group_by(player_alias, player_hero) %>%
  summarise(fb10_mult = mean(fb_pct, na.rm =T),
            games = n()) %>%
  filter(games > 5) %>%
  arrange(desc(fb10_mult))
