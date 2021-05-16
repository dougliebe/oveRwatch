## How do boxscore stats predict team kills and deaths
## want to determine the value of each pt of dmg, healing, and shield

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
stats_q <- tbl(con, "STATS")
team_game_q <- tbl(con, "TEAM_GAME")

team_score_q <- read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",
                           sheet = 'Fixed GameScores') %>% filter(!is.na(game_id))

stats_q %>%
  select(player_team, game_id, player_id , player_hero, final_blows, deaths,
         hero_damage_dealt, damage_taken, 
         barrier_damage_dealt, damage_blocked,
         healing_dealt, healing_received,
         hero_time_played) %>%
  collect() ->
  data

data %>%
  mutate(across(c('final_blows', 'deaths',
                  'hero_damage_dealt', 'damage_taken', 
                  'barrier_damage_dealt', 'damage_blocked',
                  'healing_dealt', 'healing_received'), parse_number)) %>%
  group_by(game_id, player_team, player_hero) %>%
  summarise(kills = sum(final_blows)/sum(hero_time_played),
            dmg_dlt = sum(hero_damage_dealt)/sum(hero_time_played),
            dmg_tkn = sum(damage_taken)/sum(hero_time_played),
            shield_dlt = sum(barrier_damage_dealt)/sum(hero_time_played),
            shield_blk = sum(damage_blocked)/sum(hero_time_played),
            heal_dlt = sum(healing_dealt)/sum(hero_time_played),
            heal_tkn = sum(healing_received)/sum(hero_time_played),
            time_played = sum(hero_time_played)) %>%
  filter(time_played >= 300) %>%
  left_join(team_game_q %>% collect(), by = c('game_id',"player_team" = 'team_in_game_name')) %>%
  left_join(team_score_q %>%
              pivot_longer(c(opponent, team_name),
                           names_to = 'type_team') %>%
              mutate(result = ifelse(type_team == "team_name", result == 1, result == 0)),
            by = c('game_id','team_name' = 'value')) ->
  filtered_data

## find which heros we have enough data
filtered_data %>%
  ungroup() %>%
  count(player_hero) %>%
  filter(n > 30) ->
  heros_filter

## run a model for each hero
filtered_data %>%
  filter(player_hero %in% heros_filter$player_hero) %>%
  group_by(player_hero, result) %>%
  summarise(kills = mean(kills)) %>%
  filter(!is.na(result)) %>%
  ggplot(aes(result, kills, fill = player_hero), alpha = 0.2)+
  geom_col()+
  facet_wrap(~player_hero)

  
  
  
  
  
  
  
  
  















