library(odbc)
library(DBI)
library(tidyverse)
library(dbplyr)
con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "payload",
                      username    = 'admin',
                      password    = "laguerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)

### Test the new db tables
tbl(con, "GAMERESULT") ->
  game_result_q
tbl(con, "GAMESTART") ->
  game_start_q
tbl(con, "ROSTERS") ->
  roster_q
tbl(con, "TEAMS") ->
  team_q
tbl(con, "PLAYERSTATUS") ->
  status_q

# get the guids
map_guids <- read_delim(here::here('guids', 'payload_guids_maps.tsv'),
                        delim = "\t",col_types = "cc")
map_guids$map_name %>% 
  tidyjson::spread_values(
    map_name = tidyjson::jstring(en_US)
  ) %>%
  bind_cols(guid = map_guids$guid) %>% 
  as_tibble() ->
  map_guids

## find how many map wins each player had this weekend
game_result_q %>%
  left_join(roster_q, by = 'match_game_id') %>% 
  left_join(team_q, by = c('winningteamid' = "team_id")) %>%
  filter(end_reason == "NORMAL") %>% 
  collect() ->
  normal_games

normal_games %>% 
  filter(base_player.esports_player_id == 4141) %>% 
  mutate(win = team.esports_team_id == esports_team_id) %>%
  group_by(base_player.esports_player_id) %>%
  summarise(wins = sum(win),
            loses = n()-wins)

team_q %>% 
  group_by(team_id) %>% 
  count() %>% arrange(desc(n))

game <- "matchgame-014966a6-5a44-c14e-ae3b-4068e1680a51"
status_q %>% 
  filter(match_game_id == !!game) %>% 
  collect() ->
  game_data_statuses

game_start_q %>% 
  mutate(map_guid = as.character(format(map_guid, scientific = F))) %>% 
  filter(match_game_id == !!game) %>% 
  # mutate(map_guid = map_guid %% 10000) %>% 
  collect() %>%
  left_join(map_guids, by=c('map_guid' = 'guid'))

game_data_statuses %>% 
  mutate(time = lubridate::as_datetime(time/1000)) %>% 
  filter(position.x > -1000) %>% 
  ggplot(aes(position.x, position.y, color = as.factor(player.esports_player_id)))+
  geom_point(alpha = 0.2)










