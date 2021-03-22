
source(here::here('workshop-r-files',"convert_log_data.R"))
library(odbc)
library(DBI)
library(tidyverse)
library(dbplyr)
library(bigrquery)
billing <- "civil-ripple-305517"
con <- DBI::dbConnect(
  # driver
  bigrquery::bigquery(),
  # projects are server names in aws
  project = "civil-ripple-305517",
  # datasets are databases in aws 
  dataset = "overwatch",
  billing = billing
  )
# can see tables in dataset
bq_dataset_tables(con)


bq_table_upload(STATS)

bq_table_download(x = bq_table("civil-ripple-305517",'overwatch','STATS')) %>%
  pull(game_id) %>%
  unique(.) ->
  games_already_in

STATS %>%
  filter(!game_id %in% games_already_in) ->
  filtered_STATS

filtered_STATS %>%
  # filter(game_id == "031920210319115100") %>%
  # as.list() %>%
  bq_table_upload(x = bq_table("civil-ripple-305517",'overwatch','STATS'),
                  create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')

bq_tab

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


### left join 
aliases_q <- read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",sheet = 'aliases',col_types = 'cccc') %>%
  mutate(player_alias = tolower(player_alias),
         player_name = tolower(player_name))

STATS %>%
  left_join(aliases_q, by = c('player_id' = "player_name")) %>%
  mutate(player_id = coalesce(player_alias, player_id)) %>%
  # group_by(player_id) %>% count() %>% arrange(desc(n))
  # filter(player_alias == "skewed") %>% 
  filter( player_hero == "Brigitte") ->
  moth_skewed

moth_skewed %>%
  group_by(player_id) %>%
  summarise(time_played = sum(hero_time_played),
            dmg = sum(as.numeric(hero_damage_dealt)),
            healing = sum(as.numeric(healing_dealt)),
            deaths = sum(as.numeric(deaths))) %>%
  mutate(dmg = dmg/time_played*600,
         healing = healing/time_played*600,
         deaths = deaths/time_played*600)

tbl(con, "STATS") %>%
  pull(player_id) ->
  names

names %>%
  as_tibble() %>%
  select(player_name = 1) %>%
  left_join(aliases_q) %>%
  mutate(player_id = coalesce(player_alias, player_name)) %>%
  select(player_id) ->
  new_names

DBI::dbWriteTable(conn =  con,
                  name = "STATS",
                  value = new_names, 
                  as_bq_fields(new_names),
                  overwrite = TRUE, 
                  append = FALSE)
