library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = here::here('data','match_data','20210419'),pattern = "payload_gameinfo.*.tsv",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE, nrows = 1000)
slice(data,250)


#### Handle the info ####

## To look at the data
data$score_info[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()

## need to first create the tables for each level of info
## 1. instance
data$info %>%
  spread_values(match_game_id = jstring(esports_ids, match_game_id)) %>%
  # gather_object %>% 
  select(match_game_id) %>%
  enter_object('instance_id') %>%
  spread_all %>%
  as_tibble %>%
  distinct(.) ->
  INSTANCE

## 2. matchgame
data$info %>%
  spread_values(map_type = jstring(game_context, map_type),
                game_mode_guid = jstring(game_context, game_mode_guid),
                esports_match_game_number = jnumber(esports_ids, esports_match_game_number),
                match_id =  jstring(esports_ids, match_id),
                match_game_id =  jstring(esports_ids, match_game_id)) %>%
  select(-document.id) %>%
  as_tibble %>%
  distinct(.) ->
  MATCHGAME

## 3. match
data$info %>%
  spread_values(esports_tournament_id = jstring(esports_ids, esports_tournament_id),
                esports_match_id =  jstring(esports_ids, esports_match_id),
                match_id =  jstring(esports_ids, match_id)) %>%
  select(-document.id) %>%
  as_tibble() %>%
  distinct(.) ->
  MATCH


## to break down the vector of json entries in attributes
f2 <- function(x) {
  res <- fromJSON(paste0("[", paste(x, collapse = ","), "]"), flatten = TRUE)
  lst <- sapply(res, is.list)
  res[lst] <- lapply(res[lst], function(x) as.data.table(transpose(x)))
  res <- flatten(res)
  return(res)
}

## 4. Tournament
data$info %>%
  enter_object(esports_ids) %>%
  spread_all %>%
  select(contains("tournament")) %>%
  select(-esports_tournament_attributes) %>%
  bind_cols(
    data$info %>%
      spread_values(esports_tournament_attributes = jstring(esports_ids, esports_tournament_attributes)) %>% 
      pull(esports_tournament_attributes) %>%
      f2
  ) %>%
  as_tibble() %>%
  distinct ->
  TOURNAMENT


## SCORE Info Handling ###  

## 5. Round
data$score_info %>%
  spread_all %>%
  select(round_num, attacking_team.esports_team_id, round_name_guid) %>%
  bind_cols(data$info %>%
              spread_values(match_game_id = jstring(esports_ids, match_game_id)) %>% 
              enter_object('instance_id') %>%
              spread_all %>%
              as_tibble %>%
              select(-document.id)) %>%
  as_tibble() %>%
  distinct ->
  ROUND

## 6. Teamscore
data$score_info %>%
  spread_values(round_num = jnumber(round_num)) %>%
  select(-document.id) %>%
  bind_cols(data$info %>%
              enter_object('instance_id') %>%
              spread_all %>%
              as_tibble %>%
              select(-document.id)) %>%
  bind_cols(time = data$time) %>%
  enter_object('team_info') %>%
  gather_array() %>%
  spread_all %>%
  select(-array.index, -team.team_id, -team.esports_team_id) %>%
  as_tibble() ->
  TEAMSCORE
  
  
## EVENT handling

## Event rows
data %>%
  select(time, schema_name, time_c, esports_match_id) %>%
  # # need keys for info and score
  bind_cols(
    data$info %>%
      spread_values(event_id = jnumber(event_id),
                    match_game_id = jstring(esports_ids, match_game_id)) %>%
      select(event_id,match_game_id) %>% as_tibble,
    data$score_info %>%
      spread_values(attacking_team = jstring(attacking_team_id),
                    round_num = jnumber(round_num)) %>%
      select(round_num) %>%
      as_tibble
  ) %>%
  group_by(match_game_id) %>%
  mutate(schema_event_id = 1:n()) ->
  gameinfo_events
  
