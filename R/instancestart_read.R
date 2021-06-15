library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = here::here('data','match_data','20210419'),pattern = "payload_instancestart.*.tsv",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
head(data,1)



#### Handle the info ####

## To look at the data
data$info[1] %>%
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


## EVENT handling

## Event rows
data %>%
  select(time, schema_name, time_c, esports_match_id) %>%
  # # need keys for info only
  bind_cols(
    data$info %>%
      spread_values(event_id = jnumber(event_id),
                    src = jnumber(instance_id, src),
                    seq = jnumber(instance_id, seq),
                    match_game_id = jstring(esports_ids, match_game_id)) %>%
      select(event_id, seq, src, ,match_game_id) %>% as_tibble
  ) %>%
  group_by(match_game_id) %>%
  mutate(schema_event_id = 1:n()) ->
  instancestart_events
