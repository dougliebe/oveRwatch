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


base <- data$info %>%
  # spread_all() %>%
  spread_values(
    esports_match_id = jnumber(esports_ids,esports_match_id),
    match_id = jstring(esports_ids, match_id),
    esports_tournament_handle = jstring(esports_ids, esports_tournament_handle),
    esports_tournament_id = jnumber(esports_ids, esports_tournament_id),
    map_guid = jnumber(game_context, map_guid),
    map_type = jstring(game_context, map_type),
    game_mode_guid = jnumber(game_context, game_mode_guid),
    match_game_id = jstring(esports_ids, match_game_id),
    esports_match_game_number = jnumber(esports_ids, esports_match_game_number)
  ) %>%
  tibble::as_tibble() 

string <- data$info %>%
  enter_object('esports_ids','esports_tournament_attributes') %>%
  append_values_string()

attributes <- string$string %>%
  as.tbl_json() %>%
  # spread_all
  spread_values(
    environment = jstring( environment),
    phase = jstring( phase),
    season_id = jstring( season_id),
    type = jstring( type),
    format = jstring( stage, format)
  ) %>%
  tibble::as_tibble()

## Match data
base %>%
  left_join(attributes, by = 'document.id') %>%
  select(match_id, esports_match_id, esports_tournament_id, esports_tournament_handle, 
         environment, phase, season_id, type, format) %>%
  distinct()
# %>%
# write_delim(paste0("D:/OW/new/",
#                    str_replace(filenames_foroutput[1], ".tsv",replacement = ""),
#                    "_matches",
#                    ".txt" ))

## game data
base %>%
  select(match_game_id,match_id, esports_match_id, esports_match_game_number, map_guid,
         map_type, game_mode_guid) %>%
  distinct()