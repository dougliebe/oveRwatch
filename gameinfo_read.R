library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = "D:/OW/",pattern = "payload_gameinfo.*.tsv$",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE, nrows = 1000)
head(data,1)


#### Handle the info ####

## To look at the data
data$score_info[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()

base <- data$info %>%
  spread_values(
    'event_id'= jnumber(event_id),
    sequence_length = jnumber(sequence_length),
    instance_id = jnumber(instance_id, seq),
    instance_id.src = jnumber(instance_id, src)
  ) %>%
  tibble::as_tibble() 

# data.info$esports_ids
esports_ids <- data$info %>%
  # spread_all() %>%
  enter_object('esports_ids') %>%
  spread_all %>%
  tibble::as_tibble() %>%
  select(-esports_tournament_attributes)

context <- data$info %>%
  enter_object('game_context') %>%
  spread_all %>%
  tibble::as_tibble()

string <- data$info %>%
  enter_object('esports_ids','esports_tournament_attributes') %>%
  append_values_string()

attributes <- string$string %>%
  as.tbl_json() %>%
  spread_all %>%
  tibble::as_tibble()



######## Break up the score_info data ######

data$score_info %>%
  spread_values(
    round_num= jnumber(round_num),
    # round_name_guid = jstring(round_name_guid),
    attacking_team_id = jstring(attacking_team_id)
  ) %>%
  enter_object(team_info) %>%
  gather_array() %>%
  spread_values(
    team_id = jstring(team_id),
    payload_distance = jnumber(payload_distance),
    time_banked = jnumber(time_banked),
    score = jnumber(score),
    control_info = jstring(control_info)
  ) %>%
  tibble::as_tibble() %>%
  left_join(data$info %>%
              spread_values(
                event_id = jnumber(event_id),
                match_game_id = jstring(esports_ids, match_game_id)
                ) %>%
              as.tibble() %>%
              bind_cols(time = data$time,
                        time_c = data$time_c,
                        schema_name = data$schema_name), by = 'document.id'
  ) %>%
  select(-team_id, -document.id, -array.index)


## Break into 3 groups:
# event, game, match

## Match data
base %>%
  left_join(esports_ids, by = 'document.id') %>%
  left_join(attributes, by = 'document.id') %>%
  select(match_id, esports_match_id, esports_tournament_id, esports_tournament_handle, 
         environment, phase, season_id, type, stage.format) %>%
  distinct() %>%
  write_delim(paste0("D:/OW/new/",
                     str_replace(filenames_foroutput[1], ".tsv",replacement = ""),
                     "_matches",
                     ".txt" ))

## game data
base %>%
  left_join(esports_ids, by = 'document.id') %>%
  left_join(context, by = 'document.id') %>%
  select(match_game_id, esports_match_id, esports_match_game_number, map_guid,
         map_type, game_mode_guid) %>%
  distinct() %>%
  write_delim(paste0("D:/OW/new/",
                     str_replace(filenames_foroutput[1], ".tsv",replacement = ""),
                     "_games",
                     ".txt" ))

## event data
base %>%
  left_join(esports_ids, by = 'document.id') %>%
  left_join(context, by = 'document.id') %>%
  select(document.id, event_id,match_game_id, sequence_length,  round) %>%
  left_join(base_score %>%
              left_join(team_info, by = 'document.id'), by = 'document.id') %>%
  write_delim(paste0("D:/OW/new/",
                     str_replace(filenames_foroutput[1], ".tsv",replacement = ""),
                     "_events",
                     ".txt" ))
