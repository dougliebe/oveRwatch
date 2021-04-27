library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = here::here('data','match_data','20210419'),pattern = "payload_gameresult.*.tsv",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
head(data,1)



#### Handle the info ####

## To look at the data
data$players[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()
data$players[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()

base <- data$info %>%
  spread_values(
    'event_id'= jnumber(event_id),
    sequence_length = jnumber(sequence_length)
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

## To look at the data
data$score_info[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()

base_score <- data$score_info %>%
  spread_all %>%
  # spread_values(
  #   round_num= jnumber(round_num),
  #   round_name_guid = jstring(round_name_guid),
  #   attacking_team_id = jstring(attacking_team_id)
  # ) %>%
  tibble::as_tibble() %>%
  select(-attacking_team_id) %>%
  bind_cols(data[,c(1,3,8)])

team_info <- data$score_info %>%
  enter_object(team_info) %>%
  gather_array() %>%
  # spread_values(
  #   team_id = jstring(team_id),
  #   payload_distance = jnumber(payload_distance),
  #   time_banked = jnumber(time_banked),
  #   score = jnumber(score),
  #   control_info = jstring(control_info)
  # ) %>%
  # enter_object(team) %>%
  spread_all %>%
  tibble::as_tibble() %>%
  select(-team_id)

## player info
data$players %>%
  gather_array() %>%
  spread_values(
    team_id = jstring(team, team_id),
    esports_team_id = jnumber(team, esports_team_id),
    battletag = jstring(base_player,battletag),
    esports_player_id = jnumber(base_player,esports_player_id)
  ) %>%
  as.tibble() %>%
  left_join(data$info)
  bind_cols(esports_match_id = data$esports_match_id,
            time = data$time) %>%
  select(-document.id, -array.index)

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
  
