library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = here::here('data','match_data','20210419'),pattern = "payload_playerstatus.*.tsv",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE, nrow = 1)
head(data,1)



#### Handle the info ####

## To look at the data
data$info[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()

data %>% 
  # as_tbl_json('statuses') %>%
  # # spread_all %>%
  # gather_array() %>%
  # # enter_object(player_id) %>%
  # spread_all %>%
  # as_tibble() %>%
  as_tbl_json('info') %>%
  enter_object('esports_ids') %>%
  gather_object() %>%
  enter_object('game_context')
  enter_object(position) %>%
  gather_object() %>%
  spread_all


start <- Sys.time()
data$statuses %>%
  gather_array() %>%
  # spread_all %>%
  spread_values(
    "player_id" = jnumber(player_id, seq),
    "team_id"= jstring(team_id),
    "hero_guid"= jnumber(hero_guid),
    "health"= jnumber(health),
    "armor"= jnumber(armor),
    "shields"= jnumber(shields),
    "max_health"= jnumber(max_health),
    "max_armor"= jnumber(max_armor),
    "max_shields"= jnumber(max_shields),
    "respawn_time_remaining"= jnumber(respawn_time_remaining),
    "ultimate_percent"= jnumber(ultimate_percent),
    "respawn_progress"= jnumber(respawn_progress),
    "is_dead"= jlogical(is_dead),
    "is_ultimate_ready"= jlogical(is_ultimate_ready),
    "yaw" = jnumber(yaw),
    'position.x' = jnumber(position, x),
    'position.y' = jnumber(position, y),
    'position.z' = jnumber(position, z)
  ) %>%
  as.tibble() %>%
  left_join(data %>%
              mutate(document.id = 1:n()) %>%
              select(document.id, time) %>%
              bind_cols(data$info %>%
                          spread_values(
                            match_game_id = jstring(esports_ids, match_game_id),
                            event_id = jnumber(event_id)
                          ) %>%
                          as.tibble() %>%
                          select(-document.id)), by = 'document.id') %>%
  select(-document.id, -array.index)
Sys.time() - start

data %>%
  str()
  enter_object(statuses)
  gather_array()
