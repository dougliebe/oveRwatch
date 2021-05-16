library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = here::here('data','match_data','20210419'),pattern = "payload_heroswitch.*.tsv",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE, nrows = 100)
head(data,1)



#### Handle the info ####

## To look at the data
data$player[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()


## EVENT handling

## Event rows
data %>%
  select(time, schema_name, time_c, esports_match_id, old_hero_guid, new_hero_guid) %>%
  # # need keys for info and player
  bind_cols(
    data$info %>%
      spread_values(event_id = jnumber(event_id),
                    match_game_id = jstring(esports_ids, match_game_id)) %>%
      select(event_id,match_game_id) %>% as_tibble,
    data$player %>%
      spread_values(esports_player_id = jnumber(esports_player_id)) %>%
      select(esports_player_id) %>% as_tibble
  ) %>%
  group_by(match_game_id) %>%
  mutate(schema_event_id = 1:n()) ->
  heroswitch_events
