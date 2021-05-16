library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = here::here('data','match_data','20210419'),pattern = "payload_kill.*.tsv",
                        full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
head(data,1)



#### Handle the info ####

## To look at the data
data$info[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()


### Only need events here
##
### Event Handling
data %>%
  select(time,schema_name, time_c,killed_player_hero_guid,death_yaw,killer_yaw,killed_pet) %>%
  bind_cols(data$killed_player_id %>%
              spread_values(
                killed_player_id = jnumber(seq)
              ) %>%
              as_tibble() %>%
              select(-document.id),
            data$final_blow_player_id %>%
              spread_values(
                final_blow_player_id = jnumber(seq)
              ) %>%
              as_tibble() %>%
              select(-document.id),
            data$death_position  %>%
              spread_values(
                death_position.x = jnumber(x),
                death_position.y = jnumber(y),
                death_position.z = jnumber(z)
              ) %>%
              as_tibble() %>%
              select(-document.id),
            data$killer_position  %>%
              spread_values(
                killer_position.x = jnumber(x),
                killer_position.y = jnumber(y),
                killer_position.z = jnumber(z)
              ) %>%
              as_tibble() %>%
              select(-document.id),
            
        ## need keys for info
            data$info %>%
              spread_values(
                match_game_id = jstring(esports_ids, match_game_id),
                event_id = jnumber(event_id)
              ) %>%
              as_tibble() %>%
              select(-document.id)) %>%
  group_by(match_game_id) %>%
  mutate(schema_event_id = 1:n()) ->
  kill_events
