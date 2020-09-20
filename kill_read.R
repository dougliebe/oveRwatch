library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = "D:/OW/",pattern = "payload_kill.*.tsv$",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
head(data,1)



#### Handle the info ####

## To look at the data
data$info[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()

## Create table with 1 line for kill,
## will add more lines for recent damagers
data %>%
  select(time,killed_player_hero_guid,death_yaw,killer_yaw,killed_pet) %>%
  bind_cols(data$killed_player_id %>%
              spread_values(
                killed_player_id = jnumber(seq)
              ) %>%
              as.tibble() %>%
              select(-document.id),
            data$final_blow_player_id %>%
              spread_values(
                final_blow_player_id = jnumber(seq)
              ) %>%
              as.tibble() %>%
              select(-document.id),
            data$death_position  %>%
              spread_values(
                death_position.x = jnumber(x),
                death_position.y = jnumber(y),
                death_position.z = jnumber(z)
              ) %>%
              as.tibble() %>%
              select(-document.id),
            data$killer_position  %>%
              spread_values(
                killer_position.x = jnumber(x),
                killer_position.y = jnumber(y),
                killer_position.z = jnumber(z)
              ) %>%
              as.tibble() %>%
              select(-document.id)) %>%
  bind_cols(data$info %>%
              spread_values(
                match_game_id = jstring(esports_ids, match_game_id),
                event_id = jnumber(event_id)
              ) %>%
              as.tibble() %>%
              select(-document.id)) %>%
  head
