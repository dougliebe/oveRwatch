library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = here::here('data','match_data','20210419'),pattern = "payload_gamestart.*.tsv",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
head(data,1)



#### Handle the info ####

## To look at the data
data$info[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()

data$player[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()


data$info %>%
  spread_values(
    match_game_id = jstring(esports_ids, match_game_id)
  ) %>%
  bind_cols(data %>%
              select(time_c, time,schema_name)
  ) %>%
  as.tibble() %>%
  select(-document.id) %>%
  group_by(instance_id) %>%
  count()

data$players %>%
  gather_array() %>%
  spread_values(
    player_id = jnumber(base_player,player_id, seq),
    battletag = jstring(base_player, battletag),
    esports_player_id = jnumber(base_player, esports_player_id)
  ) %>%
  as.tibble() %>%
  select(-document.id, -array.index)
