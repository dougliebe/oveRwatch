library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = here::here('data','match_data','20210419'),pattern = "payload_playerjoined.*.tsv",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE, nrows = 1)
head(data,1)



#### Handle the info ####

## To look at the data
data$info[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()

data$player %>%
  spread_values(
    battletag = jstring(battletag),
    esports_player_id = jnumber(esports_player_id)
  ) %>%
  as.tibble() %>%
  bind_cols(data$info %>%
              spread_values(
                match_game_id = jstring(esports_ids, match_game_id)
              ) %>%
              as.tibble() %>%
              select(-document.id)) %>%
  bind_cols(esports_match_id = data$esports_match_id,
            time = data$time,
            team_id = data$team_id) %>%
  select(-document.id)
