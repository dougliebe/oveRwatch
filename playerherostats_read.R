library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = "D:/OW/",pattern = "payload_playerherostats.*.tsv$",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE, nrows = 1000 )
head(data,1)



#### Handle the info ####

## To look at the data
data$info[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()

start <- Sys.time()
data$player %>%
  spread_values(
    player_id = jnumber(player_id, seq)
    # battletag = jstring(battletag),
    # esports_player_id = jnumber(esports_player_id)
  ) %>%
  left_join(data$info %>%
              spread_values(
                event_id = jnumber(event_id),
                match_game_id = jstring(esports_ids, match_game_id)
              ), by = 'document.id') %>%
  left_join(data$stat %>%
              spread_values(
                short_stat_guid = jnumber(short_stat_guid),
                amount = jnumber(amount)
              ) %>%
              as.tibble(), by = 'document.id') %>%
  as.tibble() %>%
  select(-document.id) %>%
  bind_cols(time = data$time,
            hero_guid = data$hero_guid,
            stat_lifespan = data$stat_lifespan)
Sys.time() - start