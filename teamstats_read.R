library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = "D:/OW/",pattern = "payload_teamstats.*.tsv$",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
head(data,1)



#### Handle the info ####

## To look at the data
data$stat[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()

data %>%
  select(time_c, time, schema_name, esports_match_id) %>%
  bind_cols(
    data$team %>%
      spread_all %>%
      left_join(data$stat %>% spread_all, by = 'document.id') %>%
      left_join(data$info %>%
                  spread_values(instance_id.seq = jnumber(instance_id, seq),
                                instance_id.src = jnumber(instance_id, src)) %>%
                as.tibble(), by = "document.id") %>%
      select(-document.id)) %>%
  arrange(time)
  group_by(instance_id.seq, instance_id.src, team_id, short_stat_guid) %>%
  count()

start <- Sys.time()
data %>%
  select(time, schema_name) %>%
  bind_cols(
    data$team %>%
      spread_values(
        esports_team_id = jnumber(esports_team_id)
      ) %>%
      as.tibble() %>%
      left_join(data$stat %>%
                  spread_values(
                    short_stat_guid = jnumber(short_stat_guid),
                    amount = jnumber(amount)
                  ) %>%
                  as.tibble(), by = 'document.id') %>%
      left_join(data$info %>%
                  spread_values(match_game_id = jstring(esports_ids, match_game_id),
                                event_id = jnumber(event_id)) %>%
                  as.tibble(), by = "document.id") %>%
      select(-document.id)
    ) %>%
  as.tibble()
Sys.time()-start