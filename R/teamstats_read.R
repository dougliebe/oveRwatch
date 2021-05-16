library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = here::here('data','match_data','20210419'),pattern = "payload_teamstats.*.tsv",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE, nrow = 100)
head(data,2)



#### Handle the info ####

## Event handling
data %>%
  select( time_c,schema_name,time,esports_match_id) %>%
  bind_cols(
    data$stat %>%
      spread_all() %>%
      select(-document.id) %>%
      as_tibble(),
    data$team %>%
      spread_all() %>%
      select(esports_team_id) %>%
      as_tibble(),
    data$info %>%
      spread_values(
        match_game_id = jstring(esports_ids, match_game_id),
        event_id = jnumber(event_id)
      ) %>%
      as_tibble() %>%
      select(-document.id)
  ) %>%
  group_by(match_game_id) %>%
  mutate(schema_event_id = 1:n()) ->
  teamstats_events



## To look at the data
data$info %>%
  as.tbl_json() %>%
  spread_all %>% 
  as_tibble() %>%
  # filter(event_id == 132, sequence_length == 116, sequence_index == 115) %>%
  select(-document.id) %>%
  # distinct() %>%
  # view()
  group_by(event_id, sequence_length, sequence_index, esports_ids.esports_match_id) %>%
  count() %>%
  arrange(desc(n))
  # gather_object %>% json_types %>% count(name, type)
  # spread_values("event_id")
    spread_values(event_id=  jnumber(event_id),
                  esports_match_id = jstring(esports_ids, esports_match_id))
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
      as_tibble() %>%
      left_join(data$stat %>%
                  spread_values(
                    short_stat_guid = jnumber(short_stat_guid),
                    amount = jnumber(amount)
                  ) %>%
                  as_tibble(), by = 'document.id') %>%
      left_join(data$info %>%
                  spread_values(match_game_id = jstring(esports_ids, match_game_id),
                                event_id = jnumber(event_id)) %>%
                  as_tibble(), by = "document.id") %>%
      select(-document.id)
    ) %>%
  as_tibble()
Sys.time()-start