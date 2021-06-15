library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = here::here('data','match_data','20210419'),pattern = "payload_roundstart.*.tsv",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE, nrows = 100)
slice(data,4:7)



#### Handle the info ####
## EVENT handling

## Event rows
data %>%
  select(time, schema_name, time_c, esports_match_id) %>%
  # # need keys for info only
  bind_cols(
    data$info %>%
      spread_values(event_id = jnumber(event_id),
                    match_game_id = jstring(esports_ids, match_game_id),
                    round = jstring(game_context, round)) %>%
      select(event_id,match_game_id, round) %>% as_tibble
  ) %>%
  group_by(match_game_id) %>%
  mutate(schema_event_id = 1:n()) ->
  roundstart_events

## To look at the data
data$info[1] %>%
  as.tbl_json() %>%
  # str()
  # unnest()
  jsonlite::prettify()
data$info %>%
  enter_object(game_context, map_guid) %>%
  json_types() %>%
  append_values_number() %>%
  # spread_values(
  #   map_guid = jnumber(game_context,map_guid)
  # ) %>%
  as_tibble() %>%
  data.frame() %>%
  str()

bind_rows(do(data$info, fromJSON))

fromJSON(data$info[1], bigint_as_char = T)$game_context$map_guid

data_json <- purrr::map(data$info, jsonlite::fromJSON, bigint_as_char = T, flatten = T)

data_json[[2]]$game_context$map_guid
purrr::map_df(data_json, ~as.data.frame(t(.)))
  
data_json <- purrr::map_df(data$info, function(x) { 
  purrr::map(jsonlite::fromJSON(x, bigint_as_char=T), function(y) ifelse(is.null(y), NA, y)) 
})
data_json$game_context %>%
  unlist()
