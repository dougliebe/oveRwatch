library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = "D:/OW/",pattern = "*.tsv$",full.names = T)


data <- read.table(file = filenames[1], sep = '\t', header = TRUE, nrows = 2)
head(data,1)

data$info[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()
  
data$info %>%
  # gather_object() %>%
  # json_types() %>%
  # count(name, type)
  # enter_object(esports_ids) %>%
  # gather_object() %>%
  spread_all

# data.info$esports_ids
data$info %>%
  # spread_values(
  #     event_id = jnumber("event_id"),
  #     sequence_length = jnumber("sequence_length"),
  #     sequence_index = jnumber("sequence_index")
  #     ) %>%
  enter_object('esports_ids') %>%
  spread_all

data$info %>%
  enter_object('game_context') %>%
  spread_all %>%
  as.tbl_json()

string <- data$info %>%
  enter_object('esports_ids') %>%
  spread_values(match_id = jstring("match_id")) %>%
  enter_object('esports_tournament_attributes') %>%
  append_values_string()

string$string %>%
  as.tbl_json() %>%
# enter_object("string") %>%
  # spread_all()
  spread_all()
  as.tbl_json()

data %>%
  mutate(info = map(info, fromJSON),
         key = map(info, names),
         value = map(info, simplify)) %>%
  # unnest(c(key,value)) %>%
  data.frame() %>%
  head(1)

paste("[", paste(data$info, collapse = ","), "]") %>% fromJSON() %>%
  map_if(is.data.frame, list) %>% 
  as_tibble() %>%
  unnest(c(instance_id, esports_ids, game_context)) %>%
  map_if(is.data.frame, list) %>% 
  as_tibble() %>%
  str()
  unnest(c(esports_tournament_attributes)) %>%
  data.frame()

data %>%
  rowwise() %>%
  do(data.frame(esports_match_id = .$esports_match_id,
                fromJSON(as.character(.$info), flatten = T))) %>%

game_info <- data %>%
  rowwise() %>%
  do(data.frame(esports_match_id = .$esports_match_id,
                fromJSON(as.character(.$info), flatten = T))) %>%
  rowwise() %>%
  do(data.frame(., fromJSON(as.character(.$esports_ids.esports_tournament_attributes)))) %>%
  select(-esports_ids.esports_tournament_attributes) %>%
  data.frame()
  # write.csv('D:/Downloads/test_gameinfo.csv')

## Table of match_ids and esports_match_ids
game_info %>%
  distinct(esports_match_id, esports_ids.match_id)
## esport_tournament_handle and esports_tournament_ids
game_info %>%
  distinct(esports_ids.esports_tournament_id, esports_ids.esports_tournament_handle)


data %>%
  rowwise() %>%
  do(data.frame(time_c = .$time_c,
                time  = .$time,
                round_name = .$round_name,
                context = .$context,
                esports_match_id = .$esports_match_id,
                fromJSON(as.character(.$score_info)))) %>%
  rowwise() %>%
  do(data.frame(., fromJSON(as.character(.$esports_ids.esports_tournament_attributes)))) %>%
  data.frame()  

