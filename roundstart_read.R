library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)


## find file names in folder

filenames <- list.files(path = "D:/OW/",pattern = "payload_roundstart.*.tsv$",full.names = T)
data <- read.delim(file = filenames[1], sep = '\t', header = TRUE)
head(data,1)



#### Handle the info ####

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
