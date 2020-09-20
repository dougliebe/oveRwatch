library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)
library(bit64)

options(scipen=999)
filenames <- list.files(path = "D:/OW/guids",pattern = "*.tsv$",full.names = T)
data <- read.delim(file = filenames[1],quote = "",sep = "\t",encoding = "Latin-1",
                   colClasses = c('character','character'))
head(data,1)

data$hero %>%
  # as.tbl_json() %>%
  spread_values(
    hero = jstring(en_US)
  ) %>%
  bind_cols(hero_guid = as.integer(str_sub(data$guid, -4,-1))) %>%
  as.tibble() %>%
  select(-document.id) %>%
  # mutate(guid = as.integer64(guid))
  # mutate(guid = as.numeric(str_sub(guid, -4,-1))) %>%
  write_delim("D:/OW/guids/hero_guids.txt", delim = ",")

filenames <- list.files(path = "D:/OW/guids",pattern = "*.tsv$",full.names = T)
data <- read.delim(file = filenames[2],quote = "",sep = "\t",
                   colClasses = c('character','character'))
head(data,1)

data$map_name %>%
  # as.tbl_json() %>%
  spread_values(
    map_name = jstring(en_US)
  ) %>%
  bind_cols(map_guid = as.integer(str_sub(data$guid, -4,-1))) %>%
  as.tibble() %>%
  select(-document.id) %>%
  write_delim("D:/OW/guids/map_guids.txt", delim = ",")


filenames <- list.files(path = "D:/OW/guids",pattern = "*.tsv$",full.names = T)
data <- read.delim(file = filenames[3],quote = "",sep = "\t",encoding = "Latin-1",
                   colClasses = c('character','character'))
head(data,1)

data$stat_name %>%
  # as.tbl_json() %>%
  spread_values(
    stat_name = jstring(en_us)
  ) %>%
  bind_cols(guid = data$ssg,
            statCategory = data$statcategory) %>%
  as.tibble() %>%
  select(-document.id) %>%
  mutate(guid = as.numeric(guid))  %>%
  write_delim("D:/OW/guids/stat_guids.txt", delim = ",")
