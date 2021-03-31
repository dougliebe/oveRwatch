###
### Add raw files to bigquery to allow for future work and free up space
library(odbc)
library(DBI)
library(tidyverse)
library(dbplyr)
library(bigrquery)
billing <- "civil-ripple-305517"
con <- DBI::dbConnect(
  # driver
  bigrquery::bigquery(),
  # projects are server names in aws
  project = "civil-ripple-305517",
  # datasets are databases in aws 
  dataset = "overwatch",
  billing = billing
)


## Load data
log_data <-  tibble() 
filenames <- list.files(path = here::here("data", "scrim_txt"),
                        full.names = T, recursive = T)
filenames <- filenames[!str_detect( filenames, 'done')]
# short_filenames <- list.files(path = here::here("data", "scrim_txt"),
#                         full.names = F,
#                         pattern = "*.txt")
for( file in 1:length(filenames)) {
  # temp_ <- 
  #   mutate(time = str_match(time, "^\\[(.*?)\\]")[,2]) %>%
  #   separate(time, into = c('hr','min','s')) %>%
  #   mutate(time_s = as.numeric(hr)*3600+as.numeric(min)*60+as.numeric(s)) %>%
  #   select(-hr, -min,-s)
  game_id = paste0(unlist(str_extract_all(filenames[file], "\\d")), collapse = "")
  teams <-
    str_match_all(filenames[file], "-(.*)-(.*)/")
  date <- str_extract(filenames[file], "(20\\d\\d)-(\\d\\d)-(\\d\\d)") %>%
    lubridate::as_date()
  team_1 <- teams[[1]][1,2] %>% tolower()
  team_2 <- teams[[1]][1,3] %>% tolower()
  
  game_info_ <- tibble(date, game_id)
  
  log_ <- read_delim(filenames[file],
                     delim = "\n",
                     col_names = F,
                     locale = locale(encoding = "utf-8")
  ) %>%
    mutate(team1 = team_1,
           team2 = team_2,
           date = date,
           game_id = game_id)
  
  log_data <- bind_rows(log_data, log_)
  
  
}

print(paste0("unpacked ", length(filenames), " files"))

bq_table_upload(x = bq_table("civil-ripple-305517",'overwatch','STATS'))




