
source(here::here('workshop-r-files',"data_to_db_files.R"))

library(odbc)
library(DBI)
library(tidyverse)
library(dbplyr)

con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "overwatch",
                      username    = 'admin',
                      password    = "guerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)

## Add new data to the respective tables in OW db
db_insert_into(con, "STATS", STATS)
db_insert_into(con, "GAMES", GAMES)
db_insert_into(con, "ROUNDS", ROUNDS)
db_insert_into(con, "KILLS", KILLS)
db_insert_into(con, "ALL_EVENTS", six_dhk)
db_insert_into(con, "STATES_TIME", state_by_time)
db_insert_into(con, "ROSTER", roster_data)
db_insert_into(con, "ALIASES", roster_data %>% distinct(player_name))
