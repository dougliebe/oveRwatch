library(DBI)
library(odbc)
library(dplyr)

sort(unique(odbcListDrivers()[[1]]))
con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "MySQL ODBC 8.0 Unicode Driver",
                      Server   = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      UID      = 'admin',
                      PWD      = rstudioapi::askForPassword("Database password"),
                      # PWD      = 'guerrillas',
                      Port     = 3306,
                      Database = "overwatch")

hero_guid <- tbl(con, 'hero_guids')
map_guid <- tbl(con, 'map_guids')
stat_guid <- tbl(con, 'stat_guids')
