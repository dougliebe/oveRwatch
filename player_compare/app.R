#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(dbplyr)
library(bigrquery)
library(DT)
library(gt)
library(odbc)
library(DBI)
library(googlesheets4)
# designate project-specific cache
# options(gargle_oauth_cache = ".secrets")
# # check the value of the option, if you like
# gargle::gargle_oauth_cache()
# # trigger auth on purpose to store a token in the specified cache
# # a broswer will be opened
# googlesheets4::sheets_auth()
# # see your token file in the cache, if you like
# list.files(".secrets/")
# # sheets reauth with specified token and email address
# sheets_auth(
#     cache = ".secrets",
#     email = "youremail"
# )
# DBI::dbDisconnect(con)
gs4_deauth()
con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "overwatch",
                      username    = 'admin',
                      password    = "guerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)

stats_q <- tbl(con, "STATS")
tf_stats_q <- tbl(con, "TF_STATS")
aliases_q <- read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",
                        sheet = 'aliases',
                        col_types = 'cccc') %>%
    mutate(player_alias = tolower(player_alias),
           player_name = tolower(player_name))
games_q <-  read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",
                       sheet = 'Fixed GameScores') %>% filter(!is.na(game_id))

patch_list <- unique(games_q$patch) 
region_list <- unique(games_q$region)
opponent_list <- unique(games_q$opponent) 
map_list <- unique(games_q$map)
heros_list <- read_delim("hero_list.txt", delim = "\n")
colfunc <- colorRampPalette(c("blue", "deepskyblue"))
winpct <- tibble(adv = seq(-6, 6),
                 wpa = c(0,0,0,6.2,
                   15.3-6.2, 44.1-15.3,
                   50 - 44.1, 
                   79.2-50, 92.1-79.2,
                   95.4-92.1,97.5-95.4,
                   0.6,0))
# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Compare Players"),
    dashboardSidebar(
        selectInput(
            inputId = "hero_filter",
            label = "Choose Hero",
            choices = heros_list %>%
                arrange(Hero),
            selected = "Ana"
        ),
        checkboxGroupInput(
            inputId = "patch_filter",
            label = "Choose Patches",
            selected = patch_list,
            choices = patch_list
        ),
        checkboxGroupInput(
            inputId = "region_filter",
            label = "Choose Region",
            choices = region_list,
            selected = region_list
        ),
        selectInput(
            inputId = "opp_filter",
            label = "Choose Opponents (optional)",
            choices = opponent_list,
            multiple = T
        ),
        selectInput(
            inputId = "map_filter",
            label = "Choose Maps (optional)",
            choices = map_list,
            multiple = T
        ),
        sliderInput(
            inputId = "time_filter",
            label = "Min. Time Played (m)",
            min = 0,
            max = 250,
            value = 0,
            step = 10
        ),
        actionButton("button", "Run Query")
    ),
    dashboardBody(
        box(
            fluidRow(
                DTOutput("table")
            ),
            width = 12
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    table <- eventReactive(eventExpr = input$button, {
        if(length(input$opp_filter)==0) {
            opp_filter_ <- opponent_list
        } else {opp_filter_ <- input$opp_filter}
        if(length(input$map_filter)==0) {
            map_filter_ <- map_list
        } else {map_filter_ <- input$map_filter}
        game_ids_filter <- games_q %>%
            filter(region %in% input$region_filter,
                   patch %in% input$patch_filter, 
                   opponent %in% opp_filter_,
                   map %in% map_filter_) %>%
            pull(game_id)
        
        stats_q %>%
            # filter(player_hero == "Zarya") %>%
            filter(player_hero == !!input$hero_filter) %>%
            filter(game_id %in% !!game_ids_filter) %>%
            collect() %>%
            left_join(aliases_q, by = c('player_id' = 'player_name')) %>%
            mutate(player_name = coalesce(player_alias, player_id)) %>%
            group_by(player_name, player_hero) %>%
            summarise(time = hms::as_hms(sum(hero_time_played)),
                      fb10 = sum(parse_number(final_blows))/sum(hero_time_played)*600,
                      death10 = sum(parse_number(deaths))/sum(hero_time_played)*600,
                      dmg10 = sum(parse_number(hero_damage_dealt))/sum(hero_time_played)*600,
                      ult10 = sum(parse_number(ultimates_earned))/sum(hero_time_played)*600) %>%
            filter(time > input$time_filter*60) %>%
            arrange(desc(time)) ->
            df_stats
        
        tf_stats_q %>%
            # filter(player_hero == "Zarya") %>%
            filter(player_hero == !!input$hero_filter) %>%
            filter(game_id %in% !!game_ids_filter) %>%
            collect() %>%
            left_join(aliases_q, by = c('player_name' = 'player_name_lower')) %>%
            mutate(player_name = coalesce(player_alias, player_name)) %>%
            left_join(winpct, by = 'adv') %>%
            group_by(game_id, tf_no, player_name, player_hero) %>%
            # view()
            summarise(win = any(tf_win==1),
                      ev_kills = sum(player_kill==1 & adv %in% c(-1,0,1)),
                      ev_deaths = sum(player_death==1 & adv %in% c(-1,0,1)),
                      wpa = sum(wpa*player_kill)-sum(wpa*player_death)) %>%
            group_by(player_name, player_hero) %>%
            summarise(tfs = n(),
                      win_pct = mean(win),
                      ev_kills = sum(ev_kills),
                      ev_deaths = sum(ev_deaths),
                      wpa_all = mean(wpa, na.rm = T),
                      # wpa_wins = mean(ifelse(win, wpa, NA), na.rm = T)
            ) ->
            tf_df
        df <- df_stats %>%
            left_join(tf_df, by = c("player_name", 'player_hero')) %>%
            # mutate(high_leverage_death_pct = ev_deaths/tfs,
            #        high_leverage_kill_pct = ev_kills/tfs,
            # ) %>%
            select(player_name, player_hero, time, tfs, win_pct, wpa_all,fb10, death10, dmg10, ult10)
        df %>%
            filter(!is.na(tfs)) %>%
            # summary()
            datatable ->
            dt
        # Loop through the columns formatting according to that column's distribution
        for (j in c(5:10)) {
            # Create breaks for shading column values high to low
            nums <- df[[j]][!is.na(df[[j]])]
            brks <- stats::quantile(x <- nums, probs = seq(.05, .95, .05), na.rm = TRUE)
            # Create shades of green for backgrounds
            # y <- round(seq(255, 40, length.out = length(brks) + 1), 0)
            clrs <- colorRampPalette(c("red",'white', "green"))(20)
            
            if(names(df)[j] == "death10") {clrs <- rev(clrs)}
            
            # Format cells in j-th column
            dt <- DT::formatStyle(dt, j, backgroundColor = DT::styleInterval(brks, clrs))
        }
        dt <- dt %>%
            formatRound(columns = 6:10) %>%
            formatPercentage(5)
        
        return(list(dt = dt))
    })
    
    output$table <- renderDataTable({
       table()$dt
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
