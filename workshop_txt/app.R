#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(odbc)
library(DBI)
library(dbplyr)
library(lubridate)
library(hms)
library(reactable)

con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "overwatch",
                      username    = 'admin',
                      password    = "guerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)

kills_q <- tbl(con, "KILLS")
games_q <- tbl(con, "GAMES")
rounds_q <- tbl(con, "ROUNDS")
stats_q <- tbl(con, "STATS")
game_states_q <- tbl(con, "STATES_TIME")
all_events_q <- tbl(con, "ALL_EVENTS")
map_list <- games_q %>%
    distinct(map) %>%
    pull(map)
roster_q <- tbl(con, "ROSTER")
aliases_q <- tbl(con, "ALIASES")
# 
hero_list <- stats_q %>%
    distinct(player_hero) %>%
    pull(player_hero) %>%
    recode("LC:cio"="Lucio", "TorbjC6rn" = "Torbjorn")

team_list <- roster_q %>%
    distinct(team_name) %>%
    pull(team_name) 
    
    

mode_list <- c("Assault", "Hybrid", "Escort", "Control")

stat_list <- c(
    "Damage/s",
    "Final Blows per 10",
    "Healing/s",
    "Healing:Damage",
    "Elims per 10",
    "Ultimates per 10",
    "Damage, %",
    "FB, %"
)
# source(here::here('workshop_txt',"get_state_by_time.R"))

## add color functions for reactable
green_pallete <- function(x) {
    rgb(colorRamp(c("#ffffff","#41aa4a"))(x), maxColorValue = 255)
}
# create 19 breaks and 20 rgb color values ranging from white to red
# brks <- quantile(df, probs = seq(.05, .95, .05), na.rm = TRUE)
# clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
#     {paste0("rgb(255,", ., ",", ., ")")}
# source(here::here('workshop_txt',"base_funcs.R"))
# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "TEST"),
    dashboardSidebar(
        selectInput(inputId = 'mode_select',
                    label = "Mode",
                    choices = c("Assault", "Hybrid", "Escort", "Control", "ALL"),
                    multiple = F,selected = "ALL"),
        selectInput(inputId = 'map_select',
                    label = "Map",
                    choices = c("ALL",map_list),
                    multiple = F,selected = "ALL") ,
        selectInput(inputId = 'team_select',
                    label = "Team",
                    choices = c("ALL",team_list),
                    multiple = T,selected = "ALL") ,
        selectInput(inputId = 'hero_select',
                    label = "Hero",
                    choices = c("ALL",hero_list),
                    multiple = F,selected = "ALL"),
        selectInput(inputId = "states_select",
                    label = "State",
                    choices = list("All Situations" = 1,
                                   "Both 5 or More" = 3,
                                   "6v6 Only" = 4),
                    multiple = F, selected = 1),
        selectInput(inputId = "stats_select",
                    label = "Choose Columns",
                    choices = stat_list,
                    multiple = T),
        sliderInput(inputId = 'time_slider',
                    label = "Min. Minutes Played:",
                    min = 0,
                    max = 60,
                    value = 10,
                    step = 5
                    ),
        actionButton("button", "GO")
    ),
    dashboardBody(
        # box(textOutput('check')),
        box(width = 12,
            dataTableOutput('output')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    filtered_data <- eventReactive(input$button, {
        # change selections if "ALL"
        if(input$mode_select == "ALL") {
            mode_ <- mode_list
        } else {
            mode_ <- input$mode_select
        }
        if(input$map_select == "ALL") {
            map_ <- map_list
        } else {
            map_ <- input$map_select
        }
        if(input$hero_select == "ALL") {
            hero_ <- hero_list
        } else {
            hero_ <- input$hero_select
        }
        if("ALL" %in% input$team_select) {
            team_ <- team_list
        } else {
            team_ <- input$team_select
        }
        
        games_ <-
            games_q %>%
            filter(mode %in% mode_,
                   map %in% map_) %>%
            collect()
        
        if(input$states_select > 1) {
            
            columns_select <- input$stats_select[input$stats_select %in% c(
                "Damage/s",
                "Final Blows per 10",
                "Healing/s",
                "Healing:Damage",
                # "Elims per 10",
                # "Ultimates per 10",
                "Damage, %",
                "FB, %"
            )]

            all_events_q %>%
                filter(game_id %in% !!games_$game_id) %>%
                group_by(game_id, player_id,player_hero, state, state_change) %>%
                summarise(damage = sum(event_damage, na.rm = T),
                          first_kills = sum(event == "kill", na.rm = T),
                          pack_healing = sum(event_healing*(is_health_pack == "True"), na.rm = T),
                          team_healing = sum(event_healing*(is_health_pack == 0), na.rm = T),
                          duration = mean(duration, na.rm = T)
                ) %>%
                collect() %>%
                left_join(roster_q %>% collect(), by = c("game_id", 'player_id' = 'player_name')) %>%
                left_join(aliases_q %>% collect(), by = c("player_id" = 'player_name')) %>%
                mutate(player_id = coalesce( player_alias, player_id)) %>%
                filter(team_name %in% team_) ->
                base_data
            
            ## filter out situations
            if(input$states_select == 3) {
                correct_filter <-
                    base_data %>%
                    filter(state %in% c("6v6","5v6","6v5"))
            } else {
                correct_filter <-
                    base_data %>%
                    filter(state == "6v6")
            }
            
            total_filter_compare <-
                base_data %>%
                mutate(player_hero = recode(player_hero, "LC:cio" = "Lucio" ,
                                            "TorbjC6rn" = "Torbjorn")) %>%
                replace_na(list(
                    damage = 0, first_kills = 0, pack_healing = 0, team_healing = 0,
                    duration = 0
                )) %>%
                # left_join(aliases_q %>% collect(), by = c("player_id" = 'player_name')) %>%
                # mutate(player_id = coalesce( player_alias, player_id)) %>%
                # filter(state %in% c("6v6")) %>%
                group_by(player_id, player_hero) %>%
                summarise(
                    all_damage = sum(damage),
                    all_first_kills= sum(first_kills),
                    # all_healing = sum(pack_healing)+sum(team_healing),
                    # all_duration = sum(duration),
                    # games = length(unique(game_id))
                ) 
                
            filter_data <-
                correct_filter %>%
                mutate(player_hero = recode(player_hero, "LC:cio" = "Lucio" ,
                                            "TorbjC6rn" = "Torbjorn")) %>%
                replace_na(list(
                    damage = 0, first_kills = 0, pack_healing = 0, team_healing = 0,
                    duration = 0
                )) %>%
                # left_join(aliases_q %>% collect(), by = c("player_id" = 'player_name')) %>%
                # mutate(player_id = coalesce( player_alias, player_id)) %>%
                # filter(state %in% c("6v6")) %>%
                group_by(player_id, player_hero) %>%
                summarise(
                    damage = sum(damage),
                    first_kills= sum(first_kills),
                    pack_healing = sum(pack_healing),
                    team_healing = sum(team_healing),
                    duration = sum(duration),
                    # games = length(unique(game_id))
                ) %>%
                left_join(total_filter_compare, by = c("player_id",
                                                       "player_hero")) %>%
                mutate(
                    dps = round(damage/duration,2),
                    fbp10 = round(first_kills/duration*600,2),
                    hps = round((pack_healing+team_healing)/duration,2),
                    # pack_healing_pct = pack_healing/(pack_healing+team_healing),
                    healing_to_damage_ratio = round(hps/dps,2),
                    dmg_pct = scales::percent(damage/all_damage, accuracy = 0.1),
                    fb_pct = scales::percent(first_kills/all_first_kills, accuracy = 0.1)
                ) %>%
                # select(-damage, -first_kills, -pack_healing, -team_healing) %>%
                mutate(duration = hms::hms(duration),
                       duration = hour(duration)*60 + minute(duration) ) %>%
                filter(duration > input$time_slider,
                       player_hero %in% hero_) %>%
                arrange(desc(duration))
            
            filter_dt <-
                filter_data %>%
                rename(
                    'Player' = player_id,
                    'Hero' = player_hero,
                    'Time' = duration,
                    'Damage/s' = dps,
                    'Final Blows per 10' = fbp10,
                    'Healing/s' = hps,
                    "Damage, %" = dmg_pct,
                    "FB, %" = fb_pct,
                    # 'Pack Heals, %' = pack_healing_pct,
                    'Healing:Damage' = healing_to_damage_ratio,
                ) %>%
                select(Player, Hero, Time, columns_select)
            
        } else {
            columns_select <- input$stats_select[input$stats_select %in% c(
                "Damage/s",
                "Final Blows per 10",
                "Healing/s",
                "Healing:Damage",
                "Elims per 10",
                "Ultimates per 10"
                # "Damage, %",
                # "FB, %"
            )]
            
            filter_data <-
                stats_q %>%
                filter(game_id %in% !!games_$game_id, hero_time_played > 0) %>%
                left_join(aliases_q, c("player_id" = "player_name")) %>%
                collect() %>%
                left_join(roster_q %>% collect(), by = c("game_id", 'player_id' = 'player_name')) %>%
                filter(team_name %in% team_) %>%
                mutate(across(c(eliminations, final_blows, hero_damage_dealt,
                                barrier_damage_dealt, ultimates_earned, hero_time_played,
                                healing_received),
                              as.numeric),
                       player_id = coalesce( player_alias, player_id)) %>%
                mutate(player_hero = recode(player_hero, "LC:cio" = "Lucio" ,
                                            "TorbjC6rn" = "Torbjorn")) %>%
                dplyr::group_by(player_id, player_hero, game_id) %>%
                dplyr::summarise(
                    eliminations = sum(eliminations),
                    final_blows = max(final_blows),
                    hero_damage_dealt = max(hero_damage_dealt),
                    # barrier_damage_dealt = sum(barrier_damage_dealt),
                    ultimates_earned = sum(ultimates_earned),
                    healing = max(healing_received ),
                    duration = round(max(hero_time_played)),
                    # games = length(unique(game_id)),
                    .groups = 'keep'
                ) %>%
                dplyr::group_by(player_id, player_hero) %>%
                dplyr::summarise(
                    eliminations = sum(eliminations),
                    final_blows = sum(final_blows),
                    hero_damage_dealt = sum(hero_damage_dealt),
                    # barrier_damage_dealt = sum(barrier_damage_dealt),
                    ultimates_earned = sum(ultimates_earned),
                    healing = sum(healing ),
                    duration = round(sum(duration)),
                    # games = length(unique(game_id)),
                    .groups = 'keep'
                ) %>%
                mutate(
                    dps = round(hero_damage_dealt/duration, 2),
                    fbp10 = round(final_blows/duration*600,2 ),
                    elims10 = round(eliminations/duration*600, 2),
                    ult10 = round(ultimates_earned/duration*600,2 ),
                    hps = round(healing/duration,2),
                    # pack_healing_pct = pack_healing/(pack_healing+team_healing),
                    healing_to_damage_ratio = round(hps/dps,2)
                ) %>%
                # select(-hero_damage_dealt, -final_blows, -healing) %>%
                mutate(duration = hms::hms(duration),
                       duration = hour(duration)*60 + minute(duration) ) %>%
                filter(duration > input$time_slider,
                       player_hero %in% hero_) %>%
                arrange(desc(duration))
            
            filter_dt <-
                filter_data %>%
                # DT::datatable()
                rename(
                    'Player' = player_id,
                    'Hero' = player_hero,
                    'Time' = duration,
                    'Damage/s' = dps,
                    'Final Blows per 10' = fbp10,
                    'Healing/s' = hps,
                    "Ultimates per 10" = ult10,
                    "Elims per 10" = elims10,
                    # 'Pack Heals, %' = pack_healing_pct,
                    'Healing:Damage' = healing_to_damage_ratio,
                ) %>%
                select(Player, Hero, Time, columns_select)
        }
        
        
        
        
            
            
            
            return(list(
                filter_dt = filter_dt,
                games_ = games_,
                team_ = team_,
                columns_select = columns_select
            ))
        
    })
    
    output$output <- renderDataTable({
        # brks <- quantile(parse_number(filtered_data()$filter_dt[,filtered_data()$columns_select]), probs = seq(.05, .95, .05), na.rm = TRUE)
        # clrs <- green_pallete(seq(.00, .95, .05))
        filtered_data()$filter_dt %>%
            datatable %>%
            formatStyle(
                "Time",
                background = styleColorBar(range(filtered_data()$filter_dt$Time), 'lightblue'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center'
            ) 
        # %>%
        #     formatStyle(
        #         names(filtered_data()$filter_dt[,filtered_data()$columns_select]),
        #         backgroundColor = styleInterval(brks, clrs)
        #     )
    })
    
    # output$check <- renderText({
    #     filtered_data()$team_
    # })

}

onStop(function() {
    dbDisconnect(con)
})

# Run the application 
shinyApp(ui = ui, server = server)
