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
library(DT)
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
team_game_q <- tbl(con, 'TEAM_GAME')
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
heros_list <- read_delim(here::here('player_compare',"hero_list.txt"), delim = "\n")
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
        sidebarMenu(id = 'sidebarid',
            menuItem(
                "Compare Players",
                tabName = "compare_players"
            ),
            menuItem(
                "Widgets",
                tabName = 'map_performance'
            ),
            conditionalPanel(
                'input.sidebarid == "compare_players"',
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
            actionButton(inputId = "player_button",label =  "Run Query")),
            conditionalPanel(
              'input.sidebarid == "map_performance"',
              checkboxGroupInput(
                inputId = "patch_filter_map",
                label = "Choose Patches",
                selected = patch_list,
                choices = patch_list
              ),
              checkboxGroupInput(
                inputId = "region_filter_map",
                label = "Choose Region",
                choices = region_list,
                selected = region_list
              ),
              selectInput(
                inputId = "map_filter_map",
                label = "Choose Maps (optional)",
                choices = map_list,
                multiple = T
              ),
              actionButton(inputId = "map_button",label =  "Run Query")),
            conditionalPanel(
              'input.sidebarid == "player_time"',
              checkboxGroupInput(
                inputId = "patch_filter_map",
                label = "Choose Patches",
                selected = patch_list,
                choices = patch_list
              ),
              checkboxGroupInput(
                inputId = "region_filter_map",
                label = "Choose Region",
                choices = region_list,
                selected = region_list
              ),
              selectInput(
                inputId = "map_filter_map",
                label = "Choose Maps (optional)",
                choices = map_list,
                multiple = T
              ),
              actionButton(inputId = "map_button",label =  "Run Query"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "compare_players",
                h2("Select 'Run Query' to start"),
                box(
                    fluidRow(
                        DTOutput("table")
                    ),
                    width = 12
                )
            ),
            tabItem(
              tabName = "map_performance",
              h2("Select Map"),
              box(
                fluidRow(
                  plotOutput("map_graph")
                ),
                width = 12,height = 800
              )
            ),
            tabItem(
              tabName = "player_time",
              h2("Select Map"),
              box(
                fluidRow(
                  plotOutput("player_graph")
                ),
                width = 12,height = 800
              )
            )
        )
    )
)
onStop(function() {
  dbDisconnect(con)
  print("Disconnected")
})
server <- function(input, output, session) {
    table <- eventReactive(eventExpr = input$player_button, {
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
        
        ############### HERO COMPARE #########################
        stats_q %>%
            # filter(player_hero == "D.Va") %>%
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
                      # dmg10 = sum(parse_number(damage_blocked))/sum(hero_time_played)*600,
                      ult10 = sum(parse_number(ultimates_earned))/sum(hero_time_played)*600) %>%
          filter(time > input$time_filter*60) %>%
          filter(time > 60*60) %>%
            arrange(desc(time)) ->
            df_stats
        
        tf_stats_q %>%
            # filter(player_hero == "D.Va") %>%
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
    
    ############### MAP COMPARE #########################
    # map_graph <- eventReactive(eventExpr = input$map_button, {
    #     if(length(input$map_filter_map)==0) {
    #         map_filter_ <- map_list
    #     } else {map_filter_ <- input$map_filter_map}
    #     ## find game ids that match
    #     game_ids_filter <- games_q %>%
    #         filter(region %in% input$region_filter_map,
    #                patch %in% input$patch_filter_map, 
    #                map %in% map_filter_) %>%
    #         pull(game_id)
    #     ## Get stats and summarise dmg healing and shields by game
    #     stats_q %>%
    #         filter(game_id %in% !!game_ids_filter) %>%
    #         # filter(game_id == 030220210302153522) %>%
    #         collect() %>%
    #         left_join(aliases_q, by = c('player_id' = 'player_name')) %>%
    #         mutate(player_name = coalesce(player_alias, player_id)) %>%
    #         group_by(game_id, player_name, player_team) %>%
    #         summarise(
    #             dmg_dlt_raw = sum(parse_number(hero_damage_dealt))/sum(hero_time_played)*600,
    #             dmg_tkn_raw = sum(parse_number(damage_taken))/sum(hero_time_played)*600,
    #             shield_blk_raw = sum(parse_number(damage_blocked))/sum(hero_time_played)*600,
    #             shield_dlt_raw = sum(parse_number(barrier_damage_dealt))/sum(hero_time_played)*600,
    #             heal_dlt_raw = (sum(parse_number(healing_dealt)) +
    #                 sum(parse_number(self_healing)))/sum(hero_time_played)*600,
    #             heal_tkn_raw = sum(parse_number(healing_received))/sum(hero_time_played)*600
    #         ) %>%
    #         group_by(game_id) %>%
    #         mutate(
    #             dmg_dlt_game = dmg_dlt_raw/sum(dmg_dlt_raw),
    #             dmg_tkn_game = dmg_tkn_raw/sum(dmg_tkn_raw),
    #             shield_blk_game = shield_blk_raw/sum(shield_blk_raw),
    #             shield_dlt_game = shield_dlt_raw/sum(shield_dlt_raw),
    #             heal_dlt_game = heal_dlt_raw/sum(heal_dlt_raw),
    #             heal_tkn_game = heal_tkn_raw/sum(heal_tkn_raw),
    #             
    #         ) %>%
    #         group_by(game_id, player_team) %>%
    #         mutate(
    #             dmg_dlt_team = dmg_dlt_raw/sum(dmg_dlt_raw),
    #             dmg_tkn_team = dmg_tkn_raw/sum(dmg_tkn_raw),
    #             shield_blk_team = shield_blk_raw/sum(shield_blk_raw),
    #             shield_dlt_team = shield_dlt_raw/sum(shield_dlt_raw),
    #             heal_dlt_team = heal_dlt_raw/sum(heal_dlt_raw),
    #             heal_tkn_team = heal_tkn_raw/sum(heal_tkn_raw)
    #         ) %>%
    #         group_by(game_id) %>%
    #         mutate(game_num = cur_group_id()) %>%
    #         group_by(player_name) %>%
    #         mutate(dmg_dlt_10 = zoo::rollmeanr(dmg_dlt_team, k = 10, fill = NA),
    #                dmg_tkn_10 = zoo::rollmeanr(dmg_tkn_team, k = 10, fill = NA),
    #                shield_blk_10 = zoo::rollmeanr(shield_blk_team, k = 10, fill = NA),
    #                shield_dlt_10 = zoo::rollmeanr(shield_dlt_team, k = 10, fill = NA),
    #                heal_dlt_10 = zoo::rollmeanr(heal_dlt_team, k = 10, fill = NA),
    #                heal_tkn_10 = zoo::rollmeanr(heal_tkn_team, k = 10, fill = NA),
    #                overall_10 = dmg_dlt_raw - dmg_tkn_raw + shield_blk_raw + shield_dlt_raw + heal_dlt_raw
    #         ) %>%
    #         left_join(team_game_q %>%
    #                       filter(game_id %in% !!game_ids_filter) %>%
    #                       collect(),
    #                   by = c("game_id", 'player_team' = 'team_in_game_name')) %>%
    #         ungroup() %>%
    #         # filter(team_name == "gladiators") %>%
    #         filter(player_name %in% c("kev", 'bird','mirror','shu','space','skewed','muze','moth')) %>%
    #         select(-team_num, -player_team) %>%
    #         pivot_longer(c(ends_with("_raw"), ends_with("_game"),
    #                        ends_with("_team"), ends_with("_10")),
    #                      names_to = "stat") ->
    #         core_six
    #     mg <- 
    #         core_six %>%
    #         filter(endsWith(stat, "_10")) %>%
    #         ggplot(aes(game_num, value, color = player_name))+
    #         # geom_point()+
    #         geom_line(size = 1)+
    #         guides(color = guide_legend(override.aes = list(size=5)))+
    #         labs(title = input$map_filter_map)+
    #         facet_wrap(~stat, scales = 'free',ncol = 2)+
    #         theme_bw()+
    #         lims(x = c(10,NA)) +
    #         theme(legend.text = element_text(size = 14))
    #     return(list(
    #         mg = mg
    #     ))
    # })
    
    ############### PLAYER OVER TIME #########################
    
    player_time <- eventReactive(eventExpr = input$player_time_button, {
      
      # player_filter_ <- 
        
      
      tf_stats_q %>%
        # filter(player_hero == !!input$hero_filter) %>%
        # filter(game_id %in% !!game_ids_filter) %>%
        filter(player_name %in% c('gla004','space')) %>%
        collect() %>%
        left_join(aliases_q, by = c('player_name' = 'player_name_lower')) %>%
        mutate(player_name = coalesce(player_alias, player_name)) %>%
        # filter(player_name == "space") %>%
        left_join(winpct, by = 'adv') %>%
        group_by(game_id, tf_no, player_name, player_hero) %>%
        # view()
        summarise(win = any(tf_win==1),
                  ev_kills = sum(player_kill==1 & adv %in% c(-1,0,1)),
                  ev_deaths = sum(player_death==1 & adv %in% c(-1,0,1)),
                  wpa = sum(wpa*player_kill)-sum(wpa*player_death)) %>%
        group_by(player_name, player_hero) %>%
        mutate(tf_num = 1:n(),
               roll_wpa = zoo::rollmeanr(wpa,
                                         k = 100, fill = NA)) %>%
        ggplot(aes(tf_num, roll_wpa, color = player_hero))+
        geom_line()
      
      stats_q %>%
        filter(player_id %in% c('gla004','space')) %>%
        # filter(game_id %in% !!game_ids_filter) %>%
        # filter(game_id == 030220210302153522) %>%
        collect() %>%
        left_join(aliases_q, by = c('player_id' = 'player_name')) %>%
        mutate(player_name = coalesce(player_alias, player_id)) %>%
        # filter(player_name == "space") %>%
        group_by( player_name, player_hero) %>%
        summarise(time_played = sum(hero_time_played)) %>%
        ggplot(aes(area = time_played, fill = player_hero, label = player_hero))+
        treemapify::geom_treemap()+
        treemapify::geom_treemap_text(colour = "white", place = "centre",
                                      grow = TRUE)+
        theme(legend.position = 'none')
      
        stats_q %>%
          filter(player_id %in% c('gla004','space')) %>%
            # filter(game_id %in% !!game_ids_filter) %>%
            # filter(game_id == 030220210302153522) %>%
            collect() %>%
            left_join(aliases_q, by = c('player_id' = 'player_name')) %>%
            mutate(player_name = coalesce(player_alias, player_id)) %>%
            filter(player_name == "space") %>%
            group_by(game_id, player_name, player_hero) %>%
            summarise(
                dmg_dlt_raw = sum(parse_number(hero_damage_dealt))/sum(hero_time_played)*600,
                dmg_tkn_raw = sum(parse_number(damage_taken))/sum(hero_time_played)*600,
                shield_blk_raw = sum(parse_number(damage_blocked))/sum(hero_time_played)*600,
                shield_dlt_raw = sum(parse_number(barrier_damage_dealt))/sum(hero_time_played)*600,
                heal_dlt_raw = (sum(parse_number(healing_dealt)) +
                    sum(parse_number(self_healing)))/sum(hero_time_played)*600,
                heal_tkn_raw = sum(parse_number(healing_received))/sum(hero_time_played)*600,
                overall_10 = dmg_dlt_raw - dmg_tkn_raw + shield_blk_raw + shield_dlt_raw + heal_dlt_raw
            ) %>%
            group_by(player_hero) %>%
            mutate(game_num = 1:n(),
                   roll_overall = zoo::rollmeanr(overall_10, k = 25, fill = NA)) %>%
          ggplot(aes(game_num, roll_overall, color = player_hero))+
          geom_line()
    })
    
    output$table <- renderDataTable({
       table()$dt
    })
    # output$map_graph <- renderPlot(height = 800,{
    #     map_graph()$mg
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
