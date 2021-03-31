#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(odbc)
library(DBI)
library(tidyverse)
library(dbplyr)
library(bigrquery)
library(DT)
library(gt)
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
stats_q <- tbl(con, "STATS")
tf_stats_q <- tbl(con, "TF_STATS")
games_q <- tbl(con, "GAMES")

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
        stats_q %>%
            # filter(player_hero == "Zarya") %>%
            filter(player_hero == !!input$hero_filter) %>%
            group_by(player_alias, player_hero) %>%
            collect() %>%
            summarise(time = hms::as_hms(sum(hero_time_played)),
                      fb10 = sum(parse_number(final_blows))/sum(hero_time_played)*600,
                      death10 = sum(parse_number(deaths))/sum(hero_time_played)*600,
                      dmg10 = sum(parse_number(hero_damage_dealt))/sum(hero_time_played)*600) %>%
            filter(time > input$time_filter*60) %>%
            arrange(desc(time)) ->
            df_stats
        
        tf_stats_q %>%
            # filter(player_hero == "Zarya") %>%
            filter(player_hero == !!input$hero_filter) %>%
            collect() %>%
            left_join(winpct, by = 'adv') %>%
            group_by(game_id, tf_no, player_alias, player_hero) %>%
            summarise(win = any(tf_win),
                      ev_kills = sum(player_kill & adv %in% c(-1,0,1)),
                      ev_deaths = sum(player_death & adv %in% c(-1,0,1)),
                      wpa = sum(wpa*player_kill)-sum(wpa*player_death)) %>%
            group_by(player_alias, player_hero) %>%
            summarise(tfs = n(),
                      win_pct = mean(win),
                      ev_kills = sum(ev_kills),
                      ev_deaths = sum(ev_deaths),
                      wpa_all = mean(wpa, na.rm = T),
                      # wpa_wins = mean(ifelse(win, wpa, NA), na.rm = T)
            ) ->
            tf_df
        df <- df_stats %>%
            left_join(tf_df, by = c("player_alias", 'player_hero')) %>%
            mutate(high_leverage_death_pct = ev_deaths/tfs,
                   high_leverage_kill_pct = ev_kills/tfs,
            ) %>%
            select(player_alias, player_hero, time, tfs, win_pct, wpa_all,fb10, death10, dmg10)
        df %>%
            filter(!is.na(tfs)) %>%
            # summary()
            datatable ->
            dt
        # Loop through the columns formatting according to that column's distribution
        for (j in c(5:9)) {
            # Create breaks for shading column values high to low
            nums <- df[[j]][!is.na(df[[j]])]
            brks <- stats::quantile(x <- nums, probs = seq(.05, .95, .05), na.rm = TRUE)
            # Create shades of green for backgrounds
            # y <- round(seq(255, 40, length.out = length(brks) + 1), 0)
            clrs <- colorRampPalette(c("red",'white', "green"))(20)
            # Format cells in j-th column
            dt <- DT::formatStyle(dt, j, backgroundColor = DT::styleInterval(brks, clrs))
        }
        dt <- dt %>%
            formatRound(columns = 6:9) %>%
            formatPercentage(5)
        
        return(list(dt = dt))
    })
    
    output$table <- renderDataTable({
       table()$dt
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
