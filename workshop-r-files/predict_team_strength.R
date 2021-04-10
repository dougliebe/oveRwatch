## look at stats by date
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

## need team in game names 
team_game %>%
  head()

tf_stats_q %>%
  filter(player_kill | player_death) %>%
  collect() ->
  data
data %>% 
  mutate(adv = ifelse(player_death, -adv, adv)) %>%
  left_join(team_game, by = c("player_team" = "team_in_game_name", 'game_id')) %>%
  select(game_id, tf_no, kill_id, team_name, adv, tf_win) %>%
  filter(!is.na(team_name)) %>%
  arrange(game_id, kill_id) %>%
  group_by(game_id, tf_no, kill_id) %>%
  mutate(opp = ifelse(1:n() == 1, lead(team_name), lag(team_name))) ->
  train_data

winpct <- tibble(adv = seq(-6, 6),
                 wpa = c(0,0,0,6.2,
                         15.3-6.2, 44.1-15.3,
                         50 - 44.1, 
                         79.2-50, 92.1-79.2,
                         95.4-92.1,97.5-95.4,
                         0.6,0))

filter_train <-
  train_data %>%
  left_join(winpct, by = 'adv') %>%
  ungroup() %>%
  select(tf_win, wpa, team_name, opp) %>%
  mutate(tf_win = as.factor(tf_win))

library(tidymodels)
set.seed(1)
splits <- initial_split(filter_train)
train <- training(splits)
test <- testing(splits)
# test %>% 
#   ungroup() %>%
#   count(MAP_ID) %>% 
#   mutate(prop = n/sum(n))

## define a recipe for our model
score_rec <-
  recipe(tf_win ~ wpa+opp, data = train) %>%
  # step_interact(terms=~diff:seconds_left) %>%
  # update_role(GAME_ID, MAP_ID, team, hill, hill_no, start_score, opp_start_score,
  #             new_role = "ID") %>% 
  step_zv(all_predictors())

summary(score_rec)
# lm_mod <- 
#   linear_reg() %>% 
#   set_engine("lm")
glm_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

team_wflow <-
  workflow() %>%
  add_model(glm_mod) %>%
  add_recipe(score_rec)

score_fit <-
  team_wflow %>%
  fit(data = train)

## extract the model or fit 
score_fit %>%
  pull_workflow_fit() %>%
  tidy() %>%
  arrange(desc(estimate))


predict(score_fit, test, type = 'prob')

train_data %>%
  ungroup() %>%
  count(opp) %>%
  mutate(wpa = 50) %>%
  mutate(predict(score_fit, ., type = 'prob')) %>%
  arrange((.pred_TRUE))
