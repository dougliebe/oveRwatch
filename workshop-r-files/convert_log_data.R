library(tidyverse)
library(tidyjson)


## Load data
data <- game_info <- roster_data <- team_game <-  tibble() 
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
  team_1 <- teams[[1]][1,2]
  team_2 <- teams[[1]][1,3]
  
  game_info_ <- tibble(date, game_id)
  
  log_ <- read_delim(filenames[file],
                     delim = "\n",
                     col_names = F,
                     locale = locale(encoding = "utf-8")
                     ) %>%
    separate(X1, into=c("time", 'event', 'time_s','log'), sep = ",", extra = "merge", fill = 'right') %>%
    mutate(time = hms::as_hms(str_remove(time, "\\[")),
           time_s = as.numeric(time_s),
           game_id = game_id)
  
  ## need to label team 1 / 2 even if they have different names
  log_ %>%
    filter(event == "match_start") %>%
    separate(log,
             into = c('map', 'mode', "1", "2"),
             sep = ",") %>%
    select(game_id, `1`, `2`) %>%
    pivot_longer(c(`1`,`2`), names_to = 'team_num', values_to = 'team_in_game_name') %>%
    mutate(team_name = tolower(c(team_1, team_2))) ->
    team_game_
  
  log_ %>%
    filter(event == "hero_spawn") %>%
    separate(log,
             into = c("player_team", 'player_name'),
             sep = ",",
             extra = 'drop')  %>%
    mutate(player_name = tolower(player_name)) %>%
    left_join(team_game_, by = c('game_id', 'player_team' = 'team_in_game_name')) %>%
    select(game_id, team_name, player_name) ->
    roster_data_
  
  
  
  
  team_game <- bind_rows(team_game, team_game_)
  data <- bind_rows(data, log_)
  game_info <- bind_rows(game_info, game_info_)
  roster_data <- bind_rows(roster_data, roster_data_)
  

  }

print(paste0("unpacked ", length(filenames), " files"))



## label teamfight start and end times
# consider min. tf time = 15s
# tf win = more kills

all_times <-
  data %>%
  filter(event == 'kill') %>%
  mutate(time = as.numeric(time)) %>%
  group_by(game_id) %>%
  expand(game_id, time = (min(time)-15):(max(time)+15))

## define threshold dmg
# thresh = 0
# data %>%
#   filter(event == 'damage') %>%
#   mutate(time = as.numeric(time),
#          time = hms::as_hms(time)) %>%
#   # replace_na(list(kill = F)) %>%
#   separate(log, into = c(
#     'team_1','player_id','player_hero',"team_2",'opp_id',
#     "opp_hero", 'dmg_type', 'dmg', 'log'
#   ), extra = 'merge', fill = 'right', sep = ',') %>%
#   right_join(all_times, by = c('game_id', "time")) %>%
#   arrange(game_id, time) %>%
#   group_by(game_id, time) %>%
#   summarise(n = sum(as.numeric(dmg))) %>%
#   replace_na(list(n = 0)) %>%
#   left_join(KILLS %>% mutate(time = as.numeric(time), kill = T)%>%
#               select(game_id, time, kill), by = c('game_id', 'time')) %>%
#   group_by(game_id, time, n) %>%
#   summarise(kill = sum(kill,na.rm = T)) %>%
#   mutate(kill = replace(kill, kill == 0, NA)) %>%
#   group_by(game_id) %>%
#   mutate(last_5_dmg = zoo::rollapply(n,
#                                       width = 1,
#                                       FUN = sum,
#                                       partial = TRUE,
#                                       align = "left"),
#          last_5_time = 1,
#          dps_5 = last_5_dmg/last_5_time,
#          delta_dps = pmin(1000,pmax((dps_5-lag(dps_5, default = 0))/1, -1000))
#   ) %>%
#   filter(time < 9*60) %>%
#   ## to figure out where tfs happen
#   ## filter out until its clear starts and ends
#   # mutate(potential_start = dps_5 > thresh & lag(dps_5, default = NA) < thresh ,
#   #        potential_end = dps_5 < thresh & lag(dps_5, default = NA) > thresh ) %>%
#   # filter(potential_start | potential_end)
#   # filter(!no_tf, tf_no > 0) %>%
#   # group_by(game_id, tf_no) %>%
#   # summarise(start_time = min(time),
#   #           end_time = max(time), duration = end_time - start_time) %>%
#   # # ggplot(aes(duration))+geom_histogram()
#   ggplot()+
#   geom_line(aes(time, dps_5))+
#   geom_point(aes(time, kill*20), color = 'red')+
#   # geom_point(aes(time, potential_start*50), color = 'red')+
#   # geom_point(aes(time, potential_end*50), color = 'blue')+
# # geom_hline(yintercept = 100)+
# facet_wrap(~game_id, scales = 'free')


data %>%
  filter(event == 'kill') %>%
  mutate(time = as.numeric(time),
         kill=TRUE) %>%
  # left_join(kills_only %>% mutate(time = as.numeric(time), kill = T)%>%
  #             select(game_id, time, kill), by = c('game_id', 'time')) %>%
  # replace_na(list(kill = F)) %>%
  # separate(log, into = c(
  #   'team_1','player_id','player_hero',"team_2",'opp_id',
  #   "opp_hero", 'dmg_type', 'dmg', 'log'
  # ), extra = 'merge', fill = 'right', sep = ',') %>%
  right_join(all_times, by = c('game_id',"time")) %>%
  arrange(game_id, time) %>%
  group_by(game_id, time) %>%
  summarise(kill = any(kill)) %>%
  replace_na(list(kill = FALSE)) %>%
  group_by(game_id) %>%
  mutate(last_5_dmg = zoo::rollapply(kill,
                                     width = 15,
                                     FUN = sum,
                                     partial = TRUE,
                                     align = "right"),
         last_5_time = 15,
         dps_5 = last_5_dmg/last_5_time,
         # delta_dps = pmin(0,pmax((dps_5-lag(dps_5, default = 0))/10, -1000)),
         tf_no = cumsum(dps_5 == 0)*(dps_5 > 0)
  ) %>%
  # ggplot(aes(time, dps_5))+geom_line()
  ## to figure out where tfs happen
  ## filter out until its clear starts and ends
  # mutate(potential_start = dps_5 > thresh & lag(dps_5, default = NA) < thresh ,
  #        potential_end = dps_5 < thresh & lag(dps_5, default = NA) > thresh ) %>%
  # filter(potential_start | potential_end)
  filter(tf_no > 0) %>%
  group_by(game_id, tf_no) %>%
  mutate(tf_length = n(),
         kills = sum(kill)) %>%
  mutate(tf_no = cur_group_id()) %>%
  mutate(tf_no = ifelse(tf_length < 16 | kills < 3, -10, tf_no))  %>%
  select(game_id, time, tf_no, kill, tf_length, kills) %>%
  filter(tf_no > 0) ->
  teamfight_index
# 
# teamfight_index %>%
#   group_by(game_id, tf_no) %>%
#   mutate(tf_no = cur_group_id()) %>%
#   mutate(tf_no = ifelse(tf_length < 16 | kills < 3, -10, tf_no))  %>%
#   # filter(game_id == "20210222182652") %>%
#   # left_join(data %>%
#   #             filter(event == 'kill') %>%
#   #             mutate(time = as.numeric(time),
#   #                    kill=TRUE),
#   #           by = c("game_id", 'time'))
#   ggplot() +
#   geom_line(aes(time, tf_no)) +
#   geom_col(aes(time, kill*tf_no), color = 'red', width = 0)+
#   facet_wrap(~game_id)
