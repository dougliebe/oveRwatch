library(tidyverse)
library(tidyjson)
library(jsonlite)
library(purrr)

### Hero guids
options(scipen=999)
hero_guids <- read_delim(here::here('guids', 'hero_guids.txt'), delim = ",")

## find kill files

filenames <- list.files(path = here::here("data"),pattern = "payload_kill.*.tsv$",full.names = T)
data <- read.table(file = filenames[1], sep = '\t', header = TRUE,
                   colClasses = c(rep(NA, 5), 'character',rep(NA, 8)))
head(data %>% filter(killed_pet == FALSE),5)

# get teams for each player
filenames <- list.files(path = here::here('data'),pattern = "payload_gameresult.*.tsv$",full.names = T)
roster_data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
head(roster_data,1)
## To look at the data
roster_data$players[1] %>%
  as.tbl_json() %>%
  # unnest()
  jsonlite::prettify()


## player info
rosters <- roster_data$players %>%
  gather_array() %>%
  spread_values(
    # team_id = jstring(team, team_id),
    esports_team_id = jnumber(team, esports_team_id),
    battletag = jstring(base_player,battletag),
    player_id = jnumber(base_player,player_id, seq)
  ) %>%
  as.tibble() %>%
  left_join(roster_data$info %>%
              spread_values(
                match_game_id = jstring(esports_ids, match_game_id),
                # instance_src = jnumber(instance_id, src),
                # instance_seq = jnumber(instance_id, seq)
                # match_id = jstring(esports_ids, match_id)
              ) %>%
              as.tibble(), by = 'document.id'
  ) %>%
  select(-document.id,-array.index) %>%
  distinct(match_game_id, player_id,.keep_all = T)

teams_of_12 <- rosters %>%
  mutate(battletag = tolower(battletag)) %>%
  group_by(match_game_id ) %>%
  arrange(esports_team_id) %>%
  summarise(p1 = unique(player_id)[1],
            p2 = unique(player_id)[2],
            p3 = unique(player_id)[3],
            p4 = unique(player_id)[4],
            p5 = unique(player_id)[5],
            p6 = unique(player_id)[6],
            p7 = unique(player_id)[7],
            p8 = unique(player_id)[8],
            p9 = unique(player_id)[9],
            p10 = unique(player_id)[10],
            p11 = unique(player_id)[11],
            p12 = unique(player_id)[12]) %>%
  mutate(across(.cols = starts_with("p"), .fn = as.character)) %>%
  mutate(across(.cols = starts_with("p"), .fn = as.numeric))

#### Handle the info ####


## Create table with 1 line for kill,
## will add more lines for recent damagers
kill_data <- data %>%
  select(time,killed_player_hero_guid, killed_pet) %>%
  mutate(killed_player_hero_guid = as.integer(str_sub(killed_player_hero_guid, -4,-1))) %>%
  bind_cols(data$final_blow_player_id %>%
              spread_values(
                final_blow_player_id = jnumber(seq)
              ) %>%
              as_tibble() %>%
              select(-document.id),
            data$killed_player_id %>%
              spread_values(
                killed_player_id = jnumber(seq)
              ) %>%
              as_tibble() %>%
              select(-document.id)) %>%
  bind_cols(data$info %>%
              spread_values(
                match_game_id = jstring(esports_ids, match_game_id),
                event_id = jnumber(event_id),
                round = jnumber(game_context, round)
              ) %>%
              as.tibble()) %>%
  filter(killed_pet == FALSE) %>%
  arrange(time)

# assists <- data$recent_damagers %>%
#   gather_array() %>%
#   spread_values(
#     assist = jnumber(seq)
#   ) %>%
#   as_tibble() %>%
#   filter(array.index == 2) %>%
#   select(-array.index)

full_data <- kill_data %>%
  # left_join(assists, by = 'document.id') %>%
  left_join(rosters, by = c('match_game_id', 'killed_player_id'= 'player_id')) %>%
  left_join(rosters, by = c('match_game_id', 'final_blow_player_id'= 'player_id'))

##load in roles for heroes
roles <- read_csv(here::here('guids','hero_guids_role.csv'))

raw_data <- full_data %>%
  select(match_game_id, time, final_blow_player_id,killed_player_id, killed_player_hero_guid, round,
         team_kill_id = esports_team_id.y, team_death_id = esports_team_id.x) %>%
  group_by(match_game_id) %>%
  mutate(time_from_last_kill_s = (time - lag(time))/1000,
         team_kill = ifelse(team_kill_id == unique(team_kill_id)[1], "A","B"),
         team_death = ifelse(team_kill == "A", "B", "A")) %>%
  left_join(roles %>% select(2,death_role = 3), by = c(killed_player_hero_guid = 'hero_guid'))

add_variables <- raw_data %>%
  group_by(match_game_id) %>%
  mutate(in_teamfight = ifelse(time_from_last_kill_s <= 15, 1, 0)) %>%
  mutate_at('in_teamfight', ~replace(., is.na(.), 0)) %>%
  mutate(first_blood = ifelse(in_teamfight == 0, 1,0)) %>%
  mutate(tf_id = cumsum(first_blood)) %>%
  group_by(match_game_id, tf_id) %>%
  mutate(kills_in_teamfight = n(),
         team_A_kills = sum(team_kill == "A"),
         team_B_kills = sum(team_kill == "B"),
         teamfight_winner = case_when(
           team_A_kills > team_B_kills ~ "A",
           team_A_kills < team_B_kills ~ "B",
           TRUE ~ 'draw'
         ),
         team_kill_won = (teamfight_winner == team_kill)*1,
         team_A_alive = 6 - cumsum(team_death == "A"),
         team_B_alive = 6 - cumsum(team_death == "B"),
         team_A_alive_pre = ifelse(team_death == "A", team_A_alive + 1, team_A_alive),
         team_B_alive_pre = ifelse(team_death == "B", team_B_alive + 1, team_B_alive),
         kill_team_alive = ifelse(team_kill == "A", team_A_alive, team_B_alive),
         death_team_alive = ifelse(team_kill == "A", team_B_alive, team_A_alive),
         kill_team_alive_pre = ifelse(team_kill == "A", team_A_alive_pre, team_B_alive_pre),
         death_team_alive_pre = ifelse(team_kill == "A", team_B_alive_pre, team_A_alive_pre),
         situation_pre_kill = ifelse(team_kill == "A",
                                     team_A_alive_pre-team_B_alive_pre, 
                                     team_B_alive_pre-team_A_alive_pre)) 
  # head(20) %>% data.frame()

players_alive_added <- add_variables %>%
  left_join(teams_of_12, by = c('match_game_id')) %>%
  mutate(p1 = replace(p1, cumany(p1 == lag(killed_player_id)), NA),
         p2 = replace(p2, cumany(p2 == lag(killed_player_id)), NA),
         p3 = replace(p3, cumany(p3 == lag(killed_player_id)), NA),
         p4 = replace(p4, cumany(p4 == lag(killed_player_id)), NA),
         p5 = replace(p5, cumany(p5 == lag(killed_player_id)), NA),
         p6 = replace(p6, cumany(p6 == lag(killed_player_id)), NA),
         p7 = replace(p7, cumany(p7 == lag(killed_player_id)), NA),
         p8 = replace(p8, cumany(p8 == lag(killed_player_id)), NA),
         p9 = replace(p9, cumany(p9 == lag(killed_player_id)), NA),
         p10 = replace(p10, cumany(p10 == lag(killed_player_id)), NA),
         p11 = replace(p11, cumany(p11 == lag(killed_player_id)), NA),
         p12 = replace(p12, cumany(p12 == lag(killed_player_id)), NA),
  ) %>%
  pivot_longer(cols = starts_with("p"), names_to = 'player',
               values_to = 'player_id') %>%
  # filter(!is.na(player_id)) %>%
  left_join(rosters %>% select(match_game_id, player_id, esports_team_id), by = c('match_game_id', 'player_id'))
  
players_alive_added %>%  
  mutate(on_killing_team = (esports_team_id == team_kill_id)*1,
         got_kill = (player_id == final_blow_player_id)*1,
         got_dead = (player_id == killed_player_id)*1) %>%
  # mutate(p1 = na_if(!cumany(p1 == killed_player_id), p1)) %>%
  # group_by()
  # mutate(p1 = ifelse(any(cumany(killed_player_id %in% p1)),0,p1 )) %>%
  head(10) %>% data.frame()

add_variables %>%
  ungroup() %>%
  # filter(first_blood == 1) %>%
  group_by(situation_pre_kill) %>%
  summarise(win = mean(team_kill_won, na.rm = T),
            n = n()) %>%
  filter(n > 30) %>%
  ungroup() %>%
  mutate(win_gain = -(lag(win)-win),
         win_mult = win_gain*100
         ) %>%
  ggplot(aes(situation_pre_kill, win, label = scales::percent(-(lag(win)-win),accuracy = 0.1)))+
  geom_line()+
  geom_point()+
  geom_label()+
  theme_minimal()
  # facet_wrap(~death_role,nrow = 2)

add_variables %>%
  mutate(sit = paste0(kill_team_alive_pre,"v", death_team_alive_pre)) %>%
  group_by(sit) %>%
  summarise(win = mean(team_kill_won, na.rm = T),
            n = n()) %>%
  filter(n > 20) %>%
  data.frame()

add_variables %>%
  ungroup() %>%
  filter(first_blood == 1) %>%
  group_by(kills_in_teamfight) %>%
  summarise(win = mean(team_kill_won, na.rm = T),
            n = n()) %>%
  filter(n > 30) %>%
  ggplot(aes(kills_in_teamfight, win, label = scales::percent((win),accuracy = 0.1)))+
  geom_line()+
  geom_point()+
  geom_label()+
  theme_minimal()

add_variables %>%
  mutate(time_bins = cut(time_from_last_kill_s, breaks = c(0,2,4,6,8,10,15,20,30,60,500))) %>%
  group_by( time_bins, situation_pre_kill) %>%
  summarise(win = mean(team_kill_won, na.rm =T),
            n = n()) %>%
  filter(n > 30) %>%
  ggplot(aes(situation_pre_kill, win, col=time_bins))+geom_point()+facet_wrap(~time_bins)

add_variables %>%
  mutate(impact_added = case_when(
    situation_pre_kill < -3 ~ 3,
    situation_pre_kill == -2 ~ 6,
    situation_pre_kill == -1 ~ 29,
    situation_pre_kill == 0 ~ 35,
    situation_pre_kill == 1 ~ 13,
    situation_pre_kill == 2 ~ 3,
    situation_pre_kill == 3 ~ 2,
    situation_pre_kill > 3 ~ 1
  )) %>%
  group_by(final_blow_player_id)
  
  
  
  
  
  
  
  
  
  

