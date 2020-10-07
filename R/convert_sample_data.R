library(tidyverse)


## Load data
data <- data.frame() 
filenames <- list.files(path = here::here("killfeed"),full.names = T, pattern = "*.csv")
for( file in filenames) {
  temp <- read_csv(file) %>%
    janitor::clean_names() 
  data <- bind_rows(data, temp)
}
tail(data)

## need to make chart of when games start within rounds
game_stats <- data %>%
  # filter(event == "Swap") %>%
  group_by(round_id, frame) %>%
  summarise(n = sum(fight == 0)) %>%
  ungroup() %>%
  mutate(game_start = ifelse(n > 7, 1, 0),
         game_id = cumsum(game_start),
         # make game ids completely unique
         game_id = as.character(openssl::sha1(paste0(game_id, round_id)))) %>%
  select(round_id, frame, game_id)

data <- data %>%
  left_join(game_stats, by = c("round_id", 'frame'))
 
# players should be the same in each round_id and
# will have at least one hero switch record
rosters <- data %>%
  filter(!is.na(player), event == "Swap") %>%
  group_by(game_id, player, colour) %>%
  count() %>%
  select(-n,game_id, player_id = player, 
         team_id = colour)


# Check for misspelled names
# each round_id should have exactly 12 unique names recorded
bad_rounds <- rosters %>%
  # filter((player))
  group_by(round_id) %>%
  summarise(n = length(unique(player_id,incomparables = NA))) %>%
  filter(n != 12) %>%
  select(round_id) %>% unlist()

rosters %>%
  group_by(player_id,round_id, game_id) %>%
  summarise(teams = length(unique(team_id))) %>%
  filter(teams != 1) %>%
  group_by(round_id)  %>%
             count()

## Looking into those that need recoding
data %>%
  filter(round_id %in% 509) %>%
  group_by(player) %>%
  count() %>%
  data.frame()

# set any recoding names that are problematic
data <- data %>%
  mutate(player = recode(player, 
                         `ADMIRALRAP` = "ADMIRALRAPTR", 
                         `XVESTOLAY` = "XVESTOLA", 
                         `AKELLEX` = "KELLEX", 
                         `SAUNIA` = "SAUNA", 
                         `RERELLEX` = "KELLEX", 
                         `IXAO` = "TXAO", 
                         `SAUNIA` = "SAUNA",
                         `AFOXXI` = "AFOXX",
                         `HADL` = "HADI",
                         `RPA` = "RIPA",
                         `SKAIRIPA` = "RIPA",
                         `SIRMAJEDJ` = "SIRMAJED",
                         `JSIRMAJED` = "SIRMAJED",
                         `SKAIIPA` = "RIPA",
                         `STARBOI` = "SSTARBOI",
                         `VAZILITY` = "VIZILITY"))

## Get game-level data for matching game to map
game_level_data <- data %>%
  rename(blue_game_map = team1round) %>%
  group_by(game_id, blue_game_map) %>%
  count() %>% select(-n)

## separate kill data 
kill <- data %>%
  filter(event == "Kill") %>%
  select(frame, 
         game_id,
         kill_hero = hero,
         kill_team = colour,
         kill_player = player,
         tf_id = fight,
         death_hero = against) %>%
  filter(kill_player != "") # filter out non hero kills like de-meching/suicide

## separate death data
death <- data %>%
  filter(event == "Death") %>%
  mutate(kill_team = ifelse(colour=="Red","Blue","Red")) %>%
  select(frame,
         game_id,
         kill_team,
         death_team = colour,
         death_hero = hero,
         death_player = player) %>%
  filter(death_player != "") # filter out non hero deaths like de-meching/suicide

# Join death info onto kill info so each row is a single kill
by_kill <- kill %>%
  left_join(death, by = c('frame','game_id', 'death_hero', "kill_team")) %>%
  # janitor::get_dupes(frame,round_id, death_hero, death_team) %>% data.frame()
  group_by( game_id, tf_id) %>%
  mutate(tf_id_unique = cur_group_id()) %>%
  ungroup() %>%
  select(-tf_id)

# get time table of heros and their switches
# all rounds ids have uniformly increasing frames
hero_switch <- data %>%
  filter(event == "Swap") %>%
  select(game_id, switch_time = frame, player_id = player, hero) %>%
  group_by(game_id, player_id) %>%
  mutate(next_switch =replace_na(lead(switch_time),Inf)) 


###

teams_of_12 <- rosters %>%
  filter(!is.na(player_id)) %>%
  group_by(player_id) %>%
  mutate(n = n()) %>%
  group_by(game_id) %>% # adds a little safety that the players with the most records
  arrange(desc(n)) %>%    # actually get recorded
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
  mutate(across(.cols = starts_with("p"), .fn = as.character))

## Now we are adding variables that describe each kill row
add_vars <- by_kill %>%
  group_by(game_id, tf_id_unique) %>% # group by both so additional data with same ids won't repeat
  mutate(kills_in_teamfight = n(),
         kill_no_in_tf = 1:n(),
         team_A_kills = sum(kill_team == "Red"),
         team_B_kills = sum(kill_team == "Blue"),
         teamfight_winner = case_when(
           team_A_kills > team_B_kills ~ "Red",
           team_A_kills < team_B_kills ~ "Blue",
           TRUE ~ 'draw'
         ),
         team_kill_won = (teamfight_winner == kill_team)*1,
         team_A_alive = 6 - cumsum(kill_team == "Blue"),
         team_B_alive = 6 - cumsum(kill_team == "Red"),
         team_A_alive_pre = ifelse(kill_team == "Blue", team_A_alive + 1, team_A_alive),
         team_B_alive_pre = ifelse(kill_team == "Red", team_B_alive + 1, team_B_alive),
         kill_team_alive = ifelse(kill_team == "Red", team_A_alive, team_B_alive),
         death_team_alive = ifelse(kill_team == "Red", team_B_alive, team_A_alive),
         kill_team_alive_pre = ifelse(kill_team == "Red", team_A_alive_pre, team_B_alive_pre),
         death_team_alive_pre = ifelse(kill_team == "Red", team_B_alive_pre, team_A_alive_pre),
         situation_pre_kill = ifelse(kill_team == "Red",
                                     team_A_alive_pre-team_B_alive_pre, 
                                     team_B_alive_pre-team_A_alive_pre)) %>%
  mutate(first_blood = ifelse(1:n() == 1, 1, 0 ),
         second_kill = ifelse(1:n() == 2, 1, 0)) %>%
  group_by(.add = T, kill_team) %>%
  mutate(team_kill_no_in_tf = 1:n())

## Add the 12 most often recorded players from round_id
## Determine if they have been previously killed
## Dead player should appear on line in which he dies
add_12_players <- add_vars %>%
  left_join(teams_of_12, by = c( 'game_id')) %>%
  #split players out onto their own rows
  pivot_longer(cols = starts_with("p"), names_to = 'player',
               values_to = 'player_id') %>%
  group_by(game_id, tf_id_unique, player) %>% 
  # has this player been the dead player in this tf already?
  mutate(is_dead = cumany(player_id == death_player)) %>%
  left_join(rosters %>% select(game_id, player_id, team_id),
            by = c('game_id', 'player_id')) %>%
    group_by(game_id, tf_id_unique)

##Now we add variables specific to each player on each kill
## players can be dead, killer, or just existing
player_level_vars <- add_12_players %>%
  mutate(on_killing_team = (team_id == kill_team)*1,
         got_kill = (player_id == kill_player)*1,
         got_dead = (player_id == death_player)*1,
         got_first_blood = (got_kill == 1 & first_blood == 1)*1,
         got_first_death = (got_dead == 1 & first_blood == 1)*1,
         got_sec_blood = (got_kill == 1 & second_kill == 1)*1,
         got_sec_death = (got_dead == 1 & second_kill == 1)*1,
         player_team_won_teamfight = (team_id == teamfight_winner)*1,
         player_team_alive = ifelse(kill_team==team_id, kill_team_alive_pre, death_team_alive_pre),
         player_situation_pre_kill = ifelse(kill_team==team_id, situation_pre_kill , -situation_pre_kill )
         ) %>%
  ## add which hero each player is based on last switch in round
  mutate(dummy = T) %>%
  left_join(hero_switch %>%
              mutate(dummy = T),
            by = c('dummy','game_id', "player_id")) %>%
  filter(frame >= switch_time, frame < next_switch) %>%
  select(-dummy, -switch_time, -next_switch) %>%
  select(-kill_hero, -kill_team, -kill_player,
         -death_team, -death_player, -player, 
         -team_A_kills, -team_B_kills,
         -teamfight_winner, -team_kill_won, -team_A_alive, -team_B_alive, 
         -team_A_alive_pre, -team_B_alive_pre, 
         -kill_team_alive, -death_team_alive,
         -kill_team_alive_pre, -death_team_alive_pre,
         -situation_pre_kill,
         -first_blood) #remove unneeded vars

# add_impact_added <- player_level_vars %>%
#   mutate(impact = case_when(
#     player_situation_pre_kill <= -3 ~ 3,
#     player_situation_pre_kill == -2 ~ 6,
#     player_situation_pre_kill == -1 ~ 29,
#     player_situation_pre_kill == 0 ~ 35,
#     player_situation_pre_kill == 1 ~ 13,
#     player_situation_pre_kill == 2 ~ 5,
#     player_situation_pre_kill == 3 ~ 3,
#     player_situation_pre_kill > 3 ~ 3
#   )) %>%
#   mutate(impact = case_when(
#                       on_killing_team == 1 ~ impact,
#                       got_dead == 1 ~ -impact,
#                       TRUE ~ 0
#                     ),
#          ind_impact = ifelse(got_kill == 1 | got_dead == 1, impact, 0))

# add_impact <- add_impact_added %>%
  # group_by(player_id, player_team_won_teamfight) %>%
  # summarise(m = sum(impact, na.rm = T)/length(unique(tf_id_unique)),
  #           n = length(unique(tf_id_unique))) %>%
  # arrange(desc(m))

player_level_vars %>%
  group_by(game_id, tf_id_unique) %>%
  summarise(s = sum(got_first_blood)) %>% arrange(desc(s))

for_sheets %>%
  ggplot(aes(player_situation_pre_kill))+geom_histogram()

player_level_vars %>%
  write_csv("D:/Downloads/kills_player_level.csv")
  # head(12) %>% data.frame

### Create Teamfight-level data to blend with kill-level data
tf_level_vars <- add_impact_added %>%
  add_count(game_id, tf_id_unique, player_id, team_id,hero) %>%
  group_by(game_id, tf_id_unique, player_id, team_id) %>%
  summarise(won_tf = mean(player_team_won_teamfight),
            total_kills_in_teamfight = mean(kills_in_teamfight),
            player_first_blood = sum(got_first_blood),
            player_first_death = sum(got_first_death),
            player_kills_in_tf = sum(got_kill),
            player_deaths_in_tf = sum(got_dead),
            player_death_impact = sum((got_dead == 1)*impact),
            player_kill_impact = sum((impact>=0)*impact),
            player_high_impact_deaths = sum((abs(player_situation_pre_kill) <= 1)*got_dead),
            player_high_impact_deaths = sum((abs(player_situation_pre_kill) <= 1)*got_kill),
            hero = hero[n == max(n)][1],
            max_frame = max(frame),
            min_frame = min(frame),
            impact = sum(impact)) %>%
  group_by(game_id, tf_id_unique) %>%
  mutate(length_tf = max(max_frame)-min(min_frame)) %>%
  select(-min_frame, -max_frame) 

tf_level_vars %>%
  write_csv("D:/Downloads/tf_days1_3.csv")


