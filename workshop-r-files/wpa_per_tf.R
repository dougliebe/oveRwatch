## trying to get a picture of tfs based on heros in comps, 
# not players specifically
# should probably break down by map and side too
source(here::here('workshop-r-files',"convert_log_data.R"))
source(here::here('workshop-r-files',"txt_get_state_adv.R"))
head(teamfight_index)

library(googlesheets4)
aliases_q <- read_sheet("1WuctfnFjLekCh3SX4kpv9TS9WD07hsVgVEI98ywMaok",sheet = 'aliases',col_types = 'cccc') %>%
  mutate(player_alias = tolower(player_alias),
         player_name = tolower(player_name))

## get kills
data %>%
  filter(event == "kill") %>%
  group_by(game_id) %>%
  mutate(kill_id = 1:n(),
         time = as.numeric(time)) %>%
  separate(log, into = c(
    'Attacker Team',
    'Attacker Name',
    'Attacker Hero',
    'Victim Team',
    'Victim Name',
    'Victim Hero',
    'Event Ability',
    'Event Damage',	
    'Is Critical Hit',
    'Is Environmental'
  ), sep = ",", extra = 'drop') %>%
  janitor::clean_names() %>%
  mutate(across(c(attacker_name, victim_name), tolower) ) ->
  KILLS

teamfight_index %>%
  group_by(game_id) %>%
  summarise(tfs = length(unique(tf_no))) ->
  tfs_game_id

KILLS %>%
  left_join(teamfight_index, by = c('game_id', 'time')) %>%
  left_join(state_by_events, by = c('game_id', "kill_id")) %>%
  # filter(state %in% c("6v6","6v5", '5v6')) %>%
  pivot_longer(c(attacker_name, victim_name), 
               values_to = "player",
               names_to = 'type') %>%
  mutate(player_hero = ifelse(type == "attacker_name", attacker_hero, victim_hero)) %>%
  group_by(game_id, type, player, player_hero) %>%
  summarise(n= n(),
            wpa = sum(ifelse(type == "attacker_name", win_prob_added, -win_prob_added))) %>%
  # filter(adv == 0) %>%
  pivot_wider(id_cols = c(game_id, player, player_hero),
              names_from = type, values_from = c(n, wpa)) %>%
  replace_na(list('n_attacker_name'=0, 'n_victim_name'=0,
                  'wpa_attacker_name'=0, 'wpa_victim_name'=0)) %>%
  mutate(pm = n_attacker_name-n_victim_name,
         wpa = wpa_attacker_name + wpa_victim_name) %>% arrange(desc(pm)) %>%
  left_join(tfs_game_id) %>%
  left_join(aliases_q, by = c('player'='player_name_lower')) %>%
  mutate(player = coalesce(player_alias_lower, player)) %>%
  group_by(player, player_hero) %>%
  summarise(wpa = sum(wpa),
            pm = sum(pm),
            kills = sum(n_attacker_name),
            deaths = sum(n_victim_name),
            tfs = sum(tfs)) %>%
  mutate(wptf = wpa / tfs,
         pmtf = pm/tfs) %>%
  arrange((wptf)) ->
  hero_player_wpa

hero_player_wpa %>%
  filter(tfs > 20) %>%
  ggplot() +
  geom_label(aes(x = pmtf, y = wptf, label = player))+
  facet_wrap(~ player_hero, scales = 'free')
