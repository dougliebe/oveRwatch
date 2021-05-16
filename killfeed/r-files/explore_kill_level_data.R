
head(player_level_vars,20) %>%
  data.frame()

### filter only kills in high impact [-1 to +1] man adv

player_level_vars %>%
  filter(abs(player_situation_pre_kill) < 1) %>%
  

player_level_vars %>%
  add_count(round_id, round_map, game_id, tf_id_unique, player_id, team_id,hero) %>%
  group_by(round_id, round_map, game_id, tf_id_unique, player_id, team_id) %>%
  summarise(won_tf = mean(player_team_won_teamfight),
            total_kills_in_teamfight = mean(kills_in_teamfight),
            player_first_blood = sum(got_first_blood),
            player_first_death = sum(got_first_death),
            player_kills_in_tf = sum(got_kill),
            player_deaths_in_tf = sum(got_dead),
            player_high_impact_deaths = sum((abs(player_situation_pre_kill) <= 1)*got_dead),
            player_high_impact_deaths = sum((abs(player_situation_pre_kill) <= 1)*got_kill),
            hero = hero[n == max(n)][1],
            max_frame = max(frame),
            min_frame = min(frame)) %>%
  group_by(round_id, round_map, game_id, tf_id_unique) %>%
  mutate(length_tf = max(max_frame)-min(min_frame)) %>%
  select(-min_frame, -max_frame) 

## convert to team-level fight data
player_level_vars %>%
  group_by(round_id, round_map, game_id, tf_id_unique, team_id) %>%
  summarise(
    won_tf = mean(player_team_won_teamfight),
    team_got_fk = sum(got_first_blood),
    team_got_sk = ifelse(kills_in_teamfight[1] >1, sum(got_sec_blood),NA),
    team_kills_in_tf = sum(got_kill),
    team_deaths_in_tf = sum(got_dead),
    first_kill_hero = death_hero[kill_no_in_tf == 1][1],
    second_kill_hero = death_hero[kill_no_in_tf == 2][1],
    comp = paste0(unique(hero)[order(unique(hero))], collapse = ",")
  ) %>%
  group_by(round_id, round_map, game_id, tf_id_unique) %>%
  mutate(opp_comp = ifelse(!is.na(lag(comp)), lag(comp), lead(comp))) %>%
  write_csv("D:/Downloads/test_teamcomps.csv")
  # head() %>% data.frame()
  
## Look at only lost teamfights vs only won teamfights

# things we care about in lost teaemfights:
# - how often did you die first?
# - hero you died to?
# - average kill no in tf

player_level_vars %>%
  filter(player_team_won_teamfight == 0) %>%
  add_count(round_id, round_map, game_id, tf_id_unique, player_id, team_id,hero) %>%
  group_by(round_id, round_map, game_id, tf_id_unique, player_id, team_id) %>%
  summarise(deaths = sum(got_dead),
            first_deaths = sum(got_first_death),
            got_sec_death = sum(got_sec_death),
            first_kill_no = team_kill_no_in_tf[got_dead==1][1],
            hero = hero[n == max(n)][1]
            ) %>%
  group_by(player_id, hero) %>%
  summarise(dies_in_loss = sum(deaths == 0, na.rm = T)/n(),
            avg_kill_no = mean(first_kill_no,na.rm = T),
            n = n()) %>%
  arrange(desc(n))
  # tail(13) %>% data.frame()
  
player_level_vars %>%
  filter(got_dead == 1 & kill_no_in_tf < 3) %>%
  add_count(round_id, round_map, game_id, tf_id_unique, player_id, team_id,hero) %>%
  group_by(round_id, round_map, game_id, tf_id_unique, player_id, team_id) %>%
  summarise(won_tf = mean(player_team_won_teamfight),
            hero = hero[n == max(n)][1]
  ) %>%
  group_by(player_id, hero) %>%
  summarise(win_pct = mean(won_tf),
            n = n()) %>%
  arrange(desc(n))
tail(13) %>% data.frame()
  
#### Only in teamfight wins


























