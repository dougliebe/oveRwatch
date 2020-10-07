

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
