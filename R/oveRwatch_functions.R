# library(tidyverse)
# library(tidyjson)
# library(jsonlite)
# library(purrr)

options(scipen=999)
`%>%` <- dplyr::`%>%`
jstring <- tidyjson::jstring
jnumber <- tidyjson::jnumber
jlogical <- tidyjson::jlogical
gather_array <- tidyjson::gather_array
spread_values <- tidyjson::spread_values
left_join <- dplyr::left_join
select <- dplyr::select
distinct <- dplyr::distinct
bind_cols <- dplyr::bind_cols
as_tibble <- tibble::as_tibble

read_GameRosters <- function(path) {
  filenames <- list.files(path = path,pattern = "payload_gameresult.*.tsv$",full.names = T)
  data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
  ## player info
  data$players %>%
    gather_array() %>%
    spread_values(
      # team_id = jstring(team, team_id),
      esports_team_id = jnumber(team, esports_team_id),
      # battletag = jstring(base_player,battletag),
      esports_player_id = jnumber(base_player,esports_player_id)
    ) %>%
    as.tibble() %>%
    left_join(data$info %>%
                   spread_values(
                  match_game_id = jstring(esports_ids, match_game_id),
                  instance_src = jnumber(instance_id, src),
                  instance_seq = jnumber(instance_id, seq)
                  # match_id = jstring(esports_ids, match_id)
                ) %>%
                  as.tibble(), by = 'document.id'
    ) %>%
    select(-document.id,-array.index) %>%
    mutate(instance_src = as.character(instance_src),
           instance_seq = as.character(instance_seq))
  
}

get_GameData <- function(path) {
  start_filenames <- list.files(path = path,pattern = "payload_gamestart.*.tsv$",full.names = T)
  start_data <- read.table(file = start_filenames[1],
                           sep = '\t',
                           header = TRUE,
                           colClasses = c(NA, NA, NA, NA, NA, 'character',NA))
  end_filenames <- list.files(path = path,pattern = "payload_gameresult.*.tsv$",full.names = T)
  end_data <- read.table(file = end_filenames[1], sep = '\t', header = TRUE)
  
  start <- start_data$info %>%
    spread_values(
      # event_id = jnumber(event_id),
      instance_src = jnumber(instance_id, src),
      instance_seq = jnumber(instance_id, seq),
      match_game_id = jstring(esports_ids, match_game_id),
      match_id = jstring(esports_ids, match_id),
      # map_guid = jstring(game_context, map_guid),
      map_type = jstring(game_context, map_type),
      # game_mode_guid = jnumber(game_context, game_mode_guid),
      esports_match_game_number = jnumber(esports_ids, esports_match_game_number)
    ) %>%
    bind_cols(start_data %>%
                mutate(map_guid = as.integer(str_sub(map_guid, -4,-1))) %>%
                       select(start_time = time ,
                              map_guid)
    ) %>%
    tibble::as.tibble() %>%
    select(-document.id) %>%
    distinct() %>%
    dplyr::arrange(match_game_id, start_time) %>%
    mutate(instance_src = as.character(instance_src),
           instance_seq = as.character(instance_seq))
  
  end <- end_data$info %>%
    spread_values(
      match_game_id = jstring(esports_ids, match_game_id),
      instance_src = jnumber(instance_id, src),
      instance_seq = jnumber(instance_id, seq)
    ) %>%
    bind_cols(end_data %>%
                       select(end_time = time,
                       end_reason,
                       winningteamid)
    ) %>%
    as.tibble() %>%
    select(-document.id) %>%
    mutate(instance_src = as.character(instance_src),
           instance_seq = as.character(instance_seq))
  
  start %>%
    # dplyr::bind_cols(end)
    left_join(end, by = c("match_game_id", "instance_src", "instance_seq"))
}

get_MatchData <- function(path) {
  filenames <- list.files(path = path,pattern = "payload_instancestart.*.tsv$",full.names = T)
  data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
  
  base <- data$info %>%
    # spread_all() %>%
    spread_values(
      esports_match_id = jnumber(esports_ids,esports_match_id),
      match_id = jstring(esports_ids, match_id),
      esports_tournament_handle = jstring(esports_ids, esports_tournament_handle),
      # instance_seq = jnumber(instance_id, seq),
      esports_tournament_id = jnumber(esports_ids, esports_tournament_id)
    ) %>%
    as_tibble() 
  
  string <- data$info %>%
    enter_object('esports_ids','esports_tournament_attributes') %>%
    append_values_string()
  
  attributes <- string$string %>%
    tidyjson::as.tbl_json() %>%
    # spread_all
    spread_values(
      environment = jstring( environment),
      phase = jstring( phase),
      season_id = jstring( season_id),
      type = jstring( type),
      format = jstring( stage, format)
    ) %>%
    as_tibble()
  
  ## Match data
  base %>%
    left_join(attributes, by = 'document.id') %>%
    select(match_id,
           # instance_seq,
           esports_match_id,
           esports_tournament_id,
           esports_tournament_handle, 
           environment, phase, season_id, type, format) %>%
    distinct()
}

get_PlayerIDs <- function(path) {
  filenames <- list.files(path = path,pattern = "payload_gamestart.*.tsv$",full.names = T)
  data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
  
  data$players %>%
    gather_array() %>%
    spread_values(
      player_id = jnumber(base_player,player_id, seq),
      battletag = jstring(base_player, battletag),
      esports_player_id = jnumber(base_player, esports_player_id)
    ) %>%
    as.tibble() %>%
    select(-document.id, -array.index) %>%
    distinct(player_id, esports_player_id, .keep_all = T)
}

get_TeamIDs <- function(path) {
  ## Doesn't have the actual team names, will need to update this
  filenames <- list.files(path = path,pattern = "payload_gamestart.*.tsv$",full.names = T)
  data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
  
  data$players %>%
    gather_array() %>%
    spread_values(
      team_id = jstring(team,team_id),
      esports_team_id = jnumber(team, esports_team_id)
    ) %>%
    as.tibble() %>%
    select(-document.id, -array.index) %>%
    distinct()
}

## Use to read both files that provide score info
## gameinfo and gameresult
read_ScoreInfo <- function(data) {
  data$score_info %>%
    spread_values(
      round_num = jnumber(round_num),
      # round_name_guid = jstring(round_name_guid),
      attacking_team_id = jstring(attacking_team_id)
    ) %>%
    enter_object(team_info) %>%
    gather_array() %>%
    spread_values(
      team_id = jstring(team_id),
      payload_distance = jnumber(payload_distance),
      time_banked = jnumber(time_banked),
      score = jnumber(score),
      control_point_percent = jnumber(control_info,control_point_percent),
      holds_point = jstring(control_info, holds_point)
    ) %>%
    tibble::as_tibble() %>%
    left_join(data$info %>%
                spread_values(
                  event_id = jnumber(event_id),
                  match_game_id = jstring(esports_ids, match_game_id),
                  instance_src = jnumber(instance_id, src),
                  instance_seq = jnumber(instance_id, seq)
                ) %>%
                as.tibble() %>%
                bind_cols(time = data$time,
                          schema_name = data$schema_name), by = 'document.id'
    ) %>%
    select(-document.id, -array.index) %>%
    mutate(instance_src = as.character(instance_src),
           instance_seq = as.character(instance_seq))
}

get_ScoreInfo <- function(path) {
  filenames <- list.files(path = path,pattern = "payload_gameinfo.*.tsv$",full.names = T)
  data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
  filenames <- list.files(path = path,pattern = "payload_gameresult.*.tsv$",full.names = T)
  data_results <- read.table(file = filenames[1], sep = '\t', header = TRUE)
  
  read_ScoreInfo(data) %>% 
    dplyr::bind_rows(read_ScoreInfo(data_results))
  
}

get_HeroSwitches <- function(path) {
  filenames <- list.files(path = path,pattern = "payload_heroswitch.*.tsv$",full.names = T)
  data <- read.table(file = filenames[1], sep = '\t', header = TRUE,
                     colClasses = c(rep(NA, 5), 'character','character',NA))
  
  data$player %>%
    spread_values(
      esports_player_id = jnumber(esports_player_id)
    ) %>%
    as.tibble() %>%
    bind_cols(data$info %>%
                spread_values(
                  match_game_id = jstring(esports_ids, match_game_id),
                  event_id = jnumber(event_id),
                  instance_src = jnumber(instance_id, src),
                  instance_seq = jnumber(instance_id, seq)
                ) %>%
                as.tibble() %>%
                select(-document.id)) %>%
    bind_cols(time = data$time,
              old_hero_guid = as.integer(str_sub(data$old_hero_guid,-4,-1)),
              new_hero_guid = as.integer(str_sub(data$new_hero_guid,-4,-1))) %>%
    select(-document.id) %>%
    mutate(instance_src = as.character(instance_src),
           instance_seq = as.character(instance_seq))
  
}

get_KillEvents <- function(path) {
  filenames <- list.files(path = path,pattern = "payload_kill.*.tsv$",full.names = T)
  data <- read.table(file = filenames[1], sep = '\t', header = TRUE, nrows = 100)
  
  data %>%
    mutate(killed_player_hero_guid = as.numeric(str_sub(killed_player_hero_guid,-4,-1))) %>%
    select(time,killed_player_hero_guid,death_yaw,killer_yaw,killed_pet) %>%
    bind_cols(data$killed_player_id %>%
                spread_values(
                  killed_player_id = jnumber(seq)
                ) %>%
                as.tibble() %>%
                select(-document.id),
              data$final_blow_player_id %>%
                spread_values(
                  final_blow_player_id = jnumber(seq)
                ) %>%
                as.tibble() %>%
                select(-document.id),
              data$death_position  %>%
                spread_values(
                  death_position.x = jnumber(x),
                  death_position.y = jnumber(y),
                  death_position.z = jnumber(z)
                ) %>%
                as.tibble() %>%
                select(-document.id),
              data$killer_position  %>%
                spread_values(
                  killer_position.x = jnumber(x),
                  killer_position.y = jnumber(y),
                  killer_position.z = jnumber(z)
                ) %>%
                as.tibble() %>%
                select(-document.id)) %>%
    bind_cols(data$info %>%
                spread_values(
                  event_id = jnumber(event_id),
                  instance_src = jnumber(instance_id, src),
                  instance_seq = jnumber(instance_id, seq)
                ) %>%
                as.tibble() %>%
                select(-document.id))
}

get_PlayerStatuses <- function(path) {
  filenames <- list.files(path = path,pattern = "payload_playerstatus.*.tsv$",full.names = T)
  data <- read.table(file = filenames[1], sep = '\t', header = TRUE) #should take ~4 minutes
  
  data$statuses %>%
    gather_array() %>%
    # spread_all %>%
    spread_values(
      player_id = jnumber(player_id, seq),
      team_id= jstring(team_id),
      # hero_guid= jnumber(hero_guid),
      health= jnumber(health),
      armor= jnumber(armor),
      shields= jnumber(shields),
      max_health= jnumber(max_health),
      max_armor= jnumber(max_armor),
      max_shields= jnumber(max_shields),
      respawn_time_remaining= jnumber(respawn_time_remaining),
      ultimate_percent= jnumber(ultimate_percent),
      respawn_progress= jnumber(respawn_progress),
      is_dead= jlogical(is_dead),
      is_ultimate_ready= jlogical(is_ultimate_ready),
      yaw = jnumber(yaw),
      position.x = jnumber(position, x),
      position.y = jnumber(position, y),
      position.z = jnumber(position, z)
    ) %>%
    as.tibble() %>%
    left_join(data %>%
                mutate(document.id = 1:n()) %>%
                select(document.id, time) %>%
                bind_cols(data$info %>%
                            spread_values(
                              match_game_id = jstring(esports_ids, match_game_id),
                              event_id = jnumber(event_id)
                            ) %>%
                            as.tibble() %>%
                            select(-document.id)), by = 'document.id') %>%
    select(-document.id, -array.index) %>%
    mutate(player_id = as.character(player_id))
}

get_TeamStats <- function(path) {
  filenames <- list.files(path = path,pattern = "payload_teamstats.*.tsv$",full.names = T)
  data <- read.table(file = filenames[1], sep = '\t', header = TRUE)
  
  data %>%
    select(time) %>%
    bind_cols(
      data$team %>%
        spread_values(
          esports_team_id = jnumber(esports_team_id)
        ) %>%
        as.tibble() %>%
        left_join(data$stat %>%
                    spread_values(
                      short_stat_guid = jnumber(short_stat_guid),
                      amount = jnumber(amount)
                    ) %>%
                    as.tibble(), by = 'document.id') %>%
        left_join(data$info %>%
                    spread_values(event_id = jnumber(event_id),
                                  match_game_id = jstring(esports_ids, match_game_id),
                                  instance_src = jnumber(instance_id, src),
                                  instance_seq = jnumber(instance_id, seq)) %>%
                    as.tibble(), by = "document.id") %>%
        select(-document.id)
    ) %>%
    as.tibble() %>%
    mutate(instance_src = as.character(instance_src),
           instance_seq = as.character(instance_seq))
}

get_PlayerHeroStats <- function(path, target) {
  filenames <- list.files(path = path,pattern = "payload_playerherostats[0-9].*",full.names = T)
  file_names <- list.files(path = path,pattern = "payload_playerherostats[0-9].*",full.names = F)
  for(i in 1:length(filenames)) {
    data <- read.table(file = filenames[i], sep = '\t', header = ifelse(i == 1, TRUE, FALSE),numerals = "no.loss",
                       colClasses = c(rep(NA, 3), "NULL", rep(NA, 9))
                       ) 
    colnames(data) <- c("time_c","schema_name","time","player_id",    
                  "hero_guid","stat_lifespan","stat","team_id",         
                  "context","player","team","esports_match_id")
    
    out <- data$player %>%
      spread_values(
        player_id = jnumber(player_id, seq)
        # battletag = jstring(battletag),
        # esports_player_id = jnumber(esports_player_id)
      ) %>%
      # left_join(data$info %>%
      #             spread_values(
      #               event_id = jnumber(event_id),
      #               # match_game_id = jstring(esports_ids, match_game_id),
      #               instance_src = jnumber(instance_id, src),
      #               instance_seq = jnumber(instance_id, seq)
      #             ), by = 'document.id') %>%
      left_join(data$stat %>%
                  spread_values(
                    short_stat_guid = jnumber(short_stat_guid),
                    amount = jnumber(amount)
                  ) %>%
                  as.tibble(), by = 'document.id') %>%
      as.tibble() %>%
      select(-document.id) %>%
      bind_cols(time = data$time,
                hero_guid = as.integer(str_sub(data$hero_guid,-4,-1)),
                stat_lifespan = data$stat_lifespan) %>%
      mutate(player_id = as.character(player_id))
    
    write_delim(out , path = paste0(path,target,"/",file_names[i],".txt"), delim = ",",col_names = ifelse(i == 1, TRUE, FALSE))
  }
}



