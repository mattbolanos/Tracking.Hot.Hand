#Load in packages
library(tidyverse)
library(readr)
library(nbastatR)
library(data.table)
library(janitor)
library(RPostgres)
library(DBI)

players <- nba_players()
players <- players %>% select(idPlayer, namePlayer)

db <- 'postgres' 

host_db <- [redacted]

db_port <- [redacted]    

db_user <- [redacted]  

db_password <- [redacted]  

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)



shots <- dbGetQuery(con, "SELECT * FROM nba_shots_15_16")

setwd("~/Desktop/Analytics/Analytics/Tracking.Hot.Hand/cleaning.scripts")
#Function for reading, cleaning, uploading csv files from Python
player_streaks_with_distance <- function(a_csv){
 
  tracking_data <- read_csv(a_csv) %>%
    mutate(game_id = substr(game_id, 3, nchar(game_id)),
           game_id = as.double(game_id)) %>%
    subset(select = -c(shot_clock))
  
  game <- as.numeric(tracking_data[1,8])
  
  game_shots <- shots %>% 
    mutate(game_id = as.double(game_id)) %>% 
    filter(game_id == game) %>% 
    clean_names()
  
  full_df <- left_join(tracking_data, game_shots, by = c("game_id", "event_id"))
  
  right_time <- filter(full_df, is.na(is_shot_made) != T) %>%
    left_join(players, by = c("player_id" = "idPlayer")) %>%
    mutate(name = case_when(
      player_id == "-1" ~ "Ball",
      TRUE ~ namePlayer),
      namePlayer = case_when(player_id == "-1" ~ "Ball",
                             TRUE ~ namePlayer),
      x_loc = case_when(
        x_loc > 47 ~ 94-x_loc,
        TRUE ~ x_loc),
      shot_game_clock = (minutes_remaining*60) + seconds_remaining,
      shooter = name_player) %>% 
    subset(select = -c(name_player, name)) %>% 
    as.data.frame() %>% 
    clean_names()
  
  
  ball_locs <- right_time %>% 
    filter(name_player == "Ball") %>% 
    mutate(ball_x_loc = x_loc, 
           ball_y_loc = y_loc
    ) %>% 
    subset(select = -c(x_loc,y_loc)) %>%
    group_by(game_id, event_id, game_clock)%>%
    slice(n()) %>% 
    select(event_id, game_clock, game_id, ball_x_loc, ball_y_loc) %>% 
    as.data.frame()
  
  right_time <- right_time %>% 
    left_join(ball_locs, by = c("game_id", "event_id", "game_clock")) %>% 
    mutate(dist_to_ball = sqrt(((ball_x_loc - x_loc)^2)+((ball_y_loc - y_loc)^2)))
  
  
  fixed_shot_times <- right_time %>% 
    mutate(
      shooter_id = player_id,
      shooter_team_id = team_id,
      shooter_team_name = name_team) %>%
    subset(select = -c(player_id,team_id,name_team)) %>% 
    filter(shooter == name_player & dist_to_ball <= 1.75 & game_clock > shot_game_clock) %>% 
    group_by(game_id, event_id) %>% 
    slice_min(game_clock, n = 1) %>% 
    group_by(game_id, event_id, game_clock)%>%
    slice(n()) %>% 
    mutate(shooter_x_loc = x_loc,
           shooter_y_loc = y_loc) %>% 
    select(shooter, shooter_team_name, shooter_team_id, game_clock,shot_game_clock, game_id, event_id, 
           shooter_x_loc, shooter_y_loc, quarter, minutes_remaining, seconds_remaining,
           type_event, type_action, type_shot, zone_basic, name_zone, 
           zone_range, distance_shot, is_shot_attempted, is_shot_made,
           ball_x_loc, ball_y_loc,streak_col)
  
  player_locs <- right_time %>% 
    select(name_player,team_id, x_loc, y_loc, game_id, event_id, game_clock, quarter) %>% 
    filter(name_player != "Ball")
  
  shots_with_defenders <- left_join(fixed_shot_times, player_locs, by = c("game_id", "event_id", "game_clock", "quarter")) %>% 
    filter(shooter != name_player) %>% 
    mutate(distance_to_shooter = sqrt(((shooter_x_loc - x_loc)^2)+
                                        (shooter_y_loc - y_loc)^2),
           distance_to_shooter = case_when(shooter_team_id == team_id ~ 300,
                                           TRUE ~ distance_to_shooter)) %>% 
    group_by(event_id, game_id) %>% 
    slice_min(distance_to_shooter, n = 1) %>% 
    group_by(event_id, game_id) %>% 
    slice(n()) %>% 
    mutate(closest_defender_distance_bin = case_when(
      distance_to_shooter >= 6 ~ "Wide Open",
      between(distance_to_shooter, 4, 6) ~ "Open",
      between(distance_to_shooter, 2, 4) ~ "Tight",
      between(distance_to_shooter, 0, 2) ~ "Very Tight",
    )) %>% 
    select(game_id,event_id,streak_col,shooter,shooter_team_name,game_clock,
           type_action,type_shot,zone_basic,name_zone,zone_range,distance_shot,
           is_shot_attempted,is_shot_made,name_player,distance_to_shooter,closest_defender_distance_bin)
  
  
  
  shots_with_defenders[c("streak_col")][is.na(shots_with_defenders[c("streak_col")])] <- "0"
  
  if (nrow(game_shots) - nrow(shots_with_defenders) < 30){
    dbWriteTable(con, "hhand_shots_tracking", shots_with_defenders, row.names=FALSE, append=TRUE)
    
  }
  
  
  
  
  file.remove(a_csv)

  
}


game_files <-list.files(pattern = ".csv", full.names = FALSE, ignore.case = TRUE)

lapply(game_files, FUN = player_streaks_with_distance)



