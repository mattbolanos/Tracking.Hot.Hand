library(tidyverse)
library(readr)
library(nbastatR)
library(data.table)
library(janitor)
library(RPostgres)
library(DBI)

# players <- nba_players()
# players <- players %>% select(idPlayer, namePlayer)

db <- 'postgres'

host_db <- 'nba-shots-hot-hand.crmgvw22m9mc.us-east-2.rds.amazonaws.com'

db_port <- '5432'

db_user <- 'mattbolanos'

db_password <- 'General30!'

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)

# shots <- dbGetQuery(con, "SELECT * FROM nba_shots_15_16")
# 
# setwd("~/Desktop/Analytics/Analytics/Tracking.Hot.Hand/cleaning.scripts")
# 
# player_streaks_with_distance <- function(a_csv){
#   
#   tracking_data <- read_csv(a_csv) %>%
#     mutate(game_id = substr(game_id, 3, nchar(game_id)),
#            game_id = as.double(game_id)) %>%
#     subset(select = -c(shot_clock))
#   
#   game <- as.numeric(tracking_data[1,8])
#   
#   game_shots <- shots %>% 
#     mutate(game_id = as.double(game_id)) %>% 
#     filter(game_id == game) %>% 
#     clean_names()
#   
#   full_df <- left_join(tracking_data, game_shots, by = c("game_id", "event_id"))
#   
#   right_time <- filter(full_df, is.na(is_shot_made) != T) %>%
#     left_join(players, by = c("player_id" = "idPlayer")) %>%
#     mutate(name = case_when(
#       player_id == "-1" ~ "Ball",
#       TRUE ~ namePlayer),
#       namePlayer = case_when(player_id == "-1" ~ "Ball",
#                              TRUE ~ namePlayer),
#       x_loc = case_when(
#         x_loc > 47 ~ 94-x_loc,
#         TRUE ~ x_loc),
#       shot_game_clock = (minutes_remaining*60) + seconds_remaining,
#       shooter = name_player) %>% 
#     subset(select = -c(name_player, name)) %>% 
#     as.data.frame() %>% 
#     clean_names()
#   
#   
#   ball_locs <- right_time %>% 
#     filter(name_player == "Ball") %>% 
#     mutate(ball_x_loc = x_loc, 
#            ball_y_loc = y_loc
#     ) %>% 
#     subset(select = -c(x_loc,y_loc)) %>%
#     group_by(game_id, event_id, game_clock)%>%
#     slice(n()) %>% 
#     select(event_id, game_clock, game_id, ball_x_loc, ball_y_loc) %>% 
#     as.data.frame()
#   
#   right_time <- right_time %>% 
#     left_join(ball_locs, by = c("game_id", "event_id", "game_clock")) %>% 
#     mutate(dist_to_ball = sqrt(((ball_x_loc - x_loc)^2)+((ball_y_loc - y_loc)^2)))
#   
#   
#   fixed_shot_times <- right_time %>% 
#     mutate(
#       shooter_id = player_id,
#       shooter_team_id = team_id,
#       shooter_team_name = name_team) %>%
#     subset(select = -c(player_id,team_id,name_team)) %>% 
#     filter(shooter == name_player & dist_to_ball <= 1.75 & game_clock > shot_game_clock) %>% 
#     group_by(game_id, event_id) %>% 
#     slice_min(game_clock, n = 1) %>% 
#     group_by(game_id, event_id, game_clock)%>%
#     slice(n()) %>% 
#     mutate(shooter_x_loc = x_loc,
#            shooter_y_loc = y_loc) %>% 
#     select(shooter, shooter_team_name, shooter_team_id, game_clock,shot_game_clock, game_id, event_id, 
#            shooter_x_loc, shooter_y_loc, quarter, minutes_remaining, seconds_remaining,
#            type_event, type_action, type_shot, zone_basic, name_zone, 
#            zone_range, distance_shot, is_shot_attempted, is_shot_made,
#            ball_x_loc, ball_y_loc,streak_col)
#   
#   player_locs <- right_time %>% 
#     select(name_player,team_id, x_loc, y_loc, game_id, event_id, game_clock, quarter) %>% 
#     filter(name_player != "Ball")
#   
#   shots_with_defenders <- left_join(fixed_shot_times, player_locs, by = c("game_id", "event_id", "game_clock", "quarter")) %>% 
#     filter(shooter != name_player) %>% 
#     mutate(distance_to_shooter = sqrt(((shooter_x_loc - x_loc)^2)+
#                                         (shooter_y_loc - y_loc)^2),
#            distance_to_shooter = case_when(shooter_team_id == team_id ~ 300,
#                                            TRUE ~ distance_to_shooter)) %>% 
#     group_by(event_id, game_id) %>% 
#     slice_min(distance_to_shooter, n = 1) %>% 
#     group_by(event_id, game_id) %>% 
#     slice(n()) %>% 
#     mutate(closest_defender_distance_bin = case_when(
#       distance_to_shooter >= 6 ~ "Wide Open",
#       between(distance_to_shooter, 4, 6) ~ "Open",
#       between(distance_to_shooter, 2, 4) ~ "Tight",
#       between(distance_to_shooter, 0, 2) ~ "Very Tight",
#     )) %>% 
#     select(game_id,event_id,streak_col,shooter,shooter_team_name,game_clock,
#            type_action,type_shot,zone_basic,name_zone,zone_range,distance_shot,
#            is_shot_attempted,is_shot_made,name_player,distance_to_shooter,closest_defender_distance_bin)
#   
#   
#   
#   shots_with_defenders[c("streak_col")][is.na(shots_with_defenders[c("streak_col")])] <- "0"
#   
#   if (nrow(game_shots) - nrow(shots_with_defenders) < 30){
#     dbWriteTable(con, "hhand_shots_tracking", shots_with_defenders, row.names=FALSE, append=TRUE)
#     # write_csv(shots_with_defenders, file = paste0("clean", a_csv))
#   }
#   
#   
#   
#   
#   file.remove(a_csv)
# 
#   
# }
# 
# 
# game_files <-list.files(pattern = ".csv", full.names = FALSE, ignore.case = TRUE)
# 
# lapply(game_files, FUN = player_streaks_with_distance)


# datatable ---------------------------------------------------------------

library(reshape2)

all_shots <- dbGetQuery(con, "SELECT shooter,shooter_team_name, streak_col, type_shot,zone_basic,name_zone,
         zone_range, distance_shot, is_shot_attempted, is_shot_made,
         closest_defender_distance_bin FROM hhand_shots_tracking") 

averages_makes <- all_shots %>% 
  group_by(shooter,streak_col) %>% 
  summarise(fgm = sum(is_shot_made),
            fga = sum(is_shot_attempted),
            threes = sum(type_shot %like% "3PT"),
            base_streak_efg = (fgm+(.5*threes))/fga) %>% 
  filter(streak_col %like% "Make") %>% 
  select(shooter, streak_col, base_streak_efg, fga) %>% 
  pivot_wider(names_from = c("streak_col"),
              values_from = c("base_streak_efg", "fga"),values_fill = 0, names_sep = "_") %>%
  clean_names() %>% 
  select(shooter, base_streak_efg_1_make, base_streak_efg_2_makes,
         base_streak_efg_3_makes)



averages_misses <- all_shots %>% 
  group_by(shooter,streak_col) %>% 
  summarise(fgm = sum(is_shot_made),
            fga = sum(is_shot_attempted),
            threes = sum(type_shot %like% "3PT"),
            base_streak_efg = (fgm+(.5*threes))/fga) %>% 
  filter(streak_col %like% "Miss") %>% 
  select(shooter, streak_col, base_streak_efg, fga) %>% 
  pivot_wider(names_from = c("streak_col"),
              values_from = c("base_streak_efg", "fga"),values_fill = 0, names_sep = "_") %>%
  clean_names() %>% 
  select(shooter, base_streak_efg_1_miss, base_streak_efg_2_misses,
         base_streak_efg_3_misses)


total_fgas <- all_shots %>% 
  mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
                                      closest_defender_distance_bin == "Very Tight" ~ "Tight",
                                      TRUE ~ closest_defender_distance_bin)) %>% 
  group_by(shooter) %>% 
  summarise(total_fga = sum(is_shot_attempted),
            total_tight_attempts = sum(defender_distance =="Tight"),
            percent_fga_tight = total_tight_attempts/total_fga) 


make_summary <- all_shots %>% 
  mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
                                      closest_defender_distance_bin == "Very Tight" ~ "Tight",
                                      TRUE ~ closest_defender_distance_bin)) %>% 
  group_by(shooter, streak_col, defender_distance) %>%
  filter(streak_col %like% "Make")%>% 
  summarise(fgm = sum(is_shot_made),
            fga = sum(is_shot_attempted),
            fg_percent = fgm/fga,
            threes = sum(type_shot %like% "3PT"),
            efg_percent = (fgm+(.5*threes))/fga) %>% 
  ungroup() %>% 
  select(shooter,streak_col, defender_distance, efg_percent,fga) %>%
  pivot_wider(names_from = c("defender_distance","streak_col"),
              values_from = c("efg_percent", "fga"),values_fill = 0, names_sep = "_") %>% 
  left_join(averages_makes, by = c("shooter")) %>% 
  clean_names() %>% 
  left_join(total_fgas, by = c("shooter")) %>% 
  mutate(fga_1_make = fga_open_1_make + fga_tight_1_make,
         fga_2_make = 
         coverage_diff_1_make = efg_percent_tight_1_make - efg_percent_open_1_make,
         coverage_diff_2_makes = efg_percent_tight_2_makes - efg_percent_open_2_makes,
         coverage_diff_3_makes = efg_percent_tight_3_makes - efg_percent_open_3_makes) %>% 
  mutate_if(is.numeric, round,digits=4)%>%
  select(shooter, total_fga, streak_fga,percent_fga_tight, base_streak_efg_1_make, efg_percent_open_1_make,
         efg_percent_tight_1_make, coverage_diff_1_make, base_streak_efg_2_makes, efg_percent_open_2_makes,
         efg_percent_tight_2_makes, coverage_diff_2_makes, base_streak_efg_3_makes, efg_percent_open_3_makes,
         efg_percent_tight_3_makes, coverage_diff_3_makes)



miss_summary <- all_shots %>% 
  mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
                                      closest_defender_distance_bin == "Very Tight" ~ "Tight",
                                      TRUE ~ closest_defender_distance_bin)) %>% 
  group_by(shooter, streak_col, defender_distance) %>%
  filter(streak_col %like% "Miss")%>% 
  summarise(fgm = sum(is_shot_made),
            fga = sum(is_shot_attempted),
            fg_percent = fgm/fga,
            threes = sum(type_shot %like% "3PT"),
            efg_percent = (fgm+(.5*threes))/fga) %>% 
  ungroup() %>% 
  select(shooter,streak_col, defender_distance, efg_percent,fga) %>%
  pivot_wider(names_from = c("defender_distance","streak_col"),
              values_from = c("efg_percent", "fga"),values_fill = 0, names_sep = "_") %>% 
  left_join(averages_misses, by = c("shooter")) %>% 
  clean_names() %>% 
  left_join(total_fgas, by = c("shooter")) %>% 
  mutate(
         coverage_diff_1_miss = efg_percent_tight_1_miss - efg_percent_open_1_miss,
         coverage_diff_2_misses = efg_percent_tight_2_misses - efg_percent_open_2_misses,
         coverage_diff_3_misses = efg_percent_tight_3_misses - efg_percent_open_3_misses) %>% 
  mutate_if(is.numeric, round,digits=4) %>% 
  select(shooter, total_fga, percent_fga_tight, base_streak_efg_1_miss, efg_percent_open_1_miss,
         efg_percent_tight_1_miss, coverage_diff_1_miss, base_streak_efg_2_misses, efg_percent_open_2_misses,
         efg_percent_tight_2_misses, coverage_diff_2_misses, base_streak_efg_3_misses, efg_percent_open_3_misses,
         efg_percent_tight_3_misses, coverage_diff_3_misses)

# colnames(shots_sum) <- sub("fg_percent_", "FG% ", colnames(shots_sum))
# colnames(shots_sum) <- sub("fga_", "FGA ", colnames(shots_sum))
# 
# colnames(shots_sum) <- sub("open_", "Open ", colnames(shots_sum))
# colnames(shots_sum) <- sub("tight_", "Tight ", colnames(shots_sum))
# 
# 
# colnames(shots_sum) <- sub("_make", " Make", colnames(shots_sum))
# colnames(shots_sum) <- sub("_miss", " Miss ", colnames(shots_sum))
# 
# colnames(shots_sum) <- sub("3", "3+", colnames(shots_sum))

teams <- all_shots %>% 
  select(shooter_team_name, shooter) %>% 
  group_by(shooter) %>% 
  slice(n()) %>% 
  ungroup()

colnames(miss_summary)

final_misses <- miss_summary %>% 
  left_join(teams, by = c("shooter")) %>% 
  select(shooter_team_name, colnames(miss_summary))


final_makes <- make_summary %>% 
  left_join(teams, by = c("shooter")) %>% 
  select(shooter_team_name, colnames(make_summary)) %>% 
  ungroup()


new_test <- test %>% 
  left_join(teams, by = c("shooter")) %>% 
  select(shooter_team_name, colnames(test)) %>% 
  ungroup()


test <-  all_shots %>% 
  group_by(shooter,streak_col) %>% 
  mutate(fgm = sum(is_shot_made),
            fga = sum(is_shot_attempted),
            threes = sum(type_shot %like% "3PT"),
            base_streak_efg = (fgm+(.5*threes))/fga) %>% 
  mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
                                      closest_defender_distance_bin == "Very Tight" ~ "Tight",
                                      TRUE ~ closest_defender_distance_bin)) %>% 
  ungroup() %>% 
  group_by(shooter, streak_col, defender_distance) %>%
  filter(streak_col == "1 Make")%>% 
  summarise(fgm = sum(is_shot_made),
            fga = sum(is_shot_attempted),
            fg_percent = fgm/fga,
            threes = sum(type_shot %like% "3PT"),
            efg_percent = (fgm+(.5*threes))/fga) %>% 
  ungroup() %>% 
  select(shooter,streak_col, defender_distance, efg_percent,fga) %>%
  pivot_wider(names_from = c("defender_distance","streak_col"),
              values_from = c("efg_percent", "fga"),values_fill = 0, names_sep = "_") %>%
  left_join(averages, by = c("shooter")) %>%
  clean_names() %>%
  left_join(total_fgas, by = c("shooter")) %>%
  mutate(streak_fga=Reduce("+",.[4:5]),
    coverage_diff = Reduce("-",.[3:2])
    ) %>%
  mutate_if(is.numeric, round,digits=4) %>% 
  left_join(teams, by = c("shooter")) %>% 
  select(shooter_team_name, colnames(test))

# %>%
#   select(shooter, total_fga, streak_fga,percent_fga_tight, base_streak_efg_1_make, efg_percent_open_1_make,
#          efg_percent_tight_1_make, coverage_diff) %>%
#   ungroup()


averages <- all_shots %>% 
  filter(streak_col == "1 Make") %>% 
  group_by(shooter,streak_col) %>% 
  summarise(fgm = sum(is_shot_made),
            fga = sum(is_shot_attempted),
            threes = sum(type_shot %like% "3PT"),
            base_streak_efg = (fgm+(.5*threes))/fga) %>% 
  select(shooter, base_streak_efg) 

test <-  all_shots %>% 
  mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
                                      closest_defender_distance_bin == "Very Tight" ~ "Tight",
                                      TRUE ~ closest_defender_distance_bin)) %>% 
  filter(streak_col == "1 Make")%>% 
  group_by(shooter, streak_col, defender_distance) %>%
  
  summarise(fgm = sum(is_shot_made),
            fga = sum(is_shot_attempted),
            fg_percent = fgm/fga,
            threes = sum(type_shot %like% "3PT"),
            efg_percent = (fgm+(.5*threes))/fga) %>% 
  ungroup() %>% 
  select(shooter,streak_col, defender_distance, efg_percent,fga) %>%
  pivot_wider(names_from = c("defender_distance","streak_col"),
              values_from = c("efg_percent", "fga"),values_fill = 0, names_sep = "_") %>%
  left_join(averages, by = c("shooter")) %>%
  clean_names() %>%
  left_join(total_fgas, by = c("shooter")) %>%
  mutate(streak_fga=Reduce("+",.[4:5]),
         coverage_diff = Reduce("-",.[3:2])
  ) %>%
  mutate_if(is.numeric, round,digits=4) %>% 
  left_join(teams, by = c("shooter")) %>% 
  select(shooter_team_name, colnames(test)) %>% 
  arrange(desc(streak_fga))






# app needs ---------------------------------------------------------------

all_shots <- dbGetQuery(con, "SELECT shooter,shooter_team_name, streak_col, type_shot,zone_basic,name_zone,
         zone_range, distance_shot, is_shot_attempted, is_shot_made,
         closest_defender_distance_bin FROM hhand_shots_tracking WHERE
                          streak_col = '1 Make'") 

teams <- all_shots %>% 
  select(shooter_team_name, shooter) %>% 
  group_by(shooter) %>% 
  slice(n()) %>% 
  ungroup()

total_fgas <- all_shots %>% 
  mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
                                      closest_defender_distance_bin == "Very Tight" ~ "Tight",
                                      TRUE ~ closest_defender_distance_bin)) %>% 
  group_by(shooter) %>% 
  summarise(total_fga = sum(is_shot_attempted),
            total_tight_attempts = sum(defender_distance =="Tight"),
            percent_fga_tight = total_tight_attempts/total_fga) 

averages <- all_shots %>% 
  filter(streak_col == "1 Make") %>% 
  group_by(shooter,streak_col) %>% 
  summarise(fgm = sum(is_shot_made),
            fga = sum(is_shot_attempted),
            threes = sum(type_shot %like% "3PT"),
            base_streak_efg = (fgm+(.5*threes))/fga) %>% 
  select(shooter, base_streak_efg) 


streak_data <-  all_shots %>% 
  mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
                                      closest_defender_distance_bin == "Very Tight" ~ "Tight",
                                      TRUE ~ closest_defender_distance_bin)) %>% 
  filter(streak_col == "1 Make")%>% 
  group_by(shooter, streak_col, defender_distance) %>%
  
  summarise(fgm = sum(is_shot_made),
            fga = sum(is_shot_attempted),
            fg_percent = fgm/fga,
            threes = sum(type_shot %like% "3PT"),
            efg_percent = (fgm+(.5*threes))/fga) %>% 
  ungroup() %>% 
  select(shooter,streak_col, defender_distance, efg_percent,fga) %>%
  pivot_wider(names_from = c("defender_distance","streak_col"),
              values_from = c("efg_percent", "fga"),values_fill = 0, names_sep = "_") %>%
  left_join(averages, by = c("shooter")) %>%
  clean_names() %>%
  left_join(total_fgas, by = c("shooter")) %>%
  mutate(streak_fga=Reduce("+",.[4:5]),
         coverage_diff = Reduce("-",.[3:2])
  ) %>%
  mutate_if(is.numeric, round,digits=4) %>% 
  left_join(teams, by = c("shooter")) %>% 
  select(shooter_team_name, colnames(test)) %>% 
  arrange(desc(streak_fga))



idk <- test[8:8]

# test --------------------------------------------------------------------


tab_query <- paste0("SELECT shooter,shooter_team_name, streak_col, type_shot,zone_basic,name_zone,
         zone_range, distance_shot, is_shot_attempted, is_shot_made,
         closest_defender_distance_bin FROM hhand_shots_tracking")

all_shots <- dbGetQuery(con, tab_query)

total_fgas <- all_shots %>% 
  mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
                                      closest_defender_distance_bin == "Very Tight" ~ "Tight",
                                      TRUE ~ closest_defender_distance_bin)) %>% 
  group_by(shooter,shooter_team_name) %>% 
  summarise(total_fga = sum(is_shot_attempted),
            total_tight_attempts = sum(defender_distance =="Tight"),
            percent_fga_tight = total_tight_attempts/total_fga)

# all_shots <- dbGetQuery(con, tab_query)

# all_shots <- dbGetQuery(con, "SELECT shooter,shooter_team_name, streak_col, type_shot,zone_basic,name_zone,
#        zone_range, distance_shot, is_shot_attempted, is_shot_made,
#        closest_defender_distance_bin FROM hhand_shots_tracking") 

# teams <- all_shots %>% 
#   select(shooter_team_name, shooter) %>% 
#   group_by(shooter) %>% 
#   slice(n()) %>% 
#   ungroup()
# 
# total_fgas <- all_shots %>% 
#   mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
#                                       closest_defender_distance_bin == "Very Tight" ~ "Tight",
#                                       TRUE ~ closest_defender_distance_bin)) %>% 
#   group_by(shooter) %>% 
#   summarise(total_fga = sum(is_shot_attempted),
#             total_tight_attempts = sum(defender_distance =="Tight"),
#             percent_fga_tight = total_tight_attempts/total_fga) 
# 
# averages <- all_shots %>% 
#   filter(streak_col == "1 Make") %>% 
#   group_by(shooter,streak_col) %>% 
#   summarise(fgm = sum(is_shot_made),
#             fga = sum(is_shot_attempted),
#             threes = sum(type_shot %like% "3PT"),
#             base_streak_efg = (fgm+(.5*threes))/fga) %>% 
#   select(shooter, base_streak_efg)
# 
# final_data <- all_shots %>% 
#   filter(streak_col == "1 Make") %>% 
#   mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
#                                       closest_defender_distance_bin == "Very Tight" ~ "Tight",
#                                       TRUE ~ closest_defender_distance_bin)) %>% 
#   group_by(shooter, streak_col, defender_distance) %>%
#   
#   summarise(fgm = sum(is_shot_made),
#             fga = sum(is_shot_attempted),
#             fg_percent = fgm/fga,
#             threes = sum(type_shot %like% "3PT"),
#             efg_percent = (fgm+(.5*threes))/fga) %>% 
#   ungroup() %>% 
#   select(shooter,streak_col, defender_distance, efg_percent,fga) %>%
#   pivot_wider(names_from = c("defender_distance","streak_col"),
#               values_from = c("efg_percent", "fga"),values_fill = 0, names_sep = "_") %>%
#   left_join(averages, by = c("shooter")) %>%
#   clean_names() %>%
#   left_join(total_fgas, by = c("shooter")) %>%
#   mutate(streak_fga=Reduce("+",.[4:5]),
#          coverage_diff = Reduce("-",.[3:2])
#   ) %>%
#   mutate_if(is.numeric, round,digits=4) %>% 
#   left_join(teams, by = c("shooter")) %>% 
#   select(12,1,7,10,9,6,2,3,11)
# 
# colnames(final_data)



