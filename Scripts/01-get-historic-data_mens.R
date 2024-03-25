##%######################################################%##
#                                                          #
####              Libraries and functions               ####
#                                                          #
##%######################################################%##

library(tidyverse)
library(cricketdata)
`%notin%` <- Negate(`%in%`)

##%######################################################%##
#                                                          #
####              Get data from cricsheet               ####
#                                                          #
##%######################################################%##

# Get match level dataset
match_data_ipl <-
  fetch_cricsheet(type = "match",
                  gender = "male",
                  competition = "ipl")

# Get player level dataset
player_data_ipl <-
  fetch_cricsheet(type = "player",
                  gender = "male",
                  competition = "ipl")

# Get ball by ball level dataset
ball_by_ball_data_ipl <-
  fetch_cricsheet(type = "bbb",
                  gender = "male",
                  competition = "ipl") |> 
  mutate(match_id = as.character(match_id))

##%######################################################%##
#                                                          #
####               Get match level runs,                ####
####           balls, boundaries and wickets            ####
#                                                          #
##%######################################################%##

venue_data <-
ball_by_ball_data_ipl |> 
  mutate(all_runs = runs_off_bat + extras) |>
  group_by(match_id,start_date, venue, innings) |>
  summarise(
    batting_team = min(batting_team),
    bowling_team = min(bowling_team),
    runs = sum(all_runs, na.rm = TRUE),
    balls = n(),
    fours = sum(runs_off_bat == 4, na.rm = TRUE),
    sixes = sum(runs_off_bat == 6, na.rm = TRUE),
    wickets = sum(wicket, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  group_by(match_id) |> 
  mutate(match_total = sum(runs, na.rm = TRUE)) |> 
  rename(innings_total = runs)

##%######################################################%##
#                                                          #
####  Aggregate ball by ball data to get player stats   ####
#                                                          #
##%######################################################%##

# Batting stats
batting_stats_ipl <-
ball_by_ball_data_ipl |>
  group_by(striker, match_id, start_date, venue) |>
  summarise(
    innings = min(innings),
    balls_faced = n(),
    runs_scored = sum(runs_off_bat, na.rm = TRUE),
    fours = sum(runs_off_bat == 4, na.rm = TRUE),
    sixes = sum(runs_off_bat == 6, na.rm = TRUE),
    not_out = max(wicket, na.rm = TRUE) == 0
  ) |> 
  left_join(player_data_ipl, by = c("striker" = "player", "match_id" = "match_id")) |>
  ungroup() |> 
  arrange(desc(runs_scored)) |> 
  rename(player = striker)

# Get Batting Position
batting_position_ipl_strikers <-
  ball_by_ball_data_ipl |>
  select(match_id, innings, striker, non_striker, bowler, over, ball, wickets_lost_yet) |>
  group_by(match_id, innings, striker) |>
  slice_head(n = 1) |>
  mutate(player = striker, role = "Striker") |> 
  ungroup() |> 
  select(-striker, -non_striker)

batting_position_ipl_non_strikers <-
  ball_by_ball_data_ipl |>
  select(match_id, innings, striker, non_striker, bowler, over, ball, wickets_lost_yet) |>
  group_by(match_id, innings, non_striker) |>
  slice_head(n = 1) |>
  mutate(player = non_striker, role = "Non-Striker") |> 
  ungroup() |> 
  select(-striker, -non_striker)

batting_position_ipl <-
  bind_rows(batting_position_ipl_strikers, batting_position_ipl_non_strikers) |>
  arrange(match_id, innings, over, ball, player, desc(role)) |>
  group_by(match_id, innings, player) |>
  slice_head(n = 1) |>
  arrange(match_id, innings, over, ball, player, desc(role)) |>
  group_by(match_id, innings) |> 
  mutate(batting_position = row_number()) |>
  select(match_id, innings, player, batting_position)
  
batting_stats_ipl <-
  batting_stats_ipl |> 
  left_join(batting_position_ipl, by = c("match_id", "innings", "player")) |> 
  mutate(DNB = FALSE)

# Bowling stats
bowling_stats_ipl <-
  ball_by_ball_data_ipl |> 
  group_by(bowler, match_id, start_date, venue) |>
  summarise(
    innings = min(innings),
    balls_bowled = n(),
    runs_conceded = sum(runs_off_bat + extras, na.rm = TRUE),
    wickets_taken = sum(wicket_type != "" & wicket_type != "run out", na.rm = TRUE),
    economy_rate = runs_conceded / (balls_bowled / 6)
  ) |> 
  left_join(player_data_ipl, by = c("bowler" = "player", "match_id" = "match_id")) |>
  ungroup() |> 
  arrange(desc(wickets_taken)) |> 
  rename(player = bowler)

# Get Balls in innings
match_balls_bowled_innings <-
  ball_by_ball_data_ipl |>
  group_by(match_id, innings) |>
  slice_tail(n = 1) |>
  mutate(balls_bowled = 6*over + ball) |> 
  select(match_id, innings, balls_bowled_innings = balls_bowled)

##%######################################################%##
#                                                          #
####                 Add in DNB Batters                 ####
#                                                          #
##%######################################################%##

# Innings 1
batting_match_data_innings_1_dnb <-
  match_data_ipl |> 
  mutate(non_toss_winner = ifelse(toss_winner == team1, team2, team1)) |>
  transmute(match_id,
            start_date = ymd(date),
            venue,
            innings = 1,
            balls_faced = 0,
            runs_scored = 0,
            fours = 0,
            sixes = 0,
            not_out = TRUE,
            team = ifelse(toss_decision == "bat", toss_winner, non_toss_winner),
            DNB = TRUE) |> 
  left_join(player_data_ipl, by = c("match_id", "team")) |> 
  mutate(player_id = paste(player, match_id)) |>
  filter(player_id %notin% paste(batting_stats_ipl$player, batting_stats_ipl$match_id))

# Innings 2
batting_match_data_innings_2_dnb <-
  match_data_ipl |> 
  mutate(non_toss_winner = ifelse(toss_winner == team1, team2, team1)) |>
  transmute(match_id,
            start_date = ymd(date),
            venue,
            innings = 2,
            balls_faced = 0,
            runs_scored = 0,
            fours = 0,
            sixes = 0,
            not_out = TRUE,
            team = ifelse(toss_decision == "field", toss_winner, non_toss_winner),
            DNB = TRUE) |> 
  left_join(player_data_ipl, by = c("match_id", "team")) |> 
  mutate(player_id = paste(player, match_id)) |>
  filter(player_id %notin% paste(batting_stats_ipl$player, batting_stats_ipl$match_id))

# Combine
batting_match_data_dnb <-
  bind_rows(batting_match_data_innings_1_dnb, batting_match_data_innings_2_dnb) |> 
  select(-player_id)

# Add to batting stats----------------------------------------------
batting_stats_ipl <-
  bind_rows(batting_stats_ipl, batting_match_data_dnb) |> 
  arrange(match_id, innings, batting_position, player)

# Add Season---------------------------------------------------------
batting_stats_ipl <-
  batting_stats_ipl |> 
  left_join(match_data_ipl[,c("match_id", "season")], relationship = "many-to-one")

bowling_stats_ipl <-
  bowling_stats_ipl |> 
  left_join(match_data_ipl[,c("match_id", "season")], relationship = "many-to-one")

# Add opposition team------------------------------------------------
batting_stats_ipl <-
  batting_stats_ipl |> 
  left_join(match_data_ipl[,c("match_id", "team1", "team2")], relationship = "many-to-one") |> 
  mutate(opposition = ifelse(team == team1, team2, team1)) |> 
  select(-team1, -team2)

bowling_stats_ipl <-
  bowling_stats_ipl |> 
  left_join(match_data_ipl[,c("match_id", "team1", "team2")], relationship = "many-to-one") |> 
  mutate(opposition = ifelse(team == team1, team2, team1)) |> 
  select(-team1, -team2)

# Add balls bowled----------------------------------------------
batting_stats_ipl <-
  batting_stats_ipl |> 
  left_join(match_balls_bowled_innings, by = c("match_id", "innings"))

bowling_stats_ipl <-
  bowling_stats_ipl |> 
  left_join(match_balls_bowled_innings, by = c("match_id", "innings"))

##%######################################################%##
#                                                          #
####                  Write out as RDS                  ####
#                                                          #
##%######################################################%##
# 
# # Standardise Venue Names
# batting_stats_ipl <-
#   batting_stats_ipl |>
#   mutate(venue = case_when(
#     str_detect(venue, "Brisbane Cricket Ground") ~ "The Gabba",
#     str_detect(venue, "Sydney Cricket Ground") ~ "SCG",
#     str_detect(venue, "Adelaide Oval") ~ "Adelaide Oval",
#     str_detect(venue, "Manuka Oval") ~ "Manuka Oval",
#     str_detect(venue, "Melbourne Cricket Ground") ~ "MCG",
#     str_detect(venue, "Perth Stadium") ~ "Perth Stadium",
#     str_detect(venue, "Blundstone Arena") ~ "Blundstone Arena",
#     str_detect(venue, "Docklands") ~ "Marvel Stadium",
#     str_detect(venue, "Marvel Stadium") ~ "Marvel Stadium",
#     str_detect(venue, "International Sports Stadium") ~ "Coffs Harbour",
#     str_detect(venue, "Sydney Showground Stadium") ~ "Sydney Showground Stadium",
#     str_detect(venue, "Carrara Oval") ~ "Carrara Oval",
#     str_detect(venue, "Adelaide Oval") ~ "Adelaide Oval",
#     str_detect(venue, "Traeger Park") ~ "Traeger Park",
#     str_detect(venue, "Metricon Stadium") ~ "Metricon Stadium",
#     str_detect(venue, "University of Tasmania Stadium") ~ "University of Tasmania Stadium",
#     str_detect(venue, "(UTAS)|(Aurora)") ~ "University of Tasmania Stadium",
#     str_detect(venue, "Bellerive") ~ "Bellerive Oval",
#     str_detect(venue, "Simonds|Kardinia|GMHBA") ~ "GMHBA Stadium",
#     str_detect(venue, "Western Australia Cricket Association Ground|W\\.A\\.C\\.A") ~ "WACA",
#     TRUE ~ venue))
# 
# bowling_stats_ipl <-
#   bowling_stats_ipl |>
#   mutate(venue = case_when(
#     str_detect(venue, "Brisbane Cricket Ground") ~ "The Gabba",
#     str_detect(venue, "Sydney Cricket Ground") ~ "SCG",
#     str_detect(venue, "Adelaide Oval") ~ "Adelaide Oval",
#     str_detect(venue, "Manuka Oval") ~ "Manuka Oval",
#     str_detect(venue, "Melbourne Cricket Ground") ~ "MCG",
#     str_detect(venue, "Perth Stadium") ~ "Perth Stadium",
#     str_detect(venue, "Blundstone Arena") ~ "Blundstone Arena",
#     str_detect(venue, "Docklands") ~ "Marvel Stadium",
#     str_detect(venue, "Marvel Stadium") ~ "Marvel Stadium",
#     str_detect(venue, "International Sports Stadium") ~ "Coffs Harbour",
#     str_detect(venue, "Sydney Showground Stadium") ~ "Sydney Showground Stadium",
#     str_detect(venue, "Carrara Oval") ~ "Carrara Oval",
#     str_detect(venue, "Adelaide Oval") ~ "Adelaide Oval",
#     str_detect(venue, "Traeger Park") ~ "Traeger Park",
#     str_detect(venue, "Metricon Stadium") ~ "Metricon Stadium",
#     str_detect(venue, "University of Tasmania Stadium") ~ "University of Tasmania Stadium",
#     str_detect(venue, "(UTAS)|(Aurora)") ~ "University of Tasmania Stadium",
#     str_detect(venue, "Bellerive") ~ "Bellerive Oval",
#     str_detect(venue, "Simonds|Kardinia|GMHBA") ~ "GMHBA Stadium",
#     str_detect(venue, "Western Australia Cricket Association Ground|W\\.A\\.C\\.A") ~ "WACA",
#     TRUE ~ venue))
# 
# venue_data <-
#   venue_data |>
#   mutate(
#     venue = case_when(
#       str_detect(venue, "Brisbane Cricket Ground") ~ "The Gabba",
#       str_detect(venue, "Sydney Cricket Ground") ~ "SCG",
#       str_detect(venue, "Adelaide Oval") ~ "Adelaide Oval",
#       str_detect(venue, "Manuka Oval") ~ "Manuka Oval",
#       str_detect(venue, "Melbourne Cricket Ground") ~ "MCG",
#       str_detect(venue, "Perth Stadium") ~ "Perth Stadium",
#       str_detect(venue, "Blundstone Arena") ~ "Blundstone Arena",
#       str_detect(venue, "Docklands") ~ "Marvel Stadium",
#       str_detect(venue, "Marvel Stadium") ~ "Marvel Stadium",
#       str_detect(venue, "International Sports Stadium") ~ "Coffs Harbour",
#       str_detect(venue, "Sydney Showground Stadium") ~ "Sydney Showground Stadium",
#       str_detect(venue, "Carrara Oval") ~ "Carrara Oval",
#       str_detect(venue, "Adelaide Oval") ~ "Adelaide Oval",
#       str_detect(venue, "Traeger Park") ~ "Traeger Park",
#       str_detect(venue, "Metricon Stadium") ~ "Metricon Stadium",
#       str_detect(venue, "University of Tasmania Stadium") ~ "University of Tasmania Stadium",
#       str_detect(venue, "(UTAS)|(Aurora)") ~ "University of Tasmania Stadium",
#       str_detect(venue, "Bellerive") ~ "Bellerive Oval",
#       str_detect(venue, "Simonds|Kardinia|GMHBA") ~ "GMHBA Stadium",
#       str_detect(
#         venue,
#         "Western Australia Cricket Association Ground|W\\.A\\.C\\.A"
#       ) ~ "WACA",
#       TRUE ~ venue
#     )
#   )

#===============================================================================
# Write out data
#===============================================================================

write_rds(bowling_stats_ipl, "Data/bowling_stats_ipl.rds")
write_rds(batting_stats_ipl, "Data/batting_stats_ipl.rds")
write_rds(venue_data, "Data/venue_data_ipl.rds")

# Write out player names by season dataset
player_data_ipl |> 
  left_join(match_data_ipl[, c("match_id", "season")]) |> 
  distinct(team, player, season) |> 
  arrange(player, desc(season)) |> 
  write_rds("Data/player_names_teams_ipl.rds")
