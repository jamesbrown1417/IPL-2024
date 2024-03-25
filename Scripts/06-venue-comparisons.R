library(tidyverse)

venue_data_bbl <- read_rds("Data/venue_data_bbl.rds")

venue_summary_first_innings <-
venue_data_bbl |> 
  filter(innings == 1 & balls >= 120) |> 
  group_by(venue) |>
  summarise(
    games = n(),
    median_runs = median(innings_total),
    median_fours = median(fours),
    median_sixes = median(sixes),
    median_wickets = median(wickets)) |> 
  arrange(desc(median_runs))

venue_summary_total <-
venue_data_bbl |> 
  mutate(first_innings_balls = ifelse(innings == 1, balls, NA)) |>
  filter(max(first_innings_balls, na.rm = TRUE) >= 120) |>
  group_by(match_id, venue) |> 
  mutate(match_fours = sum(fours, na.rm = TRUE),
         match_sixes = sum(sixes, na.rm = TRUE),
         match_wickets = sum(wickets, na.rm = TRUE)) |> 
  group_by(venue) |>
  summarise(
    games = n() / 2,
    median_runs = median(match_total),
    median_fours = median(match_fours),
    median_sixes = median(match_sixes),
    median_wickets = median(match_wickets))
  

