# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# Squads 2024
ipl_squads_2024 <- read_csv("Data/ipl_squads_2024.csv")

# Function to fix team names
source("Scripts/07-fix-team-names.R")

#===============================================================================
# Get JSON for each match
#===============================================================================

# Read in df
df <- read_csv("Odds-Scraper/Neds/neds_ipl_match_urls.csv")

# Get match json files
json_match_files <- list.files("Odds-Scraper/Neds/", pattern = "^data_.*.json", full.names = TRUE)

event_json_list <- map(json_match_files, ~fromJSON(.x))

#===============================================================================
# Get the market information for each match
#===============================================================================

# Initialize empty vectors to store the market names and IDs for mapping
market_lookup_name <- character()
market_lookup_id <- character()

# Initialize empty vectors to store data
entrants <- character()
market_id <- character()
match_names <- character()
handicaps <- numeric()
prices <- numeric()

# Loop through the entrants
for (i in seq_along(event_json_list)) {
  match_name <- df$event_name[i]
  match <- event_json_list[[i]]
  
  for (entrant in match$entrants) {
    entrants <- c(entrants, entrant$name)
    market_id <- c(market_id, entrant$market_id)
    match_names <- c(match_names, match_name)
  }
  
  # Loop through the markets
  for (market in match$markets) {

    # Check for market name existence and append accordingly
    if (is.null(market$name)) {
      market_lookup_name <- c(market_lookup_name, NA)  # Ensure NA is added if there's no name
    } else {
      market_lookup_name <- c(market_lookup_name, market$name)
    }
    
    # Check for id existence and append accordingly
    if (is.null(market$id)) {
      market_lookup_id <- c(market_lookup_id, NA)  # Ensure NA is added if there's no id
    } else {
      market_lookup_id <- c(market_lookup_id, market$id)
    }
    
    # Check for handicap existence and append accordingly
    if (is.null(market$handicap)) {
      handicaps <- c(handicaps, NA)  # Ensure NA is added if there's no handicap
    } else {
      handicaps <- c(handicaps, market$handicap)
    }
  }
  
  # Loop through the prices
  for (price in match$prices) {
    fractional_odds <- price$odds$numerator / price$odds$denominator
    decimal_odds <- fractional_odds + 1
    prices <- c(prices, decimal_odds)
  }
}

# Create market lookup dataframe
market_lookup_df <- data.frame(market_id = market_lookup_id, market_name = market_lookup_name, handicaps = handicaps)

# Create market dataframe
market_df <- data.frame(match_name = match_names, market_id = market_id, entrants = entrants, price = prices)

# Merge market lookup dataframe with market dataframe
market_df <- merge(market_df, market_lookup_df, by = 'market_id', all.x = TRUE)

# Reorder columns in market_df
market_df <-
  market_df |>
  select(match_name, market_name, entrants, handicaps, price) |> 
  mutate(match_name = str_replace(match_name, " vs ", " v "))

##%######################################################%##
#                                                          #
####               Get Head to Head Data                ####
#                                                          #
##%######################################################%##

# Filter to only include head to head markets
h2h_data <-
  market_df |> 
  filter(market_name == "Match Betting") |> 
  select(-market_name)


# Home teams
home_teams <-
  h2h_data |> 
  separate(match_name, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  filter(entrants == home_team) |> 
  select(match = match_name, home_team, home_win = price)

# Away teams
away_teams <-
  h2h_data |> 
  separate(match_name, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  filter(entrants == away_team) |> 
  select(match = match_name, away_team, away_win = price)

# Merge home and away teams
h2h_data <-
  home_teams |> 
  left_join(away_teams, by = c("match")) |> 
  mutate(margin = round(1 / home_win + 1 / away_win, digits = 2)) |>
  mutate(agency = "Neds") |>
  select(match, home_team, away_team, home_win, away_win, margin, agency)

##%######################################################%##
#                                                          #
####                  Player Runs Data                  ####
#                                                          #
##%######################################################%##

# Filter to only include player runs markets
player_runs_data <-
  market_df |> 
  filter(str_detect(market_name, "(Total Runs)|(to Score)"))

# Overs
runs_overs <-
  player_runs_data |>
  filter(str_detect(entrants, "Over") |
           str_detect(market_name, "to Score")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = "Player Runs",
    player_name,
    line = handicap,
    over_price = price,
    agency = "Neds"
  )

# Unders
runs_unders <-
  player_runs_data |>
  filter(str_detect(entrants, "Under")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = "Player Runs",
    player_name,
    line = handicap,
    under_price = price,
    agency = "Neds"
  )

# Merge overs and unders
player_runs_data <-
  runs_overs |> 
  full_join(runs_unders, by = c("match", "player_name", "line", "agency", "market_name")) |> 
  select(match, market = market_name, player_name, line, over_price, under_price, agency) |> 
  mutate(player_name = case_when(player_name == "Tom Rogers" ~ "Tom F Rogers",
                                 .default = player_name)) |> 
  left_join(ipl_squads_2024, by = c("player_name" = "player")) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  rename(player_team = team) |> 
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)

# Write to CSV------------------------------------------------------------------
player_runs_data |> 
  write_csv("Data/scraped_odds/neds_player_runs.csv")

##%######################################################%##
#                                                          #
####                Player Wickets Data                 ####
#                                                          #
##%######################################################%##

# Filter to only include player wickets markets
player_wickets_data <-
  market_df |> 
  filter(str_detect(market_name, "(Player Wickets)|(to Take)"))

# Overs
wickets_overs <-
  player_wickets_data |>
  filter(str_detect(entrants, "Over") |
           str_detect(market_name, "to Take")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = "Player Wickets",
    player_name,
    line = handicap,
    over_price = price,
    agency = "Neds"
  )

# Unders
wickets_unders <-
  player_wickets_data |>
  filter(str_detect(entrants, "Under")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = "Player Wickets",
    player_name,
    line = handicap,
    under_price = price,
    agency = "Neds"
  )

# Merge overs and unders
player_wickets_data <-
  wickets_overs |> 
  full_join(wickets_unders, by = c("match", "player_name", "line", "agency", "market_name")) |> 
  select(match, market = market_name, player_name, line, over_price, under_price, agency) |> 
  mutate(player_name = case_when(player_name == "Tom Rogers" ~ "Tom Rogers",
                                 .default = player_name)) |> 
  left_join(ipl_squads_2024, by = c("player_name" = "player")) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  rename(player_team = team) |> 
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)


# Write to CSV------------------------------------------------------------------
player_wickets_data |> 
  write_csv("Data/scraped_odds/neds_player_wickets.csv")

##%######################################################%##
#                                                          #
####             Player Boundaries Data                 ####
#                                                          #
##%######################################################%##

# Filter to only include player boundaries markets
player_boundaries_data <-
  market_df |> 
  filter(str_detect(market_name, "(Player Boundaries)|(to Hit)")) |> 
  filter(str_detect(market_name, "\\+"))

# Overs
boundaries_overs <-
  player_boundaries_data |>
  filter(str_detect(entrants, "Over") |
           str_detect(market_name, "to Hit")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = ifelse(str_detect(market_name, "Four"), "Number of 4s", "Number of 6s"),
    player_name,
    line = handicap,
    over_price = price,
    agency = "Neds"
  )

# Unders
boundaries_unders <-
  player_boundaries_data |>
  filter(str_detect(entrants, "Under")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = if_else(str_detect(market_name, "Four"), "Player 4s", "Player 6s"),
    player_name,
    line = handicap,
    under_price = price,
    agency = "Neds"
  )

# Merge overs and unders
player_boundaries_data <-
  boundaries_overs |> 
  full_join(boundaries_unders, by = c("match", "player_name", "line", "agency", "market_name")) |> 
  select(match, market = market_name, player_name, line, over_price, under_price, agency) |> 
  mutate(player_name = case_when(player_name == "Sk Rasheed" ~ "Shaik Rasheed",
                                 .default = player_name)) |> 
  left_join(ipl_squads_2024, by = c("player_name" = "player")) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  rename(player_team = team) |> 
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)

# Write to CSV------------------------------------------------------------------
player_boundaries_data |> 
  write_csv("Data/scraped_odds/neds_player_boundaries.csv")
