# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(tidyjson)

pointsbet_h2h_main <- function() {
  
  # URL of website
  pointsbet_url = "https://api.au.pointsbet.com/api/v2/competitions/20040/events/featured?includeLive=false"
  
  # Make request and get response
  pointsbet_response <-
    request(pointsbet_url) |>
    req_perform() |> 
    resp_body_json()
  
  # List of matches and data
  events <- pointsbet_response$events
  
  # Loop through to get all data--------------------------------------------------
  
  # Create empty vectors
  match_names <- c()
  match_starts_at <- c()
  home_teams <- c()
  away_teams <- c()
  event_names <- c()
  outcome_names <- c()
  outcome_prices <- c()
  keys <- c()
  
  # Loop through events
  for (match in events) {
    for (market in match$fixedOddsMarkets) {
      for (outcome in market$outcomes) {
        # Append data to vectors
        match_names <- c(match_names, match$name)
        match_starts_at <- c(match_starts_at, match$startsAt)
        home_teams <- c(home_teams, match$homeTeam)
        away_teams <- c(away_teams, match$awayTeam)
        event_names <- c(event_names, market$eventName)
        outcome_names <- c(outcome_names, outcome$name)
        outcome_prices <- c(outcome_prices, outcome$price)
        keys <- c(keys, match$key)
      }
    }
  }
  
  # Output tibble
  pointsbet_data <-
    tibble(
      match = match_names,
      start_time = match_starts_at,
      home_team = home_teams,
      away_team = away_teams,
      event = event_names,
      outcome = outcome_names,
      price = outcome_prices
    ) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    relocate(match, .before = start_time)
  
  #===============================================================================
  # Head to head markets
  #===============================================================================
  
  # Filter to head to head markets
  pointsbet_data_h2h <-
    pointsbet_data |> 
    filter(event == "Match Winner") 
  
  # Home Teams
  pointsbet_data_h2h_home <-
    pointsbet_data_h2h |> 
    filter(home_team == outcome) |>
    select(match, start_time, market = event, home_team, home_win = price)
  
  # Away Teams
  pointsbet_data_h2h_away <-
    pointsbet_data_h2h |> 
    filter(away_team == outcome) |>
    select(match, start_time, market = event, away_team, away_win = price)
  
  # Combine
  pointsbet_h2h <-
    full_join(pointsbet_data_h2h_home, pointsbet_data_h2h_away, by = c("match", "start_time", "market")) |> 
    mutate(market = "Head To Head") |>
    select(match, start_time, market, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Pointsbet")
  
  # Write to csv
  write_csv(pointsbet_h2h, "Data/scraped_odds/pointsbet_h2h.csv")
  
  #===============================================================================
  # Player Props
  #===============================================================================
  
  # Get unique keys
  keys <- unique(keys)
  
  # Get each match's api page
  match_urls <- paste0("https://api.au.pointsbet.com/api/mes/v3/events/", keys)
  
  # Create a function that gets the player props from each URL
  get_player_props <- function(url) {
    # Make request and get response
    pointsbet_response <-
      request(url) |>
      req_perform() |>
      resp_body_json()
    
    # Loop through to get prop data---------------------------------------------
    
    # Create empty vectors
    match_names <- c()
    market_names <- c()
    outcome_names <- c()
    outcome_types <- c()
    outcome_prices <- c()
    
    # Loop through events
    for (market in pointsbet_response$fixedOddsMarkets) {
      for (outcome in market$outcomes) {
        # Append data to vectors
        match_names <- c(match_names, pointsbet_response$name)
        
        if (!is.null(market$name)) {
          market_names <- c(market_names, market$name)
        } else {
          market_names <- c(market_names, NA)
        }
        
        if (!is.null(outcome$name)) {
          outcome_names <- c(outcome_names, outcome$name)
        } else {
          outcome_names <- c(outcome_names, NA)
        }
        
        if (!is.null(outcome$outcomeType)) {
          outcome_types <- c(outcome_types, outcome$outcomeType)
        } else {
          outcome_types <- c(outcome_types, NA)
        }
        
        if (!is.null(outcome$price)) {
          outcome_prices <- c(outcome_prices, outcome$price)
        } else {
          outcome_prices <- c(outcome_prices, NA)
        }
      }
    }
    
    # Output tibble
    tibble(
      match = match_names,
      market = market_names,
      outcome = outcome_names,
      outcome_type = outcome_types,
      price = outcome_prices
    )
  }
  
  # Map function to each URL
  pointsbet_data_player_props <- map_df(match_urls, get_player_props)
  
  #===============================================================================
  # Player Runs
  #===============================================================================
  
  # Player runs alternative totals----------------------------------------------
  
  # Filter list to player runs
  pointsbet_player_runs_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Pick Your Own Runs")) |>
    mutate(line = str_extract(outcome, "[0-9]{1,3}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(match = str_replace(match, "@", "v")) |> 
    mutate(outcome = str_remove(outcome, " To Score.*$")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(outcome = case_when(outcome == "Tom Rogers" ~ "Tom F Rogers",
                               outcome == "Steven Smith" ~ "Steve Smith",
                               .default = outcome)) |>
    left_join(player_teams[, c("player_name", "player_team")], by = c("outcome" = "player_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market = "Player Runs",
      player_name = outcome,
      player_team,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet")
  
  # Player runs over / under----------------------------------------------------
  
  # Filter list to player runs over under
  pointsbet_player_runs_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player .* Total Runs"))
  
  # Get Overs
  pointsbet_player_runs_over <-
    pointsbet_player_runs_over_under |> 
    filter(str_detect(outcome, "Over")) |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Over ") |>
    mutate(line = as.numeric(line)) |> 
    mutate(match = str_replace(match, "@", "v")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(player_name = case_when(player_name == "Tom Rogers" ~ "Tom F Rogers",
                                   player_name == "Steven Smith" ~ "Steve Smith",
                               .default = player_name)) |>
    left_join(player_teams[, c("player_name", "player_team")], by = c("player_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |> 
    transmute(
      match,
      home_team,
      away_team,
      market = "Player Runs",
      player_name,
      player_team,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet")
  
  # Get Unders
  pointsbet_player_runs_under <-
    pointsbet_player_runs_over_under |> 
    filter(str_detect(outcome, "Under")) |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Under ") |>
    mutate(line = as.numeric(line)) |> 
    mutate(match = str_replace(match, "@", "v")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(player_name = case_when(player_name == "Tom Rogers" ~ "Tom F Rogers",
                                   player_name == "Steven Smith" ~ "Steve Smith",
                                   .default = player_name)) |>
    left_join(player_teams[, c("player_name", "player_team")], by = c("player_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |> 
    transmute(
      match,
      home_team,
      away_team,
      market = "Player Runs",
      player_name,
      player_team,
      opposition_team,
      line,
      under_price = price,
      agency = "Pointsbet")
  
  # Combine overs and unders
  pointsbet_player_runs_over_under <- 
    pointsbet_player_runs_over |>
    left_join(pointsbet_player_runs_under) |>
    select(match, home_team, away_team, market, player_name, player_team, opposition_team, line, over_price, under_price, agency)
  
  # Write to csv----------------------------------------------------------------
  pointsbet_player_runs_lines |> 
    bind_rows(pointsbet_player_runs_over_under) |> 
  write_csv("Data/scraped_odds/pointsbet_player_runs.csv")
  
  #===============================================================================
  # Player Wickets
  #===============================================================================
  
  # Player wickets alternative totals----------------------------------------------
  
  # Filter list to player wickets
  pointsbet_player_wickets_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Bowler .* Wickets")) |>
    mutate(line = str_extract(outcome, "[0-9]{1,3}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(match = str_replace(match, "@", "v")) |> 
    mutate(outcome = str_remove(outcome, " To Take.*$")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(outcome = case_when(outcome == "Matthew Kuhnemann" ~ "Matt Kuhnemann",
                               outcome == "Mitch Swepson" ~ "Mitchell Swepson",
                               outcome == "Tom S. Rogers" ~ "Tom Rogers",
                               outcome == "Andrew tye" ~ "Andrew Tye",
                               .default = outcome)) |>
    left_join(player_teams[, c("player_name", "player_team")], by = c("outcome" = "player_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market = "Player Wickets",
      player_name = outcome,
      player_team,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet")
  
  # Write to csv----------------------------------------------------------------
  pointsbet_player_wickets_lines |> 
    write_csv("Data/scraped_odds/pointsbet_player_wickets.csv")
  
  #===============================================================================
  # Player Boundaries
  #===============================================================================
  
  # Player fours alternative totals----------------------------------------------
  
  # Filter list to player fours
  pointsbet_player_fours_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Pick Your Own Fours")) |>
    mutate(line = str_extract(outcome, "[0-9]{1,3}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(match = str_replace(match, "@", "v")) |> 
    mutate(outcome = str_remove(outcome, " To Hit.*$")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(outcome = case_when(outcome == "Tom Rogers" ~ "Tom F Rogers",
                               outcome == "Steven Smith" ~ "Steve Smith",
                               .default = outcome)) |>
    left_join(player_teams[, c("player_name", "player_team")], by = c("outcome" = "player_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market = "Number of 4s",
      player_name = outcome,
      player_team,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet")
  
  # Player sixes alternative totals----------------------------------------------
  
  # Filter list to player sixes
  pointsbet_player_sixes_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Pick Your Own Sixes")) |>
    mutate(line = str_extract(outcome, "[0-9]{1,3}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(match = str_replace(match, "@", "v")) |> 
    mutate(outcome = str_remove(outcome, " To Hit.*$")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(outcome = case_when(outcome == "Tom Rogers" ~ "Tom F Rogers",
                               outcome == "Steven Smith" ~ "Steve Smith",
                               .default = outcome)) |>
    left_join(player_teams[, c("player_name", "player_team")], by = c("outcome" = "player_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market = "Number of 6s",
      player_name = outcome,
      player_team,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet")
  
  # Write to csv----------------------------------------------------------------
  pointsbet_player_sixes_lines |> 
    bind_rows(pointsbet_player_fours_lines) |> 
    write_csv("Data/scraped_odds/pointsbet_player_boundaries.csv")
  
}

##%######################################################%##
#                                                          #
####                   Run functions                    ####
#                                                          #
##%######################################################%##

# This runs both the props and head to head as they use same info
h2h_safe_pointsbet <- safely(pointsbet_h2h_main)

# Run functions
h2h_safe_pointsbet()
