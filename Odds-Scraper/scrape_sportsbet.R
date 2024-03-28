# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# URL of website
sportsbet_url = "https://www.sportsbet.com.au/betting/cricket/twenty20-ipl"

# Squads 2024
ipl_squads_2024 <- read_csv("Data/ipl_squads_2024.csv")

# Function to fix team names
source("Scripts/07-fix-team-names.R")

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

main_markets_function <- function() {
  # Get data from main market page
  matches <-
    sportsbet_url |>
    read_html() |>
    html_nodes(".White_fqa53j6")
  
  # Function to get team names
  get_team_names <- function(match) {
    team_names <-
      match |>
      html_nodes(".normal_fgzdi7m") |>
      html_text()
    
    # Home team and Away Team
    home_team <- team_names[1]
    away_team <- team_names[2]
    
    # Output
    tibble(home_team, away_team)
  }
  
  # Function to get odds
  get_odds <- function(match) {
    odds <-
      match |>
      html_nodes(".priceTextSize_frw9zm9") |>
      html_text() |>
      as.numeric()
    
    # Home team
    home_win <- odds[1]
    away_win <- odds[2]
    
    # Output
    tibble(home_win, away_win)
  }
  
  # Function to get start time
  get_start_time <- function(match) {
    start_time <-
      match |>
      html_nodes(".oneLine_f15ay66x") |>
      html_text()
    
    # Output
    tibble(start_time)
  }
  
  # Map functions to each match and combine together
  all_main_market_data <-
    bind_cols(
      map(matches, get_team_names) |> bind_rows(),
      map(matches, get_odds) |> bind_rows(),
      map(matches, get_start_time) |> bind_rows()
    )
  
  #===============================================================================
  # Head to Head markets---------------------------------------------------------#
  #===============================================================================
  
  sportsbet_h2h <-
    all_main_market_data |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match,
           start_time,
           market_name,
           home_team,
           home_win,
           away_team,
           away_win) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Sportsbet") |>
    mutate(start_time = str_extract(start_time, "\\,.*")) |>
    mutate(start_time = str_remove(start_time, "\\, ")) |>
    mutate(start_time = str_remove(start_time, " \\d{2}\\:\\d{2}")) |>
    mutate(start_time = dmy(paste(start_time, "2023"))) |>
    mutate(start_time = if_else(month(start_time) < 9, start_time + years(1), start_time))
  
  # Write to csv
  write_csv(sportsbet_h2h, "Data/scraped_odds/sportsbet_h2h.csv")
  
}

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

player_props_function <- function() {
  # Function to get team names
  get_team_names <- function(match) {
    team_names <-
      match |>
      html_nodes(".normal_fgzdi7m") |>
      html_text()
    
    # Home team and Away Team
    home_team <- team_names[1]
    away_team <- team_names[2]
    
    # Output
    tibble(home_team, away_team)
  }
  
  # Get match links
  match_links <-
    sportsbet_url |>
    read_html() |>
    html_nodes(".link_ft4u1lp") |>
    html_attr("href")
  
  # Get match IDs from links
  match_ids <-
    match_links |>
    str_extract("\\d{4,10}$") |>
    as.numeric()
  
  # Get data from main market page
  matches <-
    sportsbet_url |>
    read_html() |>
    html_nodes(".White_fqa53j6")
  
  # Get team names that correspond to each match link
  team_names <-
    map_dfr(matches, get_team_names) |>
    bind_cols("match_id" = match_ids)
  
  # Get all links
  top_markets_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/304/Markets"
    )
  run_scorer_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/306/Markets"
    )
  top_wicket_takers_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/307/Markets"
    )
  first_innings_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/325/Markets"
    )
  player_boundaries_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/429/Markets"
    )
  
  # Function to read a url and get the player props-------------------------------
  
  read_prop_url <- function(url) {
    # Make request and get response
    sb_response <-
      request(url) |>
      req_perform() |>
      resp_body_json()
    
    # Empty vectors to append to
    prop_market_name = c()
    selection_name_prop = c()
    prop_market_selection = c()
    prop_market_price = c()
    player_id = c()
    market_id = c()
    handicap = c()
    
    # Loop through each market
    for (market in sb_response) {
      for (selection in market$selections) {
        # Append to vectors
        prop_market_name = c(prop_market_name, market$name)
        selection_name_prop = c(selection_name_prop, selection$name)
        prop_market_selection = c(prop_market_selection, selection$resultType)
        prop_market_price = c(prop_market_price, selection$price$winPrice)
        player_id = c(player_id, selection$externalId)
        market_id = c(market_id, market$externalId)
        if (is.null(selection$unformattedHandicap)) {
          selection$unformattedHandicap = NA
          handicap = c(handicap, selection$unformattedHandicap)
        } else {
          selection$unformattedHandicap = as.numeric(selection$unformattedHandicap)
          handicap = c(handicap, selection$unformattedHandicap)
        }
      }
    }
    
    # Output
    tibble(
      prop_market_name,
      selection_name_prop,
      prop_market_selection,
      prop_market_price,
      player_id,
      market_id,
      handicap,
      url
    )
  }
  
  # Safe version that just returns NULL if there is an error
  safe_read_prop_url <- safely(read_prop_url, otherwise = NULL)
  
  #===============================================================================
  # Top Run Scorers
  #===============================================================================
  
  # Map function to player points urls
  top_run_scorers_data <-
    map(top_markets_links, safe_read_prop_url)
  
  # Get just result part from output
  top_run_scorers_data <-
    top_run_scorers_data |>
    map("result") |>
    map_df(bind_rows) |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team))
  
  #===============================================================================
  # Player Runs
  #===============================================================================
  
  # Map function to player points urls
  player_runs_data <-
    map(run_scorer_links, safe_read_prop_url)
  
  # Get just result part from output
  player_runs_data <-
    player_runs_data |>
    map("result") |>
    map_df(bind_rows) |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team))
  
  # Player runs over / under
  player_runs_overs <-
    player_runs_data |>
    filter(str_detect(prop_market_name, "Total Runs")) |>
    filter(str_detect(selection_name_prop, "Over")) |>
    transmute(
      match,
      market = "Player Runs",
      home_team,
      away_team,
      player_name = str_remove(selection_name_prop, " - Over"),
      line = handicap,
      over_price = prop_market_price
    ) |> 
    mutate(player_name = str_remove(player_name, " Over$"))
  
  player_runs_unders <-
    player_runs_data |>
    filter(str_detect(prop_market_name, "Total Runs")) |>
    filter(str_detect(selection_name_prop, "Under")) |>
    transmute(
      match,
      market = "Player Runs",
      home_team,
      away_team,
      player_name = str_remove(selection_name_prop, " - Under"),
      line = handicap,
      under_price = prop_market_price
    ) |> 
    mutate(player_name = str_remove(player_name, " Under$"))
  
  player_runs_combined <-
    player_runs_overs |>
    full_join(
      player_runs_unders,
      by = c(
        "match",
        "market",
        "home_team",
        "away_team",
        "player_name",
        "line"
      )
    ) |>
    mutate(
      player_name = case_when(
        player_name == "Francois du Plessis" ~ "Faf du Plessis",
        .default = player_name
      )
    ) |>
    mutate(agency = "Sportsbet")
  
  # Player runs alternative lines
  player_runs_alternative_lines <-
    top_run_scorers_data |>
    filter(str_detect(prop_market_name, "Player Multis")) |>
    filter(str_detect(selection_name_prop, "\\+ Runs$")) |>
    mutate(prop_market_name = str_replace(prop_market_name, "Fifty", "50")) |>
    transmute(
      match,
      market = "Player Runs",
      home_team,
      away_team,
      player_name = str_remove(selection_name_prop, " \\d+\\+ Runs$"),
      line = str_extract(selection_name_prop, "[0-9]{1,3}"),
      over_price = prop_market_price
    ) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(agency = "Sportsbet") |>
    mutate(
      player_name = case_when(
        player_name == "Francois du Plessis" ~ "Faf du Plessis",
        .default = player_name
      )
    )
  
  # Combine all
  player_runs_all <-
    bind_rows(player_runs_combined, player_runs_alternative_lines) |>
    arrange(player_name, line) |>
    left_join(ipl_squads_2024, by = c("player_name" = "player")) |>
    mutate(
      opposition_team = case_when(
        team == home_team ~ away_team,
        team == away_team ~ home_team
      )
    ) |>
    relocate(team, opposition_team, .after = player_name) |> 
    rename(player_team = team)
  
  # Write to csv----------------------------------------------------------------
  write_csv(player_runs_all,
            "Data/scraped_odds/sportsbet_player_runs.csv")
  
  #=============================================================================
  # Boundaries
  #=============================================================================
  
  # Map function to player boundaries urls
  player_boundaries_data <-
    map(player_boundaries_links, safe_read_prop_url)
  
  # Get just result part from output
  player_boundaries_data <-
    player_boundaries_data |>
    map("result") |>
    map_df(bind_rows) |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team))
  
  # Over Under boundaries
  player_boundaries_overs <-
    player_boundaries_data |>
    filter(str_detect(prop_market_name, "Number of")) |>
    filter(str_detect(selection_name_prop, "Over")) |>
    transmute(
      match,
      market = str_extract(prop_market_name, "Number of .*"),
      home_team,
      away_team,
      player_name = str_remove(selection_name_prop, " - Over"),
      line = handicap,
      over_price = prop_market_price
    ) |>
    mutate(
      player_name = case_when(
        player_name == "Tom Rogers (Stars)" ~ "Tom F Rogers",
        player_name == "Tom Rogers." ~ "Tom Rogers",
        player_name == "Matt Renshaw" ~ "Matthew Renshaw",
        player_name == "Oliver Davies" ~ "Ollie Davies",
        .default = player_name
      )
    )
  
  player_boundaries_unders <-
    player_boundaries_data |>
    filter(str_detect(prop_market_name, "Number of")) |>
    filter(str_detect(selection_name_prop, "Under")) |>
    transmute(
      match,
      market = str_extract(prop_market_name, "Number of .*"),
      home_team,
      away_team,
      player_name = str_remove(selection_name_prop, " - Under"),
      line = handicap,
      under_price = prop_market_price
    ) |>
    mutate(
      player_name = case_when(
        player_name == "Tom Rogers (Stars)" ~ "Tom F Rogers",
        player_name == "Tom Rogers." ~ "Tom Rogers",
        player_name == "Matt Renshaw" ~ "Matthew Renshaw",
        player_name == "Oliver Davies" ~ "Ollie Davies",
        .default = player_name
      )
    )
  
  player_boundaries_combined <-
    player_boundaries_overs |>
    left_join(
      player_boundaries_unders,
      by = c(
        "match",
        "player_name",
        "market",
        "home_team",
        "away_team",
        "line"
      )
    ) |>
    mutate(agency = "Sportsbet") |>
    mutate(
      player_name = case_when(
        player_name == "Tom Rogers (Stars)" ~ "Tom F Rogers",
        player_name == "Tom Rogers." ~ "Tom Rogers",
        player_name == "Matt Renshaw" ~ "Matthew Renshaw",
        player_name == "Oliver Davies" ~ "Ollie Davies",
        .default = player_name
      )
    ) |>
    left_join(player_teams[, c("player_name", "player_team")], by = "player_name") |>
    mutate(
      opposition_team = case_when(
        player_team == home_team ~ away_team,
        player_team == away_team ~ home_team
      )
    ) |>
    relocate(player_team, opposition_team, .after = player_name)
  
  # Get Player to score a 6 markets
  to_score_a_6 <-
    top_run_scorers_data |>
    filter(str_detect(prop_market_name, "Player To Hit A Six")) |>
    separate(
      match,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    transmute(
      match,
      market = "Number of 6s",
      home_team,
      away_team,
      player_name = selection_name_prop,
      line = 0.5,
      over_price = prop_market_price
    ) |>
    mutate(
      player_name = case_when(
        player_name == "Tom Rogers (Stars)" ~ "Tom F Rogers",
        player_name == "Tom Rogers." ~ "Tom Rogers",
        player_name == "Matt Renshaw" ~ "Matthew Renshaw",
        player_name == "Oliver Davies" ~ "Ollie Davies",
        .default = player_name
      )
    ) |>
    left_join(player_teams[, c("player_name", "player_team")], by = "player_name") |>
    mutate(
      opposition_team = case_when(
        player_team == home_team ~ away_team,
        player_team == away_team ~ home_team
      )
    ) |>
    relocate(player_team, opposition_team, .after = player_name) |>
    mutate(agency = "Sportsbet")
  
  # Write to csv----------------------------------------------------------------
  player_boundaries_combined |>
    bind_rows(to_score_a_6) |>
    write_csv("Data/scraped_odds/sportsbet_player_boundaries.csv")
  
  #=========================================================
  # Player Wickets
  #=========================================================
  
  # Player wickets alternative lines
  player_wickets_alternative_lines <-
    top_run_scorers_data |>
    filter(str_detect(prop_market_name, "Player Multis")) |>
    filter(str_detect(selection_name_prop, "\\+ Wickets$")) |>
    mutate(prop_market_name = str_replace(prop_market_name, "Fifty", "50")) |>
    transmute(
      match,
      market = "Player Wickets",
      home_team,
      away_team,
      player_name = str_remove(selection_name_prop, " \\d+\\+ Wickets$"),
      line = str_extract(selection_name_prop, "[0-9]{1,3}"),
      over_price = prop_market_price
    ) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(agency = "Sportsbet") |>
    mutate(
      player_name = case_when(
        player_name == "Mohammad Siraj" ~ "Mohammed Siraj",
        .default = player_name
      )
    ) |> 
    left_join(ipl_squads_2024, by = c("player_name" = "player")) |>
    mutate(
      opposition_team = case_when(
        team == home_team ~ away_team,
        team == away_team ~ home_team
      )
    ) |>
    relocate(team, opposition_team, .after = player_name) |> 
    rename(player_team = team) |> 
    select(-first_name, -last_name, -first_initial, -join_name)
  
  # Write to csv----------------------------------------------------------------
  player_wickets_alternative_lines |>
    write_csv("Data/scraped_odds/sportsbet_player_wickets.csv")
}

##%######################################################%##
#                                                          #
####                Run functions safely                ####
#                                                          #
##%######################################################%##

safe_main_markets <- safely(main_markets_function, otherwise = NULL)
safe_player_props <- safely(player_props_function, otherwise = NULL)

safe_main_markets()
safe_player_props()
