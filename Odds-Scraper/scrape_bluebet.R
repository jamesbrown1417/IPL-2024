# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# Get fixture and player team data
player_teams <- read_csv("Data/supercoach-data.csv")
fixture <- read_csv("Data/supercoach-fixture.csv")

# Get each teams next match
home_matches <-
  fixture |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(team = home_team, match, start_date) |>
  mutate(start_date = start_date + hours(10) + minutes(30)) |>  
  filter(start_date >= today())

away_matches <-
  fixture |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(team = away_team, match, start_date) |>
  mutate(start_date = start_date + hours(10) + minutes(30)) |>  
  filter(start_date >= today())

# Combine together
all_matches <-
  bind_rows(home_matches, away_matches) |>
  arrange(team, start_date) |> 
  group_by(team) |> 
  slice_head(n = 1) |> 
  ungroup()

# Fix certain problematic player names
player_teams <-
  player_teams |>
  mutate(
    player_name = if_else(
      player_name == "Tom Rogers" &
        player_team == "Melbourne Stars",
      "Tom F Rogers",
      player_name
    )
  )

# URL to get responses
bluebet_url = "https://web20-api.bluebet.com.au/MasterCategory?EventTypeId=109&WithLevelledMarkets=true&format=json"

# Make request and get response
bluebet_response <-
  request(bluebet_url) |>
  req_perform() |> 
  resp_body_json()

# Get matches
matches <- bluebet_response$MasterCategories[[1]]$Categories[[1]]$MasterEvents

# Function to extract market info from response---------------------------------
get_market_info <- function(market) {
  
  # Market info
  markets_name = market$EventName
  market_propositions = market$OutcomeName
  market_prices = market$Price
  
  # Output Tibble
  tibble(market = markets_name,
         propositions = market_propositions,
         prices = market_prices)
}


# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
  # Match info
  match_name = matches$MasterEventName
  match_start_time = matches$MinAdvertisedStartTime
  match_id = matches$MasterEventId
  
  # Market info
  market_info = map(matches$Markets, get_market_info) |> bind_rows()
  
  # Output Tibble
  tibble(
    match = match_name,
    match_id = match_id,
    start_time = match_start_time,
    market_name = market_info$market,
    propositions = market_info$propositions,
    prices = market_info$prices
  )
}

# Map functions to data
all_bluebet_markets <-
  map(matches, get_match_info) |> bind_rows()

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
  all_bluebet_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Twenty20 Match")) |> 
  mutate(market_name = "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 1) |> 
  rename(home_win = prices) |> 
  select(-propositions)

# Away teams
away_teams <-
  all_bluebet_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Twenty20 Match")) |> 
  mutate(market_name = "Head To Head") |>
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = prices) |> 
  select(-propositions)

# Combine
bluebet_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "BlueBet")

# Fix team names
bluebet_head_to_head_markets <-
  bluebet_head_to_head_markets |> 
  mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(bluebet_head_to_head_markets, "Data/scraped_odds/bluebet_h2h.csv")

#===============================================================================
# Player Props
#===============================================================================

# Get API URL for each market type----------------------------------------------

# Player Runs
player_runs_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G65&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Fours Links
player_fours_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G83&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Sixes Links
player_sixes_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G114&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Wickets Links
player_wickets_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G236&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Function to extract prop data from links--------------------------------------

get_prop_data <- function(link) {
  
  # Get response
  response <-
    request(link) |>
    req_perform() |> 
    resp_body_json()
  
  # Empty vectors to append to
  event_name <- c()
  outcome_title <- c()
  outcome_name <- c()
  price <- c()
  
  for (event in response$Events) {
    for (outcome in event$Outcomes) {
      event_name <- c(event_name, event$EventName)
      outcome_title <- c(outcome_title, outcome$EventName)
      outcome_name <- c(outcome_name, outcome$OutcomeName)
      price <- c(price, outcome$Price)
    }
  }
  
  # Output Tibble
  tibble(
    event_name = event_name,
    outcome_title = outcome_title,
    outcome_name = outcome_name,
    price = price
  )
}

# Safe version of function
safe_get_prop_data <- safely(get_prop_data)

# Get player runs data--------------------------------------------------------

bluebet_player_runs <-
  map(player_runs_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |>
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+")) - 0.5) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\+ ")) |>
  mutate(
    player_name = case_when(
      player_name == "Steven Smith" ~ "Steve Smith",
      .default = player_name)) |>
  left_join(player_teams[,c("player_name", "player_team")], by = "player_name") |> 
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team)) |>
  rename(market = market_name) |> 
  mutate(agency = "BlueBet") |>
  mutate(market = "Player Runs") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team"
  )

# Get player fours data--------------------------------------------------------

bluebet_player_fours <-
  map(player_fours_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |>
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+")) - 0.5) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\+ ")) |>
  mutate(
    player_name = case_when(
      player_name == "Steven Smith" ~ "Steve Smith",
      .default = player_name)) |>
  left_join(player_teams[,c("player_name", "player_team")], by = "player_name") |> 
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team)) |>
  rename(market = market_name) |> 
  mutate(agency = "BlueBet") |>
  mutate(market = "Number of 4s") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team"
  )

  # Get player sixes data--------------------------------------------------------
  
  bluebet_player_sixes <-
    map(player_sixes_links, safe_get_prop_data) |> 
    map("result") |>
    bind_rows() |>
    separate(outcome_name, into = c("market_name", "player_name"), sep = " - ") |>
    mutate(line = as.numeric(str_extract(player_name, "\\d+")) - 0.5) |>
    mutate(match = str_extract(event_name, "\\(.*\\)")) |>
    mutate(match = str_remove_all(match, "\\(|\\)")) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
    mutate(player_name = str_remove_all(player_name, " \\d+\\+ ")) |>
    mutate(
      player_name = case_when(
        player_name == "Steven Smith" ~ "Steve Smith",
        .default = player_name)) |>
    left_join(player_teams[,c("player_name", "player_team")], by = "player_name") |> 
    mutate(opposition_team = case_when(
      player_team == home_team ~ away_team,
      player_team == away_team ~ home_team)) |>
    rename(market = market_name) |> 
    mutate(agency = "BlueBet") |>
    mutate(market = "Number of 6s") |>
    select(
      "match",
      "home_team",
      "away_team",
      "market",
      "player_name",
      "player_team",
      "line",
      "over_price" = "price",
      "agency",
      "opposition_team"
    )
  
  # Get player wickets data--------------------------------------------------------
  
  bluebet_player_wickets <-
    map(player_wickets_links, safe_get_prop_data) |> 
    map("result") |>
    bind_rows() |>
    separate(outcome_name, into = c("market_name", "player_name"), sep = " - ") |>
    mutate(line = as.numeric(str_extract(player_name, "\\d+")) - 0.5) |>
    mutate(match = str_extract(event_name, "\\(.*\\)")) |>
    mutate(match = str_remove_all(match, "\\(|\\)")) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
    mutate(player_name = str_remove_all(player_name, " \\d+\\+ ")) |>
    mutate(
      player_name = case_when(
        player_name == "Steve O’Keefe" ~ "Steve O'Keefe",
        player_name == "AJ Tye" ~ "Andrew Tye",
        .default = player_name)) |>
    left_join(player_teams[,c("player_name", "player_team")], by = "player_name") |> 
    mutate(opposition_team = case_when(
      player_team == home_team ~ away_team,
      player_team == away_team ~ home_team)) |>
    rename(market = market_name) |> 
    mutate(agency = "BlueBet") |>
    mutate(market = "Player Wickets") |>
    select(
      "match",
      "home_team",
      "away_team",
      "market",
      "player_name",
      "player_team",
      "line",
      "over_price" = "price",
      "agency",
      "opposition_team"
    )
  
#===============================================================================
# Write to CSV
#===============================================================================

# Player Runs
write_csv(bluebet_player_runs, "Data/scraped_odds/bluebet_player_runs.csv")
  
# Player Wickets
write_csv(bluebet_player_wickets, "Data/scraped_odds/bluebet_player_wickets.csv")
  
# Player Boundaries
bind_rows(bluebet_player_fours, bluebet_player_sixes) |> 
  write_csv("Data/scraped_odds/bluebet_player_boundaries.csv")
