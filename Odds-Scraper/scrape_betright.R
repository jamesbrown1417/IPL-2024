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
betright_url = "https://next-api.betright.com.au/Sports/Category?categoryId=18197"

# Make request and get response
betright_response <-
  request(betright_url) |>
  req_perform() |> 
  resp_body_json()

# Get index of element that equals "Australia"
australia_index <-
betright_response$masterCategories |> 
  map("masterCategory") |>
  unlist()

australia_index <- which(australia_index == "Australia")

# Get matches
matches <- betright_response$masterCategories[[australia_index]]$categories[[1]]$masterEvents

# Function to extract market info from response---------------------------------
get_market_info <- function(market) {
  
  # Market info
  markets_name = market$eventName
  market_propositions = market$outcomeName
  market_prices = market$price
  
  # Output Tibble
  tibble(market = markets_name,
         propositions = market_propositions,
         prices = market_prices)
}


# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
  # Match info
  match_name = matches$masterEventName
  match_start_time = matches$minAdvertisedStartTime
  match_id = matches$masterEventId
  
  # Market info
  market_info = map(matches$markets, get_market_info) |> bind_rows()
  
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
all_betright_markets <-
  map(matches, get_match_info) |>
  bind_rows() |>
  filter(!is.na(prices))

# Matches to join
join_matches <-
  all_betright_markets |> 
  select(match, link = match_id)

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
  all_betright_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(market_name = "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 1) |> 
  rename(home_win = prices) |> 
  select(-propositions)

# Away teams
away_teams <-
  all_betright_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(market_name = "Head To Head") |>
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = prices) |> 
  select(-propositions)

# Combine
betright_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "BetRight")

# Fix team names
betright_head_to_head_markets <-
  betright_head_to_head_markets |> 
  mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(betright_head_to_head_markets, "Data/scraped_odds/betright_h2h.csv")

#===============================================================================
# Player Props
#===============================================================================

# Get API URL for each market type----------------------------------------------

# Player Runs
player_runs_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G313&format=json")

# Player Boundaries Links
player_boundaries_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G65&format=json")

# Player Wickets Links
player_wickets_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G314&format=json")

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
  
  for (event in response$events) {
    for (outcome in event$outcomes) {
      event_name <- c(event_name, event$eventName)
      outcome_title <- c(outcome_title, outcome$eventName)
      outcome_name <- c(outcome_name, outcome$outcomeName)
      price <- c(price, outcome$price)
    }
  }
  
  # Output Tibble
  tibble(
    event_name = event_name,
    outcome_title = outcome_title,
    outcome_name = outcome_name,
    price = price,
    link = link
  )
}

# Safe version of function
safe_get_prop_data <- safely(get_prop_data)

# Get player runs data--------------------------------------------------------

betright_player_runs <-
  map(player_runs_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |>
  filter(str_detect(outcome_title, "Player Runs")) |>
  mutate(market = "Player Runs") |>
  mutate(player_name = str_remove_all(outcome_title, "Player Runs")) |>
  mutate(player_name = str_remove_all(player_name, "-")) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |> 
  mutate(player_name = str_trim(player_name)) |>
  mutate(line = ifelse(str_detect(outcome_name, "\\+"),
                       as.numeric(str_extract(outcome_name, "\\d+")) - 0.5,
                       as.numeric(str_extract(outcome_name, "\\d+\\.\\d+")))) |>
  mutate(link = as.integer(str_extract(link, "[0-9]{4,8}"))) |>
  left_join(join_matches, by = "link", relationship = "many-to-many") |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(
    player_name = case_when(
      player_name == "Steven Smith" ~ "Steve Smith",
      player_name == "Jake FraserMcGurk" ~ "Jake Fraser-McGurk",
      player_name == "Quinton De Kock" ~ "Quinton de Kock",
      .default = player_name)) |>
  left_join(player_teams[,c("player_name", "player_team")], by = "player_name") |> 
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team)) |>
  mutate(agency = "BetRight") |>
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

betright_player_fours <-
  map(player_boundaries_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |>
  filter(str_detect(outcome_title, "Player Fours")) |>
  mutate(market = "Number of 4s") |>
  mutate(player_name = str_remove_all(outcome_title, "Player Fours")) |>
  mutate(player_name = str_remove_all(player_name, "-")) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |> 
  mutate(player_name = str_trim(player_name)) |>
  mutate(line = ifelse(str_detect(outcome_name, "\\+"),
                       as.numeric(str_extract(outcome_name, "\\d+")) - 0.5,
                       as.numeric(str_extract(outcome_name, "\\d+\\.\\d+")))) |>
  mutate(link = as.integer(str_extract(link, "[0-9]{4,8}"))) |>
  left_join(join_matches, by = "link", relationship = "many-to-many") |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(
    player_name = case_when(
      player_name == "Steven Smith" ~ "Steve Smith",
      player_name == "Jake FraserMcGurk" ~ "Jake Fraser-McGurk",
      player_name == "Quinton De Kock" ~ "Quinton de Kock",
      .default = player_name)) |>
  left_join(player_teams[,c("player_name", "player_team")], by = "player_name") |> 
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team)) |>
  mutate(agency = "BetRight") |>
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

betright_player_sixes <-
  map(player_boundaries_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |>
  filter(str_detect(outcome_title, "Player Sixes")) |>
  mutate(market = "Number of 6s") |>
  mutate(player_name = str_remove_all(outcome_title, "Player Sixes")) |>
  mutate(player_name = str_remove_all(player_name, "-")) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |> 
  mutate(player_name = str_trim(player_name)) |>
  mutate(line = ifelse(str_detect(outcome_name, "\\+"),
                       as.numeric(str_extract(outcome_name, "\\d+")) - 0.5,
                       as.numeric(str_extract(outcome_name, "\\d+\\.\\d+")))) |>
  mutate(link = as.integer(str_extract(link, "[0-9]{4,8}"))) |>
  left_join(join_matches, by = "link", relationship = "many-to-many") |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(
    player_name = case_when(
      player_name == "Steven Smith" ~ "Steve Smith",
      player_name == "Jake FraserMcGurk" ~ "Jake Fraser-McGurk",
      player_name == "Quinton De Kock" ~ "Quinton de Kock",
      .default = player_name)) |>
  left_join(player_teams[,c("player_name", "player_team")], by = "player_name") |> 
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team)) |>
  mutate(agency = "BetRight") |>
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

betright_player_wickets <-
  map(player_wickets_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |>
  filter(str_detect(outcome_title, "Player Wickets")) |>
  mutate(market = "Player Wickets") |>
  mutate(player_name = str_remove_all(outcome_title, "Player Wickets")) |>
  mutate(player_name = str_remove_all(player_name, "-")) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |> 
  mutate(player_name = str_trim(player_name)) |>
  mutate(line = ifelse(str_detect(outcome_name, "\\+"),
                       as.numeric(str_extract(outcome_name, "\\d+")) - 0.5,
                       as.numeric(str_extract(outcome_name, "\\d+\\.\\d+")))) |>
  mutate(link = as.integer(str_extract(link, "[0-9]{4,8}"))) |>
  left_join(join_matches, by = "link", relationship = "many-to-many") |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(
    player_name = case_when(
      player_name == "Micheal Neser" ~ "Michael Neser",
      .default = player_name)) |>
  left_join(player_teams[,c("player_name", "player_team")], by = "player_name") |> 
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team)) |>
  mutate(agency = "BetRight") |>
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
write_csv(betright_player_runs, "Data/scraped_odds/betright_player_runs.csv")

# Player Wickets
write_csv(betright_player_wickets, "Data/scraped_odds/betright_player_wickets.csv")

# Player Boundaries
bind_rows(betright_player_fours, betright_player_sixes) |> 
  write_csv("Data/scraped_odds/betright_player_boundaries.csv")
