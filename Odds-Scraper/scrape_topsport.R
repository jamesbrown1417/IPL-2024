# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# URL of website
topsport_url = "https://www.topsport.com.au/Sport/Cricket/IPL_Matches/Matches"

# Squads 2024
ipl_squads_2024 <- read_csv("Data/ipl_squads_2024.csv")

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

# Get data from main market page
main_markets <-
  topsport_url |> 
  read_html() |>
  html_nodes(".marketTable") |> 
  html_table()

#===============================================================================
# Use rvest to get additional market information-------------------------------#
#===============================================================================

# Get links to other markets
topsport_other_markets <-
  topsport_url |>
  read_html() |>
  html_nodes("dd") |> 
  html_attr("data-compurl")

# Remove NA
topsport_other_markets <- topsport_other_markets[!is.na(topsport_other_markets)]

# Remove ?issubcomp=true
topsport_other_markets <- str_remove(topsport_other_markets, "\\?issubcomp=true")

# Add base url
topsport_other_markets <- paste0("https://www.topsport.com.au", topsport_other_markets)

#===============================================================================
# Head to Head markets---------------------------------------------------------#
#===============================================================================

head_to_head_main <- function() {
  
  # Function to get head to head data--------------------------------------------#
  get_h2h <- function(market_table) {
    
    # Home Team Data
    home_info <- market_table[3, 1:2]
    names(home_info) <- c("home_team", "home_win")
    
    # Away Team Data
    away_info <- market_table[2, 1:2]
    names(away_info) <- c("away_team", "away_win")
    
    # Match Start Time
    match_start_time <- market_table[1, 1]
    names(match_start_time) <- "start_time"
    
    # Combine together into one table
    bind_cols(home_info, away_info, match_start_time)
    
  }
  
  # Map function to main markets list
  topsport_h2h <- map(main_markets, get_h2h) |> bind_rows()
  
  # Fix names
  topsport_h2h <-
    topsport_h2h |> 
    # mutate(home_team = fix_team_names(home_team)) |>
    # mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TopSport") |> 
    mutate(start_time = dmy_hm(start_time)) |>
    filter(!is.na(home_win))
  
  # Write to csv
  write_csv(topsport_h2h, "Data/scraped_odds/topsport_h2h.csv")
}

#===============================================================================
# Player Runs
#===============================================================================

# Function to read the html of a given url
read_topsport_html <- function(url) {
  
  # Get market name from url
  market_name <- str_extract(url, "(?<=Cricket/).*")
  market_name <- str_remove(market_name, "\\/.*$")
  
  # Get line from market name
  line <- str_extract(market_name, "\\d+")
  
  # Get match name from html
  match_name_html <-    
    url |> 
    read_html() |>
    html_nodes("h1") |>
    html_text() |> 
    paste(collapse = " ")
  
  # Get match name from extracted text
  match_name <- str_extract(match_name_html, "[A-Z]{1}[A-Za-z ]+ v [A-Z]{1}[A-Za-z ]+")
  
  # Get player name from extracted text
  player_name <- str_split(match_name_html, " - ")[[1]][2]
  
  # Get data from html
  result <-    
    url |> 
    read_html() |>
    html_nodes(".marketTable") |> 
    html_table()
  
  # Get tibble
  result[[1]] |>
    mutate(line = as.numeric(line)) |> 
    mutate(match = match_name,
           player = player_name)
}

# Get data for pick your own player runs----------------------------------------

# Get URLs
pick_your_own_runs_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Runs_Scored")]

# Map function
player_runs_alternate <-
  map(pick_your_own_runs_markets, read_topsport_html, .progress = TRUE) |> 
  bind_rows() |> 
  mutate(line = line - 0.5) |>
  rename(over_price = Win) |> 
  rename(player_name = Selection) |> 
  mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
  relocate(match, .before = player_name)

# Add player team
player_runs_alternate <-
  player_runs_alternate |>
  mutate(player_name = ifelse(
    player_name == "Francois du Plessis",
    "Faf du Plessis",
    player_name
  )) |>
  left_join(ipl_squads_2024, by = c("player_name" = "player")) |>
  mutate(market_name = "Player Runs", agency = "TopSport") |>
  select(match,
         market_name,
         player_name,
         player_team = team,
         line,
         over_price,
         agency)

# Get data for player runs over/under-----------------------------------------

# Get URLs
player_runs_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Total_Runs_.*\\(")]

# Only proceed if markets have been picked up above
if (length(player_runs_markets) > 0) {

  # Map function
  player_runs_lines <-
    map(player_runs_markets, read_topsport_html) |> 
    bind_rows() |> 
    rename(player_name = player) |> 
    mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
    mutate(line = str_extract(Selection, "\\d+\\.\\d+")) |>
    mutate(line = as.numeric(line)) |>
    rename(over_price = Win)
    
  # Get Overs
  player_runs_lines_overs <-
    player_runs_lines |> 
    filter(str_detect(Selection, "Over")) |> 
    select(-Selection)
  
  # Get Unders
  player_runs_lines_unders <-
    player_runs_lines |> 
    filter(str_detect(Selection, "Under")) |>
    rename(under_price = over_price) |> 
    select(-Selection)
  
  # Combine
  player_runs_lines <- 
    player_runs_lines_overs |> 
    left_join(player_runs_lines_unders) |> 
    mutate(market_name = "Player Runs") |> 
    mutate(agency = "TopSport") |> 
    mutate(player_name = ifelse(player_name == "Francois du Plessis", "Faf du Plessis", player_name)) |>
    left_join(ipl_squads_2024, by = c("player_name" = "player")) |>
    select(match,
           market_name,
           player_name,
           player_team = team,
           line,
           over_price,
           under_price,
           agency)

  }
  
# Write out all player runs
player_runs <- 
  player_runs_alternate |>
  bind_rows(player_runs_lines) |>
  mutate(over_price = as.numeric(over_price)) |>
  mutate(under_price = as.numeric(under_price)) |>
  mutate(market_name = "Player Runs") |>
  mutate(agency = "TopSport") |>
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(
    opposition_team = case_when(
      player_team == home_team ~ away_team,
      player_team == away_team ~ home_team
    )
  ) |>
  select(
    match,
    market = market_name,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency
  ) |> 
  arrange(player_name, line) |> 
  distinct(match, player_name, player_team, opposition_team, line, over_price, under_price, .keep_all = TRUE)

player_runs |> 
  write_csv("Data/scraped_odds/topsport_player_runs.csv")

#===============================================================================
# Player Wickets
#===============================================================================

# Get data for pick your own bowler wickets-------------------------------------

# Get URLs
pick_your_own_wickets_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Wicket[s]?_or_More")]

# Map function
player_wickets_alternate <-
  map(pick_your_own_wickets_markets, read_topsport_html) |> 
  bind_rows() |> 
  mutate(line = line - 0.5) |>
  rename(over_price = Win) |> 
  rename(player_name = Selection) |> 
  mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
  relocate(match, .before = player_name)

# Add player team
player_wickets_alternate <-
  player_wickets_alternate |>
  # mutate(player_name = ifelse(player_name == "Nathan Coulter Nile", "Nathan Coulter-Nile", player_name)) |>
  left_join(ipl_squads_2024, by = c("player_name" = "player")) |>
  mutate(market_name = "Player Wickets", agency = "TopSport") |>
  select(match,
         market_name,
         player_name,
         player_team = team,
         line,
         over_price,
         agency)

# Get data for player wickets over/under-----------------------------------------

# Get URLs
player_wickets_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Total_Wickets_.*\\(")]

# Only proceed if markets have been picked up above
if (length(player_wickets_markets) > 0) {
  
  # Map function
  player_wickets_lines <-
    map(player_wickets_markets, read_topsport_html) |> 
    bind_rows() |> 
    mutate(line = str_extract(Selection, "\\d+\\.\\d+")) |>
    mutate(line = as.numeric(line)) |>
    rename(over_price = Win) |> 
    rename(player_name = player) |> 
    mutate(player_name = str_remove(player_name, " \\(.*\\)$"))
  
  # Get Overs
  player_wickets_lines_overs <-
    player_wickets_lines |> 
    filter(str_detect(Selection, "Over")) |> 
    select(-Selection) |> 
    distinct(over_price, line, player_name, .keep_all = TRUE)
  
  # Get Unders
  player_wickets_lines_unders <-
    player_wickets_lines |> 
    filter(str_detect(Selection, "Under")) |>
    rename(under_price = over_price) |> 
    select(-Selection) |> 
    distinct(under_price, line, player_name, .keep_all = TRUE)
  
  # Combine
  player_wickets_lines <- 
    player_wickets_lines_overs |> 
    left_join(player_wickets_lines_unders, by = c("player_name", "line", "match")) |> 
    mutate(market_name = "Player Wickets") |> 
    mutate(agency = "TopSport") |> 
    mutate(player_name = ifelse(player_name == "Nathan Coulter Nile", "Nathan Coulter-Nile", player_name)) |>
    left_join(ipl_squads_2024, by = c("player_name" = "player")) |>
    select(match, market_name, player_name, player_team = team, line, over_price, under_price, agency)
}

# Write out all player wickets
player_wickets <- 
  player_wickets_alternate |>
  bind_rows(player_wickets_lines) |>
  mutate(over_price = as.numeric(over_price)) |>
  mutate(under_price = as.numeric(under_price)) |>
  mutate(market_name = "Player Wickets") |>
  mutate(agency = "TopSport") |>
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(
    opposition_team = case_when(
      player_team == home_team ~ away_team,
      player_team == away_team ~ home_team
    )
  ) |>
  select(
    match,
    market = market_name,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency
  ) |> 
  arrange(player_name, line) |> 
  distinct(match, player_name, player_team, opposition_team, line, over_price, under_price, .keep_all = TRUE)

player_wickets |> 
  write_csv("Data/scraped_odds/topsport_player_wickets.csv")

#===============================================================================
# Boundaries
#===============================================================================

# Get data for pick your own fours----------------------------------------------

# Get URLs
pick_your_own_fours_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Fours")]

# Map function
player_fours_alternate <-
  map(pick_your_own_fours_markets, read_topsport_html) |> 
  bind_rows() |> 
  mutate(line = line - 0.5) |>
  rename(over_price = Win) |> 
  rename(player_name = Selection) |> 
  mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
  relocate(match, .before = player_name)

# Add player team
player_fours_alternate <-
  player_fours_alternate |>
  mutate(
    player_name = ifelse(
      player_name == "Francois du Plessis",
      "Faf du Plessis",
      player_name
    )
  ) |>
  mutate(player_name = ifelse(player_name == "DArcy Short", "D'Arcy Short", player_name)) |>
  left_join(ipl_squads_2024, by = c("player_name" = "player")) |>
  mutate(market_name = "Number of 4s", agency = "TopSport") |>
  select(match,
         market_name,
         player_name,
         player_team = team,
         line,
         over_price,
         agency) |> 
  filter(!is.na(player_team))

# Get data for pick your own sixes----------------------------------------------

# Get URLs
pick_your_own_sixes_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Fours")]

# Map function
player_sixes_alternate <-
  map(pick_your_own_sixes_markets, read_topsport_html) |> 
  bind_rows() |> 
  mutate(line = line - 0.5) |>
  rename(over_price = Win) |> 
  rename(player_name = Selection) |> 
  mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
  relocate(match, .before = player_name)

# Add player team
player_sixes_alternate <-
  player_sixes_alternate |>
  mutate(
    player_name = ifelse(
      player_name == "Francois du Plessis",
      "Faf du Plessis",
      player_name
    )
  ) |>
  mutate(player_name = ifelse(player_name == "DArcy Short", "D'Arcy Short", player_name)) |>
  left_join(ipl_squads_2024, by = c("player_name" = "player")) |>
  mutate(market_name = "Number of 6s", agency = "TopSport") |>
  select(match,
         market_name,
         player_name,
         player_team = team,
         line,
         over_price,
         agency) |> 
  filter(!is.na(player_team))

# Write out all player boundaries
player_boundaries <- 
  player_fours_alternate |>
  bind_rows(player_sixes_alternate) |>
  mutate(over_price = as.numeric(over_price)) |>
  mutate(agency = "TopSport") |>
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(
    opposition_team = case_when(
      player_team == home_team ~ away_team,
      player_team == away_team ~ home_team
    )
  ) |>
  select(
    match,
    market = market_name,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    agency
  ) |> 
  arrange(player_name, line) |> 
  distinct(match, player_name, market, player_team, opposition_team, line, over_price, .keep_all = TRUE)

player_boundaries |> 
  write_csv("Data/scraped_odds/topsport_player_boundaries.csv")
