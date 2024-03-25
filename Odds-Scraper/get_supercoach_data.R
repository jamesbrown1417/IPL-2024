#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(httr2)
library(googlesheets4)
library(googledrive)

`%notin%` <- Negate(`%in%`)

safe_subscript <- function(x, index) {
  if (index <= length(x) && index > 0) {
    return(x[[index]])
  } else {
    return(NULL)
  }
}

# Supercoach API URL
url = "https://supercoach.heraldsun.com.au/2023/api/bbl/classic/v1/players-cf?round=0"

# Make request
req <- request(url)

# Get response
resp <- req_perform(req)

# Process response
all_data <- resp |> resp_body_json()

# Create a function to extract the data from the json list for each player
get_supercoach_data <- function(player_data) {
  tibble(
    player_id = player_data$id,
    player_name = paste(player_data$first_name, player_data$last_name),
    player_first_name = player_data$first_name,
    player_last_name = player_data$last_name,
    player_team = player_data$team$name,
    player_team_id = player_data$team_id,
    previous_games = player_data$previous_games,
    previous_average = player_data$previous_average,
    previous_total = player_data$previous_total,
    injury_suspension_status = player_data$injury_suspension_status,
    injury_suspension_status_text = player_data$injury_suspension_status_text,
    locked = player_data$locked,
    active = player_data$active,
    played_status = player_data$played_status$display,
    supercoach_price = player_data$player_stats[[1]]$price,
    points = player_data$player_stats[[1]]$points_scored,
    games = player_data$player_stats[[1]]$games
  )
}

# Map to list
extracted_data <-
  map(all_data, get_supercoach_data) |> 
  bind_rows()

# Write out to csv
write_csv(extracted_data, "Data/supercoach-data.csv")

# Get fixture
fixture_url = "https://supercoach.heraldsun.com.au/2023/api/bbl/classic/v1/real_fixture?page=1&page_size=9998"

# Make request
req <- request(fixture_url)

# Get response
resp <- req_perform(req)

# Process response
fixture_data <- resp |> resp_body_json()

# Create a function to extract the data from the json list for each player
get_supercoach_data <- function(match_data) {
  tibble(
    home_team = match_data$team1$name,
    away_team = match_data$team2$name,
    venue = match_data$venue$name,
    round = match_data$round,
    start_date = ymd_hms(match_data$kickoff, tz = "Australia/Adelaide"),
  )
}

# Map to list
extracted_data <-
  map(fixture_data, get_supercoach_data) |> 
  bind_rows()

# Write out to csv
write_csv(extracted_data, "Data/supercoach-fixture.csv")
