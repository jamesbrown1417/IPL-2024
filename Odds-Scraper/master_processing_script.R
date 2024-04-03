#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(googlesheets4)
library(googledrive)
`%notin%` <- Negate(`%in%`)

# Directory of scraped odds data
scraped_odds_dir <- "Data/scraped_odds"

# # Get empirical probability function
# source("Scripts/04-get-empirical-probabilities.R")

# # Run all odds scraping scripts-----------------------------------------------
run_scraping <- function(script_name) {
  tryCatch({
    source(script_name)
  }, error = function(e) {
    cat("Odds not released yet for:", script_name, "\n")
  })
}

# Run all odds scraping scripts
run_scraping("Odds-Scraper/scrape_sportsbet.R")
run_scraping("Odds-Scraper/scrape_TAB.R")
run_scraping("Odds-Scraper/scrape_topsport.R")
# run_scraping("Odds-Scraper/scrape_pointsbet.R")
run_scraping("Odds-Scraper/scrape_neds.R")
# run_scraping("Odds-Scraper/scrape_bluebet.R")
# run_scraping("Odds-Scraper/scrape_betright.R")

# # Google sheets authentification -----------------------------------------------
# options(gargle_oauth_cache = ".secrets")
# drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
# gs4_auth(token = drive_token())
# sheet <- gs4_find("Big Bash Data")

#===============================================================================
# Player Runs Data
#===============================================================================

# List all files in the directory that match player runs
player_runs_files <- list.files(scraped_odds_dir, pattern = "player_runs", full.names = TRUE)

# Read in all files
player_runs_data <- 
  player_runs_files |> 
  map_dfr(read_csv, col_types = cols(.default = col_character())) |> 
  arrange(player_name, line, desc(over_price)) |> 
  mutate(over_price = as.numeric(over_price),
         under_price = as.numeric(under_price)) |> 
  mutate(implied_prob_over = 1/over_price) |> 
  mutate(implied_prob_under = 1/under_price) |>
  mutate(line = as.numeric(line)) |>
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = round((max_implied_prob - min_implied_prob), 2)) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |> 
  arrange(desc(variation), player_name, desc(over_price))

# Add empirical probabilities---------------------------------------------------

# # Runs
# distinct_run_combos <-
#   player_runs_data |> 
#   distinct(name = player_name, line) |> 
#   mutate(stat = "runs")
# 
# player_emp_probs_2022_24 <-
#   pmap(distinct_run_combos, get_empirical_prob, .progress = TRUE) |> 
#   bind_rows() |> 
#   select(player_name, line, games_played, empirical_prob)

# # Add to main file
# player_runs_data <-
#   player_runs_data |> 
#   left_join(player_emp_probs_2022_24, by = c("player_name", "line")) |> 
#   rename(empirical_prob_over = empirical_prob) |> 
#   mutate(empirical_prob_under = 1 - empirical_prob_over) |> 
#   mutate(diff_over = empirical_prob_over - implied_prob_over,
#          diff_under = empirical_prob_under - implied_prob_under) |> 
#   mutate(implied_prob_over = round(implied_prob_over, 2),
#          implied_prob_under = round(implied_prob_under, 2),
#          empirical_prob_over = round(empirical_prob_over, 2),
#          empirical_prob_under = round(empirical_prob_under, 2),
#          diff_over = round(diff_over, 2),
#          diff_under = round(diff_under, 2)) |> 
#   distinct(player_name, line, agency, over_price, .keep_all = TRUE)

# Write out data----------------------------------------------------------------
player_runs_data |> 
  write_rds("Data/processed_odds/all_player_runs.rds")

# # Add to google sheets
# sheet_write(sheet, data = player_runs_data, sheet = "Player Runs")

#===============================================================================
# Player Wickets Data
#===============================================================================

# List all files in the directory that match player wickets
player_wickets_files <- list.files(scraped_odds_dir, pattern = "player_wickets", full.names = TRUE)

# Read in all files
player_wickets_data <- 
  player_wickets_files |> 
  map_dfr(read_csv, col_types = cols(.default = col_character())) |> 
  arrange(player_name, line, desc(over_price)) |> 
  mutate(over_price = as.numeric(over_price),
         under_price = as.numeric(under_price)) |> 
  mutate(implied_prob_over = 1/over_price) |> 
  mutate(implied_prob_under = 1/under_price) |>
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = round((max_implied_prob - min_implied_prob), 2)) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |> 
  arrange(desc(variation), player_name, desc(over_price))

# Add empirical probabilities---------------------------------------------------

# # Wickets
# distinct_wicket_combos <-
#   player_wickets_data |> 
#   distinct(name = player_name, line) |> 
#   mutate(stat = "wickets")
# 
# player_emp_probs_2022_24 <-
#   pmap(distinct_wicket_combos, get_empirical_prob, .progress = TRUE) |> 
#   bind_rows() |> 
#   select(player_name, line, games_played, empirical_prob)
# 
# # Add to main file
# player_wickets_data <-
#   player_wickets_data |> 
#   left_join(player_emp_probs_2022_24, by = c("player_name", "line")) |> 
#   rename(empirical_prob_over = empirical_prob) |> 
#   mutate(empirical_prob_under = 1 - empirical_prob_over) |> 
#   mutate(diff_over = empirical_prob_over - implied_prob_over,
#          diff_under = empirical_prob_under - implied_prob_under) |> 
#   mutate(implied_prob_over = round(implied_prob_over, 2),
#          implied_prob_under = round(implied_prob_under, 2),
#          empirical_prob_over = round(empirical_prob_over, 2),
#          empirical_prob_under = round(empirical_prob_under, 2),
#          diff_over = round(diff_over, 2),
#          diff_under = round(diff_under, 2)) |> 
#   distinct(player_name, line, agency, over_price, .keep_all = TRUE)

# Write out data----------------------------------------------------------------
player_wickets_data |> 
  write_rds("Data/processed_odds/all_player_wickets.rds")

# # Add to google sheets
# sheet_write(sheet, data = player_wickets_data, sheet = "Player Wickets")

#===============================================================================
# Player Fours
#===============================================================================

# List all files in the directory that match player boundaries
player_boundaries_files <- list.files(scraped_odds_dir, pattern = "player_boundaries", full.names = TRUE)

# Read in all files
player_fours_data <- 
  player_boundaries_files |> 
  map_dfr(read_csv, col_types = cols(.default = col_character())) |> 
  arrange(player_name, line, desc(over_price)) |> 
  filter(market == "Number of 4s") |>
  mutate(over_price = as.numeric(over_price),
         under_price = as.numeric(under_price)) |> 
  mutate(implied_prob_over = 1/over_price) |> 
  mutate(implied_prob_under = 1/under_price) |>
  group_by(player_name, market, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = round((max_implied_prob - min_implied_prob), 2)) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |> 
  arrange(desc(variation), player_name, desc(over_price))

# Add empirical probabilities---------------------------------------------------

# # Boundaries
# distinct_four_combos <-
#   player_fours_data |> 
#   distinct(name = player_name, line) |> 
#   mutate(stat = "fours")
# 
# player_emp_probs_2022_24 <-
#   pmap(distinct_four_combos, get_empirical_prob, .progress = TRUE) |> 
#   bind_rows() |> 
#   select(player_name, line, games_played, empirical_prob)
# 
# # Add to main file
# player_fours_data <-
#   player_fours_data |> 
#   left_join(player_emp_probs_2022_24, by = c("player_name", "line")) |> 
#   rename(empirical_prob_over = empirical_prob) |> 
#   mutate(empirical_prob_under = 1 - empirical_prob_over) |> 
#   mutate(diff_over = empirical_prob_over - implied_prob_over,
#          diff_under = empirical_prob_under - implied_prob_under) |> 
#   mutate(implied_prob_over = round(implied_prob_over, 2),
#          implied_prob_under = round(implied_prob_under, 2),
#          empirical_prob_over = round(empirical_prob_over, 2),
#          empirical_prob_under = round(empirical_prob_under, 2),
#          diff_over = round(diff_over, 2),
#          diff_under = round(diff_under, 2))

#===============================================================================
# Player Sixes
#===============================================================================

# Read in all files
player_sixes_data <- 
  player_boundaries_files |> 
  map_dfr(read_csv, col_types = cols(.default = col_character())) |> 
  arrange(player_name, line, desc(over_price)) |> 
  filter(market == "Number of 6s") |>
  mutate(over_price = as.numeric(over_price),
         under_price = as.numeric(under_price)) |> 
  mutate(implied_prob_over = 1/over_price) |> 
  mutate(implied_prob_under = 1/under_price) |>
  group_by(player_name, market, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = round((max_implied_prob - min_implied_prob), 2)) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |> 
  arrange(desc(variation), player_name, desc(over_price))

# Add empirical probabilities---------------------------------------------------

# # Boundaries
# distinct_six_combos <-
#   player_sixes_data |> 
#   distinct(name = player_name, line) |> 
#   mutate(stat = "sixes")
# 
# player_emp_probs_2022_24 <-
#   pmap(distinct_six_combos, get_empirical_prob, .progress = TRUE) |> 
#   bind_rows() |> 
#   select(player_name, line, games_played, empirical_prob)
# 
# # Add to main file
# player_sixes_data <-
#   player_sixes_data |> 
#   left_join(player_emp_probs_2022_24, by = c("player_name", "line")) |> 
#   rename(empirical_prob_over = empirical_prob) |> 
#   mutate(empirical_prob_under = 1 - empirical_prob_over) |> 
#   mutate(diff_over = empirical_prob_over - implied_prob_over,
#          diff_under = empirical_prob_under - implied_prob_under) |> 
#   mutate(implied_prob_over = round(implied_prob_over, 2),
#          implied_prob_under = round(implied_prob_under, 2),
#          empirical_prob_over = round(empirical_prob_over, 2),
#          empirical_prob_under = round(empirical_prob_under, 2),
#          diff_over = round(diff_over, 2),
#          diff_under = round(diff_under, 2)) |> 
#   distinct(player_name, market, line, agency, over_price, .keep_all = TRUE)


# Write out data----------------------------------------------------------------
player_fours_data |> 
  bind_rows(player_sixes_data) |>
  write_rds("Data/processed_odds/all_player_boundaries.rds")

# Add to google sheets
# sheet_write(sheet, data = player_fours_data |> bind_rows(player_sixes_data) , sheet = "Player Boundaries")
