library(tidyverse)

# Scraped Data
batting_stats_bbl <- readRDS("Data/batting_stats_bbl.rds")
bowling_stats_bbl <- readRDS("Data/bowling_stats_bbl.rds")

# Improved function to calculate probability and correlation of two players taking at least one wicket
calculate_wicket_probabilities <- function(bowling_data, player_pattern_1, player_pattern_2, innings_nums = c(1)) {

  # Player a data
  player_a_data <-
    bowling_data |> 
    filter(str_detect(player, player_pattern_1) & innings %in% innings_nums) |> 
    filter(balls_bowled_innings >= 120) |> 
    select(match_id, start_date, venue, innings, wickets_taken)
  
  # Player b data
  player_b_data <-
    bowling_data |> 
    filter(str_detect(player, player_pattern_2) & innings %in% innings_nums) |> 
    filter(balls_bowled_innings >= 120) |> 
    select(match_id, start_date, venue, innings, wickets_taken)
  
  # Inner Join
  filtered_data <-
    player_a_data |> 
    inner_join(player_b_data, by = c("match_id", "start_date", "venue", "innings"), suffix = c("_a", "_b")) |> 
    mutate(
      at_least_1_wicket_a = ifelse(wickets_taken_a >= 1, 1, 0),
      at_least_1_wicket_b = ifelse(wickets_taken_b >= 1, 1, 0),
      both_take_1_wicket = ifelse(at_least_1_wicket_a == 1 & at_least_1_wicket_b == 1, 1, 0)
    )
  
  # Calculate probability
  prob_both_take_1_wicket <- mean(filtered_data$both_take_1_wicket, na.rm = TRUE)
  
  # Calculate implied odds
  implied_odds <- 1 / prob_both_take_1_wicket
  
  # Games Played
  games_played <- nrow(filtered_data)
  
  # Calculate Correlation (if feasible)
  correlation <- if (games_played > 0) {
    cor(
      filtered_data$at_least_1_wicket_a,
      filtered_data$at_least_1_wicket_b,
      use = "complete.obs",
      method = "pearson"
    )
  } else {
    NA
  }
  
  # Return a tibble
  tibble(
    player_1 = player_pattern_1,
    player_2 = player_pattern_2,
    correlation = round(correlation, 3),
    implied_odds = round(implied_odds, 2),
    sample_size = games_played
  )
}

# Example usage
calculate_wicket_probabilities(bowling_stats_bbl, "Bart", "Stek", innings_nums = 1)
