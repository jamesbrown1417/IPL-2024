#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in past season stats
#===============================================================================

# Scraped Data
batting_stats_ipl <- readRDS("Data/batting_stats_ipl.rds")
bowling_stats_ipl <- readRDS("Data/bowling_stats_ipl.rds")

# Restrict to only 2021 season onwards
batting_stats_ipl <- batting_stats_ipl |> filter(start_date >= "2021-06-01")
bowling_stats_ipl <- bowling_stats_ipl |> filter(start_date >= "2021-06-01")

#===============================================================================
# Create a function that takes a player name + line and returns their hit rate
#===============================================================================

get_empirical_prob <- function(name, line, stat) {
  
  # Choose the data based on the selected player
  batting_stats_ipl <-
    batting_stats_ipl |> 
    filter(player_name == name)
  
  bowling_stats_ipl <- bowling_stats_ipl |>
    filter(player_name == name)

  # Initialize empirical_prob
  empirical_prob <- NULL
  
  # Branch based on whether stat is PTS, REB or AST
  if (stat == "runs") {
    empirical_prob <- batting_stats_ipl |> 
      summarise(games_played = n(),
                empirical_prob = mean(runs_scored >= line))
  } else if (stat == "wickets") {
    empirical_prob <- bowling_stats_ipl |> 
      summarise(games_played = n(),
                empirical_prob = mean(wickets_taken >= line))
  } else if (stat == "fours") {
    empirical_prob <- batting_stats_ipl |> 
      summarise(games_played = n(),
                empirical_prob = mean(fours >= line))
  } else if (stat == "sixes") {
    empirical_prob <- batting_stats_ipl |> 
      summarise(games_played = n(),
                empirical_prob = mean(sixes >= line))
  } else {
    stop("stat must be one of runs, wickets, fours, sixes")
  }
  
  # Add line, player_name, and season information
  empirical_prob <- empirical_prob |> 
    mutate(line = line, 
           player_name = name)
  
  # Rename the empirical_prob column to include season
  new_col_name <- paste("empirical_prob")
  empirical_prob <- empirical_prob |> 
    rename_with(~ new_col_name, .cols = "empirical_prob")
  
  # Return empirical probability
  return(empirical_prob)
}

get_empirical_prob(name = "Aaron Finch", line = 9.5, stat = "runs")
