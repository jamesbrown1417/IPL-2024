##%######################################################%##
#                                                          #
####              Libraries and functions               ####
#                                                          #
##%######################################################%##

library(tidyverse)
library(cricketdata)
library(future)
library(furrr)
`%notin%` <- Negate(`%in%`)

##%######################################################%##
#                                                          #
####              Read data from cricsheet              ####
#                                                          #
##%######################################################%##

# Scraped Data
batting_stats_ipl <- readRDS("Data/batting_stats_ipl.rds")
bowling_stats_ipl <- readRDS("Data/bowling_stats_ipl.rds")

# 2024 Squads
ipl_squads_2024 <- read_csv("Data/ipl_squads_2024.csv")

# Create first initial and last name
ipl_squads_2024 <- 
  ipl_squads_2024 |> 
  separate(player, into = c("first_name", "last_name"), sep = " ", remove = FALSE) |> 
  mutate(first_initial = substr(first_name, 1, 1)) |> 
  mutate(join_name = paste(first_initial, last_name, sep = " "))

# Create join dataset
join_data <- 
  ipl_squads_2024 |> 
  select(join_name, full_name = player, current_team = team)

##%######################################################%##
#                                                          #
####                     Join names                     ####
#                                                          #
##%######################################################%##

batting_stats_ipl <-
  batting_stats_ipl |> 
  separate(player, into = c("first_name", "last_name"), sep = " ", remove = FALSE) |> 
  mutate(first_initial = substr(first_name, 1, 1)) |>
  mutate(join_name = paste(first_initial, last_name, sep = " ")) |>
  relocate(join_name, .before = player) |>
  left_join(join_data, by = "join_name")

bowling_stats_ipl <-
  bowling_stats_ipl |> 
  separate(player, into = c("first_name", "last_name"), sep = " ", remove = FALSE) |> 
  mutate(first_initial = substr(first_name, 1, 1)) |>
  mutate(join_name = paste(first_initial, last_name, sep = " ")) |>
  relocate(join_name, .before = player) |>
  left_join(join_data, by = "join_name")

##%######################################################%##
#                                                          #
####                     Save data                      ####
#                                                          #
##%######################################################%##

saveRDS(batting_stats_ipl, "Data/batting_stats_ipl.rds")
saveRDS(bowling_stats_ipl, "Data/bowling_stats_ipl.rds")