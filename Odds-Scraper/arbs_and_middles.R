#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

# Directory of scraped odds data
scraped_odds_dir <- "Data/scraped_odds"

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
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = round((max_implied_prob - min_implied_prob), 2)) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob)

# Get possible arbitrages-------------------------------------------------------
all_unders <-
  player_runs_data |> 
  select(-match:-away_team, -variation, -implied_prob_over) |>
  filter(!is.na(under_price)) |> 
  select(-over_price) |> 
  rename(agency_under = agency)

all_overs <-
  player_runs_data |> 
  select(-match:-away_team, -variation, -implied_prob_over) |>
  filter(!is.na(over_price)) |> 
  select(-under_price) |> 
  rename(agency_over = agency) |> 
  group_by(player_name, line) |>
  slice_max(over_price) |>
  ungroup()

arbitrage <-
  all_unders |> 
  left_join(all_overs) |> 
  filter(agency_under != agency_over) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  mutate(margin = round(margin, 3)) |>
  arrange(margin)

# Get possible middles----------------------------------------------------------
all_unders_middle <-
  all_unders |> 
  rename(under_line = line)

all_overs_middle <-
  all_overs |> 
  rename(over_line = line)

middles <-
  all_unders_middle |> 
  left_join(all_overs_middle, relationship = "many-to-many") |> 
  filter(agency_under != agency_over) |>
  filter(over_line < under_line) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  mutate(margin = round(margin, 3)) |>
  mutate(under_line = as.numeric(under_line)) |>
  mutate(over_line = as.numeric(over_line)) |>
  mutate(middle_size = under_line - over_line) |>
  arrange(desc(middle_size))

# Plot
middles |> 
  ggplot(aes(x = margin, y = middle_size)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Margin", y = "Middle Size", title = "Middle Size vs Margin")

# Fit model to get biggest outlier middle size to margin relationship-----------
model <- lm(middle_size ~ margin, data = middles)

# Get residuals
middles <- middles |> 
  mutate(residuals = model$residuals)
