# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# Read in json
neds_response <- jsonlite::fromJSON("Odds-Scraper/Neds/neds_response.json")

# Initialize empty lists to store data
event_name <- character()
event_id <- character()
competition_name <- character()

# Extract event IDs and names from JSON response
for (value in neds_response$events) {
  event_name <- c(event_name, value$name)
  event_id <- c(event_id, value$id)
  competition_name <- c(competition_name, value$competition$name)
}

# Create a data frame from the vectors
df <- data.frame(event_name, event_id, competition_name)

# Filter the data frame to only include matches with ' vs ' in the event name
df <- df |> filter(str_detect(event_name, ' vs '))

# Only get IPL Games
df <- df |> filter(str_detect(competition_name, 'IPL'))

df$url <-
  paste0("https://www.neds.com.au/sports/cricket/ipl/",
         tolower(gsub(" ", "-", df$event_name)),
         "/",
         df$event_id)

# Write out as csv
write_csv(df, "Odds-Scraper/Neds/neds_ipl_match_urls.csv")
