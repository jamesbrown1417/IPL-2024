# Function to fix team names

fix_team_names <- function(team_vector) {
  new_team_vector <- ifelse(str_detect(team_vector, "Bangalore"), "Royal Challengers Bangalore", team_vector)
  new_team_vector <- ifelse(str_detect(new_team_vector, "Punjab"), "Punjab Kings", new_team_vector)
  new_team_vector <- ifelse(str_detect(new_team_vector, "Delhi"), "Delhi Capitals", new_team_vector)
  new_team_vector <- ifelse(str_detect(new_team_vector, "Hyderabad"), "Sunrisers Hyderabad", new_team_vector)
  new_team_vector <- ifelse(str_detect(new_team_vector, "Kolkata"), "Kolkata Knight Riders", new_team_vector)
  new_team_vector <- ifelse(str_detect(new_team_vector, "Mumbai"), "Mumbai Indians", new_team_vector)
  new_team_vector <- ifelse(str_detect(new_team_vector, "Chennai"), "Chennai Super Kings", new_team_vector)
  new_team_vector <- ifelse(str_detect(new_team_vector, "Lucknow"), "Lucknow Super Giants", new_team_vector)
  new_team_vector <- ifelse(str_detect(new_team_vector, "Rajasthan"), "Rajasthan Royals", new_team_vector)
  new_team_vector <- ifelse(str_detect(new_team_vector, "Gujarat"), "Gujarat Titans", new_team_vector)
}