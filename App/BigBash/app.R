library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)
library(googlesheets4)
library(googledrive)

# Google sheets authentification -----------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
gs4_auth(token = drive_token())

# Determine the operating system
os_type <- Sys.info()["sysname"]

#===============================================================================
# Load in data
#===============================================================================

# Scraped Data
batting_stats_bbl <- readRDS("../../Data/batting_stats_bbl.rds")
bowling_stats_bbl <- readRDS("../../Data/bowling_stats_bbl.rds")

# Fix names
names_key <-
  readRDS("../../Data/player_names_linking.rds") |> 
  select(player_name_supercoach, player_name_cricinfo)

batting_stats_bbl <-
  batting_stats_bbl |> 
  left_join(names_key, by = c("player" = "player_name_cricinfo")) |> 
  rename(player_name = player_name_supercoach) |> 
  relocate(player_name, .before = player) |>
  select(-player) |> 
  filter(!is.na(player_name))

bowling_stats_bbl <-
  bowling_stats_bbl |> 
  left_join(names_key, by = c("player" = "player_name_cricinfo")) |> 
  rename(player_name = player_name_supercoach) |> 
  relocate(player_name, .before = player) |>
  select(-player) |> 
  filter(!is.na(player_name))

past_two_seasons <-
  batting_stats_bbl |>
  # filter(start_date >= "2020-06-10") |>
  group_by(match_id,
           innings,
           team,
           venue) |>
  summarise(
    runs = sum(runs_scored),
    balls = sum(balls_faced),
    fours = sum(fours),
    sixes = sum(sixes)
  )

past_two_seasons |>
  # filter(innings == 1) |> 
  group_by(venue) |>
  summarise(
    games_played = n(),
    avg_runs = mean(runs),
    avg_sixes = mean(sixes),
    avg_fours = mean(fours)
  ) |> 
  arrange(desc(avg_runs))

#===============================================================================
# Read in scraped odds
#===============================================================================

# Conditional logic for loading data based on OS
if (os_type == "Windows") {
  # Read RDS Data for Windows
  player_runs_data <- read_rds("../../Data/processed_odds/all_player_runs.rds")
  player_wickets_data <- read_rds("../../Data/processed_odds/all_player_wickets.rds")
  player_boundaries_data <- read_rds("../../Data/processed_odds/all_player_boundaries.rds")
} else {
  # Google Sheets Data for other OS
  ss_name <- gs4_find("Big Bash Data")
  player_runs_data <- read_sheet(ss = ss_name, sheet = "Player Runs")
  player_wickets_data <- read_sheet(ss = ss_name, sheet = "Player Wickets")
  player_boundaries_data <- read_sheet(ss = ss_name, sheet = "Player Boundaries")
}



#===============================================================================
# UI
#===============================================================================

ui <- page_navbar(
  title = "Big Bash",
  selected = "Odds Screen",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  tags$head(tags$style(
    HTML(
      "
      .tab-content, .tab-pane {
        height: 1250px;
        overflow-y: auto;
      }
      .dataTables_wrapper {
        overflow-x: auto;
      }
    "
    )
  )),
  nav_panel(
    title = "Player Stats",
    grid_container(
      layout = c("bbl_stats player_stat_plot"),
      row_sizes = c("1fr"),
      col_sizes = c("250px", "1fr"),
      gap_size = "10px",
      grid_card(
        area = "bbl_stats",
        card_header("Settings"),
        card_body(
          textInput(
            inputId = "player_name_input_a",
            label = "Select Player:",
            value = "Glenn Maxwell"
          ),
          selectInput(
            inputId = "stat_input_a",
            label = "Select Statistic:",
            choices = c("Runs",
                        "Wickets",
                        "4s",
                        "6s"),
            multiple = FALSE,
            selected = "Runs"
          ),
          selectInput(
            inputId = "season_input_a",
            label = "Select Season:",
            choices = batting_stats_bbl$season |> unique() |> sort(),
            multiple = TRUE,
            selected = batting_stats_bbl$season |> unique() |> sort()
          ),
          selectInput(
            inputId = "venue_input_a",
            label = "Select Venue:",
            choices = batting_stats_bbl$venue |> unique() |> sort(),
            multiple = TRUE,
            selected = batting_stats_bbl$venue |> unique() |> sort()
          ),
          selectInput(
            inputId = "innings_input_a",
            label = "Select Innings:",
            choices = c("1", "2"),
            multiple = TRUE,
            selected = c("1", "2")
          ),
          checkboxInput(
            inputId = "exclude_dnb",
            label = "Exclude DNBs?",
            value = TRUE
          ),
          markdown(mds = c("__Select Minimum Innings Length:__")),
          numericInput(
            inputId = "innings_balls_bowled_min",
            label = "Number of Balls",
            value = 120
          ),
          markdown(mds = c("__Select Only Last n Games:__")),
          numericInput(
            inputId = "last_games",
            label = "Number of Games",
            value = NA
          ),
          markdown(mds = c("__Select Reference Line:__")),
          numericInput(
            inputId = "reference_line",
            label = "Line Value",
            value = 19.5
          ))),
          grid_card(area = "player_stat_plot",
                    card_body(
                      tabsetPanel(
                        id = "stat_tabs",
                        tabPanel("Plot",
                                 plotOutput(outputId = "plot", height = "800px")),
                        tabPanel(
                          "Table",
                          DTOutput(
                            outputId = "player_stat_table",
                            width = "100%",
                            height = "800px"
                          )
                        )
                      )
                    ))))
,
  nav_panel(
    title = "Odds Screen",
    grid_container(
      layout = c("odds_screen odds_table"),
      row_sizes = c("1fr"),
      col_sizes = c("250px", "1fr"),
      gap_size = "10px",
      grid_card(
        area = "odds_screen",
        card_header("Settings"),
        card_body(
          # Add your specific input fields here
          # Example: selectInput, textInput, numericInput, etc.
          selectInput(
            inputId = "agency_input",
            label = "Select Agencies:",
            choices = player_runs_data$agency |> unique(),
            multiple = TRUE,
            selectize = TRUE,
            selected = player_runs_data$agency |> unique(),
          ),
          selectInput(
            inputId = "market_input",
            label = "Select Market:",
            choices = c("Runs", "Wickets", "Boundaries"),
            multiple = FALSE
          ),
          selectInput(
            inputId = "match_input",
            label = "Select Matches:",
            choices = player_runs_data$match |> unique(),
            multiple = TRUE,
            selectize = FALSE,
            selected = player_runs_data$match |> unique()
          ),
          textInput(
            inputId = "player_name_input_b",
            label = "Select Player:",
            value = NA
          ),
          checkboxInput(
            inputId = "only_unders",
            label = "Only Show Markets With Unders",
            value = FALSE
          ),
          checkboxInput(
            inputId = "only_best",
            label = "Only Show Best Market Odds - Overs",
            value = FALSE
          ),
          checkboxInput(
            inputId = "only_best_unders",
            label = "Only Show Best Market Odds - Unders",
            value = FALSE
          )
        )
      ),
      grid_card(area = "odds_table",
                card_body(
                  DTOutput(outputId = "scraped_odds_table", height = "1000px")
                ))
    )
  )
  # You can add more nav_panels here if needed for other sections of your new project
)

#===============================================================================
# Server
#===============================================================================

server <- function(input, output, session) {
  # Add your server-side code here
  
  #=============================================================================
  # Filter player stats
  #=============================================================================
  
  filtered_player_stats <- reactive({
    # Filter player stats
    
    if (input$stat_input_a == "Runs") {
      all_player_stats <- batting_stats_bbl
    } else if (input$stat_input_a == "Wickets") {
      all_player_stats <- bowling_stats_bbl
    } else if (input$stat_input_a == "4s") {
      all_player_stats <- batting_stats_bbl
    } else if (input$stat_input_a == "6s") {
      all_player_stats <- batting_stats_bbl
    }
    
    if (input$stat_input_a == "Wickets") {
    filtered_player_stats <-
      all_player_stats |>
      filter(
        player_name == input$player_name_input_a,
      ) |>
      arrange(start_date) |>
      mutate(game_number = row_number()) |> 
      select(Date = start_date,
             Season = season,
             Venue = venue,
             Innings = innings,
             Innings_Balls = balls_bowled_innings,
             Player = player_name,
             Team = team,
             Wickets = wickets_taken,
             Balls = balls_bowled,
             game_number) |> 
      arrange(desc(Date))
    }
    
    else {
      filtered_player_stats <-
        all_player_stats |>
        filter(
          player_name == input$player_name_input_a,
        ) |>
        arrange(start_date) |>
        mutate(game_number = row_number()) |> 
        select(Date = start_date,
               Season = season,
               Venue = venue,
               Innings = innings,
               Position = batting_position,
               Innings_Balls = balls_bowled_innings,
               DNB,
               Player = player_name,
               Team = team,
               Runs = runs_scored,
               Balls = balls_faced,
               `4s` = fours,
               `6s` = sixes,
               game_number) |> 
        arrange(desc(Date))
    }
    
    # Filter by last n games
    if (!is.na(input$last_games)) {
      filtered_player_stats <-
        filtered_player_stats |>
        slice_head(n = input$last_games)
    }
    
    # Filter by innings balls bowled
    if (!is.na(input$innings_balls_bowled_min)) {
      filtered_player_stats <-
        filtered_player_stats |>
        filter(Innings_Balls >= input$innings_balls_bowled_min)
    }
    
    # Filter DNBs
    if (input$exclude_dnb & input$stat_input_a != "Wickets") {
      filtered_player_stats <-
        filtered_player_stats |>
        filter(!DNB)
    }
    
    # Filter by innings
    filtered_player_stats <-
      filtered_player_stats |>
      filter(Innings %in% input$innings_input_a)
    
    # Filter by season
    filtered_player_stats <-
      filtered_player_stats |>
      filter(Season %in% input$season_input_a)
    
    # Filter by venue
    filtered_player_stats <-
      filtered_player_stats |>
      filter(Venue %in% input$venue_input_a)
    
    # Return filtered player stats
    return(filtered_player_stats)
    
  })
  
  #=============================================================================
  # Get Proportion above reference line
  #=============================================================================
  
  proportion_above_reference_line <- reactive({
    # Get proportion above reference line
    proportion_above_reference_line <-
      filtered_player_stats() |>
      filter(!!sym(input$stat_input_a) >= input$reference_line) |>
      nrow() / nrow(filtered_player_stats())
    
    # Get implied Odds
    implied_odds <- 1 / proportion_above_reference_line
    implied_odds_under <- 1 / (1 - proportion_above_reference_line)
    
    # Get string to output
    output_string <- paste0(
      "Proportion Above Reference Line: ",
      round(proportion_above_reference_line, 2),
      "\n",
      "Implied Odds - Over: ",
      round(implied_odds, 2),
      "\n",
      "Implied Odds - Under: ",
      round(implied_odds_under, 2),
      "\n",
      "Sample Size: ",
      nrow(filtered_player_stats())
    )
    
    return(output_string)
    
  })
  
  #=============================================================================
  # Plot player stats
  #=============================================================================
  
  output$plot <- renderPlot({
    # Create a new variable that checks if the y-value is above the reference line
    df_with_color <- filtered_player_stats() %>%
      mutate(color_condition = ifelse(
        !!sym(input$stat_input_a) >= input$reference_line,
        "limegreen",
        "red1"
      ))
    
    # Plot player stats
    p <- df_with_color %>%
      ggplot(aes(
        x = game_number,
        y = !!sym(input$stat_input_a),
        color = color_condition
      )) +
      
      # Basic Elements
      geom_point(size = 3) +
      geom_smooth(
        method = "loess",
        se = FALSE,
        inherit.aes = FALSE,
        mapping = aes(x = game_number, y = !!sym(input$stat_input_a))
      ) +
      geom_hline(
        yintercept = input$reference_line,
        linetype = "dashed",
        color = "grey4",
        size = 1
      )+
      
      # Add text
      annotate(
        geom = "text",
        x = 1,
        y = max(filtered_player_stats() %>% pull(!!sym(
          input$stat_input_a
        ))),
        label = proportion_above_reference_line(),
        hjust = 0,
        vjust = 1,
        color = "black",
        size = 6
      ) +
      
      # Aesthetics
      theme_bw() +
      theme(
        plot.background = element_rect(fill = "white", colour = "white"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      ) +
      
      # Labels & Titles
      labs(title = "",
           x = "Game Number") +
      
      # Set manual color scale
      scale_color_identity() +
      
      # Additional
      theme(legend.position = "none")
    
    print(p)
  })
  
  #=============================================================================
  # Table player stats
  #=============================================================================
  
  output$player_stat_table <- renderDT({
    datatable(
      filtered_player_stats(),
      options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE),
      width = "100%",
      height = "800px"
    )
  })
  
  #=============================================================================
  # Reactive function to scrape odds
  #=============================================================================
  
  scraped_odds <- reactive({
    # Get odds---------------------------------------------------------------
    
    # Runs
    if (input$market_input == "Runs") {
      odds <-
        player_runs_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    # Boundaries
    if (input$market_input == "Boundaries") {
      odds <-
        player_boundaries_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(match %in% input$match_input) |>
        select(-match) 
    }
    
    # Wickets
    if (input$market_input == "Wickets") {
      odds <-
        player_wickets_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    if (input$player_name_input_b != "") {
      odds <-
        odds |>
        filter(str_detect(player_name, input$player_name_input_b))
    }
    
    if (input$only_best == TRUE) {
      odds <-
        odds |> 
        arrange(player_name, line, desc(over_price)) |>
        group_by(player_name, market, line) |> 
        slice_head(n = 1) |>
        ungroup()
    }
    
    if (input$only_best_unders == TRUE) {
      odds <-
        odds |> 
        arrange(player_name, line, desc(under_price)) |>
        group_by(player_name, market, line) |> 
        slice_head(n = 1) |>
        ungroup()
    }
      
      if (input$only_unders == TRUE) {
        odds <-
          odds |> 
          filter(!is.na(under_price))
    }
    
    # Return odds
    return(odds)
  })
  
  # Table output
  output$scraped_odds_table <- renderDT({
    datatable(scraped_odds(),
              fillContainer = TRUE,
              filter = "top",
              options = list(
                pageLength = 17,
                autoWidth = FALSE,
                scrollX = TRUE, scrollY = TRUE,
                lengthMenu = c(5, 10, 15, 20, 25, 30)
              ))
  })
}

# Run the application
shinyApp(ui, server)
