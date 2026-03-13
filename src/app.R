library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

DATA_PATH <- 'data/squirrels.csv'

FUR_COLOURS <- c('Gray' = '#B2BEB5', 'Cinnamon' = '#D2691E', 'Black' = '#000000')
FUR_ORDER   <- c('Gray', 'Cinnamon', 'Black')

BEHAVIOUR_COLS <- c(
  'running', 'chasing', 'climbing', 'eating', 'foraging',
  'kuks', 'quaas', 'moans', 'approaches', 'indifferent', 'runs_from'
)

load_squirrel_data <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  
  names(df) <- tolower(gsub(' ', '_', names(df)))
  
  if ('date' %in% names(df)) {
    df$date <- as.Date(as.character(df$date), format = '%Y%m%d')
  }
  
  for (col in intersect(BEHAVIOUR_COLS, names(df))) {
    df[[col]] <- tolower(as.character(df[[col]])) %in% c('true', 't', '1', 'yes')
  }
  
  df
}

# ---- UI ----

ui <- fluidPage(
  titlePanel('NYC Central Park Squirrel Dashboard'),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Shift filter
      checkboxGroupInput(
        inputId  = 'shift',
        label    = 'Shift',
        choices  = c('AM', 'PM'),
        selected = c('AM', 'PM')
      ),
      
      # Age filter
      checkboxGroupInput(
        inputId  = 'age',
        label    = 'Age',
        choices  = c('Adult', 'Juvenile'),
        selected = c('Adult', 'Juvenile')
      )
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        # Fur Colour bar chart
        column(
          6,
          h4('Fur Colour Counts'),
          plotOutput('fur_color_plot', height = '280px')
        ),
        # Top 5 Behaviours bar chart
        column(
          6,
          h4('Top 5 Behaviours'),
          plotOutput('behaviour_plot', height = '280px')
        )
      )
    )
  )
)

