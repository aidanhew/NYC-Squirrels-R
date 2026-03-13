library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# ---- Constants ----

DATA_PATH <- '../data/squirrels.csv'

FUR_COLOURS <- c('Gray' = '#B2BEB5', 'Cinnamon' = '#D2691E', 'Black' = '#000000')
FUR_ORDER   <- c('Gray', 'Cinnamon', 'Black')

BEHAVIOUR_COLS <- c(
  'running', 'chasing', 'climbing', 'eating', 'foraging',
  'kuks', 'quaas', 'moans', 'approaches', 'indifferent', 'runs_from'
)

BEHAVIOUR_LABELS <- c(
  'running'     = 'Running',
  'chasing'     = 'Chasing',
  'climbing'    = 'Climbing',
  'eating'      = 'Eating',
  'foraging'    = 'Foraging',
  'kuks'        = 'Kuks',
  'quaas'       = 'Quaas',
  'moans'       = 'Moans',
  'approaches'  = 'Approaches',
  'indifferent' = 'Indifferent',
  'runs_from'   = 'Runs From'
)

addResourcePath('img', '../img')

# ---- Data loading ----

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
  # Header
  tags$div(
    style = '
      background-image: url(\'img/squirrels_image.png\');
      background-size: cover;
      background-position: center;
      padding: 40px 20px;
      margin-bottom: 20px;
      border-radius: 4px;
    ',
    h1('Central Park Squirrel Dashboard',
       style = 'color: white; font-weight: 300, margin: 0 0 8px 0'),
    tags$p(
      tags$a('Image Source', 
             href = 'https://www.centralparknyc.org/articles/getting-to-know-central-parks-squirrels',
             style = 'color: #ccc;'),
      ' // ',
      tags$a('Data Source',
             href = 'https://data.cityofnewyork.us/Environment/2018-Central-Park-Squirrel-Census-Squirrel-Data/vfnx-vebw',
             style = 'color: #ccc;')
    )
  ),
  
  # Filters
  fluidRow(
    column(
      12,
      wellPanel(
        fluidRow(
          # Shift filter
          column(
            3,
            checkboxGroupInput(
              inputId  = 'shift',
              label    = 'Shift',
              choices  = c('AM', 'PM'),
              selected = c('AM', 'PM'),
              inline   = TRUE
            )
          ),
          # Age filter
          column(
            3,
            checkboxGroupInput(
              inputId  = 'age',
              label    = 'Age',
              choices  = c('Adult', 'Juvenile'),
              selected = c('Adult', 'Juvenile'),
              inline   = TRUE
            )
          )
        )
      )
    )
  ),
  
  # Plots
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

# ---- Server ----
server <- function(input, output, session) {
  raw_df <- reactive({
    req(file.exists(DATA_PATH))
    load_squirrel_data(DATA_PATH)
  })
  
  # Filtered DF
  filtered_df <- reactive({
    df <- raw_df()
    
    if (!is.null(input$shift) && length(input$shift) > 0) {
      df <- df |> filter(shift %in% input$shift)
    }
    
    if (!is.null(input$age) && length(input$age) > 0) {
      df <- df |> filter(age %in% input$age)
    }
    
    df
  })
  
  y_max <- reactive({
    df <- filtered_df()
    
    fur_max <- if ('primary_fur_color' %in% names(df)) {
      df |>
        filter(!is.na(primary_fur_color), primary_fur_color %in% FUR_ORDER) |>
        count(primary_fur_color) |>
        pull(n) |>
        max(0)
    } else 0
    
    present_cols <- intersect(BEHAVIOUR_COLS, names(df))
    behaviour_max <- if (length(present_cols) > 0) {
      df |>
        select(all_of(present_cols)) |>
        summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |>
        pivot_longer(everything(), values_to = 'count') |>
        pull(count) |>
        max(0)
    } else 0
    
    max(fur_max, behaviour_max)
  })
  
  # Fur Colour Counts bar chart
  output$fur_color_plot <- renderPlot({
    df <- filtered_df()
    
    validate(need(nrow(df) > 0, 'No data available for the selected filters.'))
    validate(need('primary_fur_color' %in% names(df), 'Fur colour column not found.'))
    
    plot_data <- df |>
      filter(!is.na(primary_fur_color)) |>
      count(primary_fur_color) |>
      filter(primary_fur_color %in% FUR_ORDER) |>
      mutate(primary_fur_color = factor(primary_fur_color, levels = FUR_ORDER))
    
    ggplot(plot_data, aes(x = primary_fur_color, y = n, fill = primary_fur_color)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = FUR_COLOURS) +
      coord_cartesian(ylim = c(0, y_max())) +
      labs(x = 'Fur Colour', y = 'Sightings') +
      theme_minimal() +
      theme(legend.position = 'none')
  })
  
  # Top 5 Behaviours bar chart
  output$behaviour_plot <- renderPlot({
    df <- filtered_df()
    
    validate(need(nrow(df) > 0, 'No data available for the selected filters.'))
    
    present_cols <- intersect(BEHAVIOUR_COLS, names(df))
    validate(need(length(present_cols) > 0, 'No behaviour columns found in data.'))
    
    plot_data <- df |>
      select(all_of(present_cols)) |>
      summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |>
      pivot_longer(everything(), names_to = 'behaviour', values_to = 'count') |>
      arrange(desc(count)) |>
      mutate(behaviour = BEHAVIOUR_LABELS[behaviour]) |> 
      mutate(behaviour = factor(behaviour, levels = behaviour)) |> 
      slice_head(n = 5) |>
      mutate(behaviour = factor(behaviour, levels = behaviour))
    
    ggplot(plot_data, aes(x = behaviour, y = count)) +
      geom_col(fill = '#4A7C59', width = 0.6) +
      coord_cartesian(ylim = c(0, y_max())) +
      labs(x = 'Behaviour', y = 'Sightings') +
      theme_minimal()
  })
}

# ---- Run ----

shinyApp(ui = ui, server = server)