# Load packages used by the app. Install missing packages, if needed.
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)

# Read data from a CSV file
policies <- read_csv("data/sunlight_policy_census_places.csv")

# Define lists 
policy_types <- sort(unique(policies$legal_custom))
places <- sort(unique(policies$sunlight.place))

# Set the default theme for ggplot2 plots
ggplot2::theme_set(ggplot2::theme_minimal())

# Apply the CSS used by the Shiny app to the ggplot2 plots
thematic_shiny()

# Define the Shiny UI layout
ui <- page_sidebar(
  
  # Set CSS theme
  theme = bs_theme(bootswatch = "darkly",
                   bg = "#222222",
                   fg = "#86C7ED",
                   success ="#86C7ED"),
  
  # Add title
  title = "Open Data Policies",
  
  # Add sidebar elements
  sidebar = sidebar(title = "Filters:",
                    class ="bg-secondary",
                    selectInput("policy_type", "Select a policy type", choices = policy_types, selected = "", multiple  = TRUE),
                    selectInput("place", "Select a place", choices = places, selected = "", multiple  = TRUE),
                    "This app compares the types of open data policies by place and year",
                    ),
  
  # Layout non-sidebar elements
  layout_columns(card(card_header("Policies over time"),
                      plotOutput("line")),
                 value_box(title = "Places",
                           value = textOutput("place_count"),
                           theme_color = "secondary"),
                 value_box(title = "Policy Types",
                           value = textOutput("policy_type_count"),
                           theme_color = "secondary"),
                 card(card_header("Policy Details"),
                      tableOutput("table")),
                 col_widths = c(12, 6, 6, 12),
                 row_heights = c(4, 1.5, 3))
)

# Define the Shiny server function
server <- function(input, output) {
  
  # Provide default values for industry, propensity, and contract selections
  selected_policy_types <- reactive({
    if (is.null(input$policy_type)) policy_types else input$policy_type
  })
  
  selected_places <- reactive({
    if (is.null(input$place)) places else input$place
  })
  
  # Filter data against selections
  filtered_policies <- reactive({
    policies |>
      filter(legal_custom %in% selected_policy_types(),
             sunlight.place %in% selected_places())
  })
  
  # Count by year
  policies_by_year <- reactive({
    filtered_policies() |>
      group_by(sunlight.policy_year) |>
      summarize(count = n()) |>
      ungroup()
  })
  
  # Render text for number of places
  output$place_count <- renderText({
    length(unique(filtered_policies()$sunlight.place)) |>
      format(big.mark = ",")
  })  
  
  # Render text for number of customers
  output$policy_type_count <- renderText({
    length(unique(filtered_policies()$legal_custom)) |>
      format(big.mark = ",")
  })
  
  
  # Render line plot for conversions over time
  output$line <- renderPlot({
    ggplot(policies_by_year(), aes(x = sunlight.policy_year, y = count)) +
      geom_line() +
      theme(axis.title = element_blank())
  })
  
  
  # Render table for conversion rates by subgroup
  output$table <- renderTable({
    filtered_policies() |>
      select(sunlight.place, sunlight.policy_year, legal_custom, policy_markdown)
  },
  digits = 0)
}

# Create the Shiny app
shinyApp(ui = ui, server = server)