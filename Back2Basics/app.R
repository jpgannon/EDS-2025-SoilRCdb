# Load the shiny and shinydashboard packages
library(shiny)
library(shinydashboard)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Shiny App with Checkboxes"),
  
  # No sidebar, just a main body
  dashboardSidebar(disable = TRUE),  # Disable the sidebar
  
  dashboardBody(
    # Create a box containing the checkboxes directly in the main body
    box(
      title = "Data Layers",
      status = "primary",
      solidHeader = TRUE,
      width = 6,
      
      # Add custom CSS to change the text color of checkbox labels
      tags$head(
        tags$style(HTML("
          .checkbox label {
            color: black !important;  /* Set label text color to black */
          }
        "))
      ),
      
      # Place the checkbox group directly within the box
      checkboxGroupInput(
        inputId = "checkboxes",  # ID for the checkboxes
        label = NULL,  # No label here because the box title already serves as a label
        choices = c("All", "Layer", "Flux", "Interstitial", "Fraction", "Incubation"),
        selected = NULL  # No checkboxes selected by default
      )
    )
  )
)

# Define the server function (can be expanded with functionality later)
server <- function(input, output) {
  # Placeholder for server-side logic (e.g., reacting to checkbox selections)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
