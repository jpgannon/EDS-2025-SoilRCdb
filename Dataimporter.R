'Data Importer'
'peter king'
'3/19/25'

library(shiny)
library(shinydashboard)

# --- Data Import Code (Inline) ---

load_ISRaD_data <- function() {
  # Define the raw URL of the .rda file
  file_url <- "https://raw.githubusercontent.com/International-Soil-Radiocarbon-Database/ISRaD/28bd3bed4346c8bcf9a7fa76df80158caac0e400/ISRaD_data_files/database/ISRaD_extra.rda"
  
  # Create a temporary file to download the data
  temp_file <- tempfile(fileext = ".rda")
  
  # Download the file (mode = "wb" is important for binary files)
  download.file(file_url, destfile = temp_file, mode = "wb")
  
  # Create a new environment to load the data into
  data_env <- new.env()
  
  # Load the data into the new environment.
  # load() returns the names of the loaded objects.
  loaded_names <- load(temp_file, envir = data_env)
  
  # Remove the temporary file
  unlink(temp_file)
  
  # If the file contains a single object, return that object;
  # otherwise, return a list of all objects.
  if (length(loaded_names) == 1) {
    return(data_env[[loaded_names]])
  } else {
    return(as.list(data_env))
  }
}

# Load the ISRaD data (this happens when the app starts)
ISRaD_data <- load_ISRaD_data()

# --- Shiny App UI & Server ---

ui <- dashboardPage(
  dashboardHeader(title = "Test Data Import"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        title = "Imported Data Structure",
        width = 12,
        verbatimTextOutput("data_str")
      )
    )
  )
)

server <- function(input, output, session) {
  output$data_str <- renderPrint({
    # Display the structure of the loaded ISRaD data
    str(ISRaD_data)
  })
}

shinyApp(ui, server)

