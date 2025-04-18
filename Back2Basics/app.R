# Load the shiny and shinydashboard packages
library(shiny)
library(shinydashboard)

# DEFINE UI 
ui <- dashboardPage(
  dashboardHeader(title = "Shiny App with Checkboxes and Sliders"),
  
  # No sidebar, just a main body
  dashboardSidebar(disable = TRUE),  # Disable the sidebar
  
  dashboardBody(
    
    
    
    # DATA LAYERS BOX
    box(
      title = "Data Layers",
      status = "primary",
      solidHeader = TRUE,
      width = 6,  # Adjusted width to 6 (half of the available space)
      
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
    ),
    
<<<<<<< HEAD
    
=======

>>>>>>> a38c79db676f95622abb2503e089e40c28edf61f
    
    
    
    # LAYER & FRACTION BOX
    box(
      title = "Layer and Fraction Settings",
      status = "primary",
      solidHeader = TRUE,
      width = 6,  # Adjusted width to 6 (half of the available space)
      
      # Use fluidRow to create two columns (left and right subsections)
      fluidRow(
        # Left subsection for sliders (Layer)
        column(
          width = 6,  # Adjusted to 6 columns for each subsection
          box(
            title = "Layer",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            sliderInput("obs_year", "Observation Year(s):", min = 2000, max = 2025, value = c(2000, 2025)),
            sliderInput("min_depth", "Min. Sampling Depth:", min = 0, max = 1000, value = 0),
            sliderInput("max_depth", "Max. Sampling Depth:", min = 0, max = 1000, value = 1000)
          )
        ),
        
        # FRACTION SUBSECTION
        column(
          width = 6,  # Adjusted to 6 columns for each subsection
          box(
            title = "Fraction",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            checkboxGroupInput(
              inputId = "fraction",  # ID for the checkboxes
              label = NULL,  # No label here because the box title already serves as a label
              choices = c("Density", "Particle Size", "Aggregate", "Chemical", "Physical (Other)", "Thermal", "Compound Specific"),
              selected = NULL  # No checkboxes selected by default
            )
          )
        )
      )
    ),
    
    
    
    # DATA DOWNLOADER BOX
    box(
      title = "Data Downloader",
      status = "success",
      solidHeader = TRUE,
      width = 6,  # Adjusted width to 6 (half of the available space)
      
      # Search bar at the top
      textInput("search", "Search Data:", placeholder = "Enter search term..."),
      
      # Row with 3 checkboxes
      fluidRow(
        column(
          width = 4,
          checkboxInput("checkbox1", "All", value = FALSE)
        ),
        column(
          width = 4,
          checkboxInput("checkbox2", "Columns w/ Data", value = FALSE)
        ),
        column(
          width = 4,
          checkboxInput("checkbox3", "Non-blank Radiocarbon Columns", value = FALSE)
        )
      ),
      
      # Download button
      downloadButton("download_data", "Download")
    )
  )
)




# SERVER FUNCTION
server <- function(input, output) {
  # Placeholder for server-side logic (e.g., reacting to checkbox selections or sliders)
  
  # Example: downloadHandler for the "Download" button
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_download_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create dummy data for the download (in real scenarios, you'd generate the actual data)
      data <- data.frame(
        Search_Term = input$search,
        Option1 = input$checkbox1,
        Option2 = input$checkbox2,
        Option3 = input$checkbox3
      )
      
      # Write the data to the file
      write.csv(data, file)
    }
  )
}




# Run the Shiny app
<<<<<<< HEAD
shinyApp(ui = ui, server = server)
=======
shinyApp(ui = ui, server = server)





>>>>>>> a38c79db676f95622abb2503e089e40c28edf61f
