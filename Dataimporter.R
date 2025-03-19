library(shiny)
library(shinydashboard)
library(DT)

# --- Data Import Code ---
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
  loaded_names <- load(temp_file, envir = data_env)
  
  # Remove the temporary file
  unlink(temp_file)
  
  # Return the loaded object (or list of objects)
  if (length(loaded_names) == 1) {
    return(data_env[[loaded_names]])
  } else {
    return(as.list(data_env))
  }
}

# Load the ISRaD data at startup
ISRaD_data <- load_ISRaD_data()

# --- Shiny App UI & Server ---
ui <- fluidPage(
  titlePanel("Hierarchical Dependencies with Data Import and Dynamic Table Display"),
  
  # Top-level checkboxes (only visible options)
  checkboxGroupInput(
    inputId = "top_checkboxes",
    label = "Data Layers (User Visible)",
    choices = c("All", "Layer", "Flux", "Interstitial", "Fraction", "Incubation"),
    selected = NULL
  ),
  
  # Debug outputs to show user selection vs. actual implied selection
  verbatimTextOutput("debug_user"),
  verbatimTextOutput("debug_actual"),
  
  # Placeholder for dynamically generated tabs to display tables
  uiOutput("dynamicTabs")
)

server <- function(input, output, session) {
  # --- Define the Hierarchy ---
  # Each top-level option implies a set of underlying tables (dependencies)
  hierarchy <- list(
    "All"          = c("All", "Metadata", "Site", "Profile", "Flux", "Layer", "Interstitial", "Fraction", "Incubation"),
    "Layer"        = c("Layer", "Profile", "Site", "Metadata"),
    "Flux"         = c("Flux", "Profile", "Site", "Metadata"),
    "Interstitial" = c("Interstitial", "Profile", "Site", "Metadata"),
    "Fraction"     = c("Fraction", "Profile", "Site", "Metadata"),
    "Incubation"   = c("Incubation", "Profile", "Site", "Metadata")
  )
  
  # --- Helper: Compute the full implied selection ---
  impliedSelection <- function(user_selected) {
    if ("All" %in% user_selected) {
      return(hierarchy[["All"]])
    }
    final_set <- character(0)
    for (item in user_selected) {
      final_set <- union(final_set, hierarchy[[item]])
    }
    final_set
  }
  
  # Reactive: Compute the actual (hidden) selection based on user input
  actualSelection <- reactive({
    impliedSelection(input$top_checkboxes)
  })
  
  # --- Debug Outputs ---
  output$debug_user <- renderPrint({
    cat("User sees these checked:", input$top_checkboxes, "\n")
  })
  
  output$debug_actual <- renderPrint({
    cat("Actual selection (including hidden dependencies):", 
        paste(actualSelection(), collapse = ", "), "\n")
  })
  
  # --- Dynamic UI: Create a tabsetPanel with one tab per selected table ---
  output$dynamicTabs <- renderUI({
    # Convert the actual selection to lower-case to match ISRaD_data names
    sel <- tolower(actualSelection())
    # Only include valid keys (the imported data frames)
    valid_sel <- intersect(sel, names(ISRaD_data))
    if (length(valid_sel) == 0) {
      return(h3("No data selected"))
    }
    
    # Create a tab for each valid table
    tabs <- lapply(valid_sel, function(tbl) {
      tabPanel(title = tbl, DTOutput(outputId = paste0("table_", tbl)))
    })
    do.call(tabsetPanel, tabs)
  })
  
  # --- Dynamic Server: Render each DT table for the selected tables ---
  observe({
    sel <- tolower(actualSelection())
    valid_sel <- intersect(sel, names(ISRaD_data))
    for (tbl in valid_sel) {
      local({
        tableName <- tbl
        output[[paste0("table_", tableName)]] <- renderDT({
          datatable(ISRaD_data[[tableName]], 
                    options = list(pageLength = 10, scrollX = TRUE))
        })
      })
    }
  })
}

shinyApp(ui, server)