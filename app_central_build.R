# Allow cartesian joins (use with caution – this can result in very large outputs)
options(datatable.allow.cartesian = TRUE)

library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(geojsonsf)
library(jsonlite)
library(data.table)  # For memory‑efficient merging

### 1. Data Import (Executed only once)
load_ISRaD_data <- function() {
  file_url <- "https://raw.githubusercontent.com/International-Soil-Radiocarbon-Database/ISRaD/28bd3bed4346c8bcf9a7fa76df80158caac0e400/ISRaD_data_files/database/ISRaD_extra.rda"
  temp_file <- tempfile(fileext = ".rda")
  download.file(file_url, destfile = temp_file, mode = "wb")
  data_env <- new.env()
  loaded_names <- load(temp_file, envir = data_env)
  unlink(temp_file)
  if (length(loaded_names) == 1) {
    return(data_env[[loaded_names]])
  } else {
    return(as.list(data_env))
  }
}
ISRaD_data <- load_ISRaD_data()

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "ISRaD Integrated App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", tabName = "filters", icon = icon("filter")),
      menuItem("Map", tabName = "map", icon = icon("globe")),
      menuItem("Summary", tabName = "summary", icon = icon("table")),
      menuItem("Download", tabName = "download", icon = icon("download")),
      menuItem("Data Display", tabName = "display", icon = icon("table")),
      menuItem("Documentation", tabName = "documentation", icon = icon("book"))  # Added documentation menu
    )
  ),
  dashboardBody(
    # Hidden input for hierarchical selection (updated in server)
    tags$div(style = "display:none;",
             textInput("hierarchySelected", label = NULL, value = "")
    ),
    tabItems(
      # ---- Filters Tab ----
      tabItem(tabName = "filters",
              fluidRow(
                box(title = "Always Included", width = 12,
                    "Metadata, Site, and Profile are always included."),
                box(title = "Additional Data Tables", width = 6,
                    checkboxGroupInput("add_tables", 
                                       label = "Select Additional Tables:",
                                       choices = c("Layer", "Flux", "Interstitial", "Fraction", "Incubation"),
                                       selected = NULL),
                    conditionalPanel(
                      condition = "input.add_tables.indexOf('Incubation') != -1 && (input.add_tables.indexOf('Layer') == -1 && input.add_tables.indexOf('Fraction') == -1)",
                      radioButtons("incubation_dependency", "For Incubation, select dependency:",
                                   choices = c("Layer", "Fraction", "Both"), selected = "Both")
                    )
                ),
                box(title = "Hierarchical Selection Output", width = 6,
                    verbatimTextOutput("hierarchyOutput")
                )
              ),
              fluidRow(
                # Layer filters appear if "Layer" is in the hierarchical selection
                box(title = "Layer Table Filters", width = 6,
                    conditionalPanel(
                      condition = "input.hierarchySelected.indexOf('Layer') != -1",
                      uiOutput("yearUI"),
                      uiOutput("depthRangeUI")
                    )
                ),
                # Fraction filters appear if "Fraction" is in the hierarchical selection
                box(title = "Fraction Table Filters", width = 6,
                    conditionalPanel(
                      condition = "input.hierarchySelected.indexOf('Fraction') != -1",
                      uiOutput("fractionSchemeUI")
                    )
                )
              ),
              # Box showing number of available sites from current filters
              fluidRow(
                box(title = "Sites Available Based on Current Filters",
                    width = 12,
                    textOutput("available_sites_text")
                )
              )
      ),
      
      # ---- Map Tab ----
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Site Map (Spatial Filter)", width = 12,
                    leafletOutput("map_leaflet", height = "500px"),
                    br(),
                    actionButton("clear_shapes", "Clear Spatial Selection"),
                    verbatimTextOutput("map_info")
                )
              )
      ),
      
      # ---- Summary Tab ----
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Summary Statistics", width = 12,
                    tableOutput("summary_table")
                )
              )
      ),
      
      # ---- Download Tab ----
      tabItem(tabName = "download",
              fluidRow(
                box(title = "Download Options", width = 6,
                    radioButtons("download_cols", "Columns to Download:",
                                 choices = c("All columns", "Only columns with data", "Only rows with radiocarbon values"),
                                 selected = "All columns"),
                    textInput("search_term", "Search across table:", value = "")
                ),
                box(title = "Flattened Data Preview", width = 12,
                    DTOutput("flat_table")
                ),
                box(title = "Download Data", width = 12,
                    downloadButton("download_csv", "Download CSV")
                )
              )
      ),
      
      # ---- Data Display Tab ----
      tabItem(tabName = "display",
              fluidRow(
                box(title = "Flattened Data (Filtered)", width = 12,
                    DTOutput("site_table")
                )
              )
      ),
      
      # ---- Documentation Tab ----
      tabItem(tabName = "documentation",
              p("Summary Documentation for ISRaD Integrated App"),
              p("The ISRaD Integrated App is a Shiny-based web application designed to interact with the ISRaD (International Soil Radiocarbon Database) dataset. The app offers various functionalities to filter, display, and download data, as well as explore the dataset's geographical distribution."),
              h3("User Interface (UI) Layout"),
              p("The UI is built consisting of multiple tabs, each providing specific functionality:"),
              h4("Filters Tab:"),
              p("Allows the user to select additional data tables (e.g., Layer, Flux, Interstitial, Fraction, Incubation)."),
              p("Offers filtering by observation year, depth range, and fraction scheme for the relevant tables."),
              p("Displays the number of available sites based on selected filters."),
              h4("Map Tab:"),
              p("Displays a leaflet map showing the geographical locations of sites, with dynamic interaction capabilities (drawing shapes on the map to filter data based on spatial selection)."),
              p("Users can draw polygons or rectangles on the map to select sites within a specified geographic region."),
              h4("Summary Tab:"),
              p("Provides a summary table displaying basic statistics, such as the number of entries for each data table (Site, Profile, Layer, etc.)."),
              h4("Download Tab:"),
              p("Offers options for downloading data in CSV format based on selected columns and rows."),
              p("Allows users to filter data by specific criteria like (Only columns with data) or (Only rows with radiocarbon values)."),
              h4("Data Display Tab:"),
              p("Displays a flattened view of the data, combining selected tables into a single data table for review and analysis.")
      )
    )  # End of tabItems
  )  # End of dashboardBody
)

### 3. Server Definition
server <- function(input, output, session) {
  
  #### Hierarchical / Dependency Handling ####
  base_tables <- c("Metadata", "Site", "Profile")
  additional_tables <- reactive({
    sel <- input$add_tables
    if ("Fraction" %in% sel && !("Layer" %in% sel)) {
      sel <- union(sel, "Layer")
    }
    if ("Incubation" %in% sel && !("Layer" %in% sel) && !("Fraction" %in% sel)) {
      dep <- input$incubation_dependency
      if (dep == "Layer") {
        sel <- union(sel, "Layer")
      } else if (dep == "Fraction") {
        sel <- union(sel, "Fraction")
      } else if (dep == "Both") {
        sel <- union(sel, c("Layer", "Fraction"))
      }
    }
    sel
  })
  selected_tables <- reactive({
    c(base_tables, additional_tables())
  })
  
  # Update the hidden hierarchySelected input for conditional panels.
  observe({
    updateTextInput(session, "hierarchySelected", value = paste(selected_tables(), collapse = ","))
  })
  
  output$hierarchyOutput <- renderPrint({
    cat("User selected additional tables:", paste(input$add_tables, collapse = ", "), "\n")
    cat("Actual hierarchical selection:", paste(selected_tables(), collapse = ", "), "\n")
  })
  
  #### Layer Filtering Section ####
  output$yearUI <- renderUI({
    layer_years <- as.numeric(as.character(ISRaD_data$layer$lyr_obs_date_y))
    layer_years <- layer_years[!is.na(layer_years) & !is.infinite(layer_years)]
    if (length(layer_years) == 0) return(helpText("No valid year data."))
    sliderInput("year_range", "Observation Year (lyr_obs_date_y):",
                min = min(layer_years), max = max(layer_years),
                value = c(min(layer_years), max(layer_years)),
                step = 1, sep = "")
  })
  
  output$depthRangeUI <- renderUI({
    top_vals <- as.numeric(as.character(ISRaD_data$layer$lyr_top))
    bot_vals <- as.numeric(as.character(ISRaD_data$layer$lyr_bot))
    top_vals <- top_vals[!is.na(top_vals) & !is.infinite(top_vals)]
    bot_vals <- bot_vals[!is.na(bot_vals) & !is.infinite(bot_vals)]
    all_depths <- c(top_vals, bot_vals)
    if (length(all_depths) == 0) return(helpText("No valid depth data."))
    sliderInput("depth_range", "Sampling Depth Range:",
                min = floor(min(all_depths)), max = ceiling(max(all_depths)),
                value = c(floor(min(all_depths)), ceiling(max(all_depths))),
                step = 1, sep = "")
  })
  
  output$fractionSchemeUI <- renderUI({
    schemeChoices <- unique(ISRaD_data$fraction$frc_scheme)
    schemeChoices <- schemeChoices[!is.na(schemeChoices)]
    schemeChoices <- sort(schemeChoices)
    if (length(schemeChoices) == 0) return(helpText("No fraction scheme data."))
    checkboxGroupInput("fraction_scheme", "Fraction Scheme (frc_scheme):",
                       choices = schemeChoices, selected = schemeChoices)
  })
  
  filtered_layer <- reactive({
    req(input$year_range, input$depth_range)
    ISRaD_data$layer %>%
      mutate(
        yearNum = as.numeric(as.character(lyr_obs_date_y)),
        topNum  = as.numeric(as.character(lyr_top)),
        botNum  = as.numeric(as.character(lyr_bot))
      ) %>%
      filter(!is.na(yearNum), !is.na(topNum), !is.na(botNum),
             !is.infinite(topNum), !is.infinite(botNum)) %>%
      filter(yearNum >= input$year_range[1],
             yearNum <= input$year_range[2],
             topNum  >= input$depth_range[1],
             botNum  <= input$depth_range[2])
  })
  
  output$layer_table <- renderDT({
    datatable(filtered_layer(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  #### Map Section: Dynamic Map Based on Filtered Layer ####
  # Define reactive for sites based on slider filters
  filtered_sites <- reactive({
    if ("Layer" %in% selected_tables()) {
      fl <- filtered_layer()
      valid_entries <- unique(fl$entry_name)
      if (length(valid_entries) > 0) {
        ISRaD_data$site %>% 
          filter(entry_name %in% valid_entries,
                 !is.na(site_lat), !is.na(site_long))
      } else {
        ISRaD_data$site[0, ]
      }
    } else {
      ISRaD_data$site %>% filter(!is.na(site_lat), !is.na(site_long))
    }
  })
  
  # This reactive converts the filtered sites to an sf object.
  filtered_site_sf <- reactive({
    st_as_sf(filtered_sites(), coords = c("site_long", "site_lat"), crs = 4326, remove = FALSE)
  })
  
  output$map_leaflet <- renderLeaflet({
    df <- filtered_sites()
    center_lng <- if(nrow(df) > 0) mean(df$site_long, na.rm = TRUE) else 0
    center_lat <- if(nrow(df) > 0) mean(df$site_lat, na.rm = TRUE) else 0
    
    leaflet(filtered_site_sf()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = center_lng, lat = center_lat, zoom = 3) %>%
      addCircleMarkers(
        color = "blue",
        radius = 4,
        popup = ~paste("Site:", as.character(site_name),
                       "<br>Lat:", site_lat, "Long:", site_long)
      ) %>%
      addDrawToolbar(
        targetGroup = "draw",
        polylineOptions = FALSE,
        rectangleOptions = TRUE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        polygonOptions = TRUE,
        editOptions = editToolbarOptions()
      )
  })
  
  drawn_shapes <- reactiveVal(list())
  
  polygonFeatureToSf <- function(feature) {
    geojson_txt <- toJSON(as.list(feature$geometry), auto_unbox = TRUE)
    sfobj <- geojson_sf(geojson_txt)
    st_set_crs(sfobj, 4326)
  }
  
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    if (feature$geometry$type %in% c("Polygon", "MultiPolygon")) {
      poly_sf <- polygonFeatureToSf(feature)
      old_shapes <- drawn_shapes()
      drawn_shapes(c(old_shapes, list(poly_sf)))
    }
  })
  
  observeEvent(input$clear_shapes, {
    drawn_shapes(list())
  })
  
  # Further filter the sites with drawn shapes if any are drawn.
  selected_sites <- reactive({
    shapes <- drawn_shapes()
    if (length(shapes) == 0) {
      return(filtered_site_sf())
    }
    combined <- do.call(st_union, shapes)
    st_intersection(filtered_site_sf(), combined)
  })
  
  output$map_info <- renderPrint({
    sf_obj <- selected_sites()
    if (nrow(sf_obj) == 0) {
      cat("No sites selected via map.\n")
    } else {
      cat("Selected sites via map:\n")
      print(sf_obj$site_name)
    }
  })
  
  #### "Sites Available" Box on the Filters Tab ####
  output$available_sites_text <- renderText({
    if (length(drawn_shapes()) > 0) {
      sites <- selected_sites()
      paste("There are", nrow(sites), "sites available within the drawn region.")
    } else {
      sites <- filtered_sites()
      paste("There are", nrow(sites), "sites available based on the current filters.")
    }
  })
  
  #### Define a Reactive for Final Entry Names ####
  final_entry_names <- reactive({
    # If the user has drawn a shape (spatial filter), use selected_sites();
    # otherwise, use all sites from the slider-based filter.
    if (nrow(selected_sites()) > 0) {
      unique(selected_sites()$entry_name)
    } else {
      unique(filtered_sites()$entry_name)
    }
  })
  
  #### Data Display Section: Flattened Data (Filtered) ####
  flattened_data <- reactive({
    final_entries <- final_entry_names()
    
    # Filter base tables by final_entries
    dt_meta <- as.data.table(ISRaD_data$metadata)[entry_name %in% final_entries]
    dt_site <- as.data.table(ISRaD_data$site)[entry_name %in% final_entries]
    dt_prof <- as.data.table(ISRaD_data$profile)[entry_name %in% final_entries]
    
    setkey(dt_meta, entry_name)
    setkey(dt_site, entry_name)
    setkey(dt_prof, entry_name)
    
    dt_flat <- merge(dt_meta, dt_site, by = "entry_name", all.x = TRUE)
    dt_flat <- merge(dt_flat, dt_prof, by = "entry_name", all.x = TRUE)
    
    # For the Layer table, use the filtered_layer and then restrict to final_entries.
    fl <- filtered_layer()
    if(nrow(fl) > 0) {
      dt_layer <- as.data.table(fl)[entry_name %in% final_entries]
      dt_layer_agg <- dt_layer[, .(
        lyr_names = paste(unique(as.character(lyr_name)), collapse = "; "),
        min_top = min(as.numeric(as.character(lyr_top)), na.rm = TRUE),
        max_bot = max(as.numeric(as.character(lyr_bot)), na.rm = TRUE),
        avg_14c = mean(as.numeric(as.character(lyr_14c)), na.rm = TRUE)
      ), by = entry_name]
      setkey(dt_layer_agg, entry_name)
      dt_flat <- merge(dt_flat, dt_layer_agg, by = "entry_name", all.x = TRUE, allow.cartesian = TRUE)
    }
    
    as.data.frame(dt_flat)
  })
  
  output$flat_table <- renderDT({
    datatable(flattened_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$site_table <- renderDT({
    datatable(flattened_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  #### Download Section ####
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("ISRaD_flattened_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- flattened_data()
      if (input$download_cols == "Only columns with data") {
        df <- df[, colSums(!is.na(df)) > 0]
      }
      if (input$download_cols == "Only rows with radiocarbon values") {
        rcols <- c()
        if ("lyr_14c" %in% colnames(df)) { rcols <- c(rcols, "lyr_14c") }
        if ("flx_14c" %in% colnames(df)) { rcols <- c(rcols, "flx_14c") }
        if ("ist_14c" %in% colnames(df)) { rcols <- c(rcols, "ist_14c") }
        if ("frc_14c" %in% colnames(df)) { rcols <- c(rcols, "frc_14c") }
        if ("inc_14c" %in% colnames(df)) { rcols <- c(rcols, "inc_14c") }
        if (length(rcols) > 0) {
          df <- df[apply(df[, rcols, drop = FALSE], 1, function(x) any(!is.na(x))), ]
        }
      }
      if (nchar(input$search_term) > 0) {
        term <- tolower(input$search_term)
        df <- df[apply(df, 1, function(row) {
          any(grepl(term, tolower(as.character(row)), fixed = TRUE))
        }), ]
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  #### Summary Section ####
  output$summary_table <- renderTable({
    flat <- flattened_data()
    summary_stats <- data.frame(
      Table = c("Site", "Profile"),
      Entries = c(length(unique(flat$site_name)),
                  length(unique(flat$pro_name))),
      stringsAsFactors = FALSE
    )
    if ("Layer" %in% selected_tables()) {
      summary_stats <- rbind(summary_stats,
                             data.frame(Table = "Layer", Entries = length(unique(flat$lyr_name))))
    }
    if ("Flux" %in% selected_tables()) {
      summary_stats <- rbind(summary_stats,
                             data.frame(Table = "Flux", Entries = length(unique(flat$flx_name))))
    }
    if ("Interstitial" %in% selected_tables()) {
      summary_stats <- rbind(summary_stats,
                             data.frame(Table = "Interstitial", Entries = length(unique(flat$ist_name))))
    }
    if ("Fraction" %in% selected_tables()) {
      summary_stats <- rbind(summary_stats,
                             data.frame(Table = "Fraction", Entries = length(unique(flat$frc_name))))
    }
    if ("Incubation" %in% selected_tables()) {
      summary_stats <- rbind(summary_stats,
                             data.frame(Table = "Incubation", Entries = length(unique(flat$inc_name))))
    }
    summary_stats
  })
}

shinyApp(ui, server)
