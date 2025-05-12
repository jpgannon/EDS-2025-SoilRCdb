# Allow cartesian joins (use with caution – this can result in very large outputs)
options(datatable.allow.cartesian = TRUE)

library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(leaflet.extras)
library(sf)
# Disable the s2 geometry engine to avoid errors with st_union on large/global geometries.
sf_use_s2(FALSE)
library(dplyr)
library(geojsonsf)
library(jsonlite)
library(data.table)  # For memory‑efficient merging
library(rnaturalearth)
library(rnaturalearthdata)

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

### Compute default ranges for the sliders from the entire layer dataset.
default_year_range <- {
  layer_years <- as.numeric(as.character(ISRaD_data$layer$lyr_obs_date_y))
  layer_years <- layer_years[!is.na(layer_years) & !is.infinite(layer_years)]
  c(min(layer_years), max(layer_years))
}
default_depth_range <- {
  top_vals <- as.numeric(as.character(ISRaD_data$layer$lyr_top))
  bot_vals <- as.numeric(as.character(ISRaD_data$layer$lyr_bot))
  all_depths <- c(top_vals, bot_vals)
  all_depths <- all_depths[!is.na(all_depths) & !is.infinite(all_depths)]
  c(floor(min(all_depths)), ceiling(max(all_depths)))
}

### 2. UI Definition with Custom CSS
customCSS <- "
/* Use Google Fonts */
@import url('https://fonts.googleapis.com/css?family=Lato:400,700|Roboto:400,700');

body, .content-wrapper, .right-side {
  font-family: 'Lato', sans-serif;
  background-color: #ecf0f5;
}
.main-header .logo {
  background-color: #2c3e50 !important;
  font-family: 'Roboto', sans-serif;
  font-size: 24px;
}
.main-header .navbar {
  background-color: #2c3e50 !important;
}
.sidebar {
  background-color: #34495e;
}
.sidebar-menu > li > a {
  font-size: 16px;
  color: #ecf0f5 !important;
}
.box {
  border-top: 3px solid #2980b9;
  box-shadow: 2px 2px 5px rgba(0,0,0,0.1);
  margin-bottom: 20px;
}
.box-title {
  font-family: 'Roboto', sans-serif;
  font-size: 18px;
  font-weight: bold;
}
.btn, .btn-primary {
  background-color: #2980b9 !important;
  border-color: #2980b9 !important;
}
.leaflet-container {
  border: 2px solid #2980b9;
}
"

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "ISRaD Integrated App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", tabName = "filters", icon = icon("filter")),
      menuItem("Map", tabName = "map", icon = icon("globe")),
      menuItem("Summary", tabName = "summary", icon = icon("table")),
      menuItem("Download", tabName = "download", icon = icon("download")),
      menuItem("Data Display", tabName = "display", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(customCSS))),
    tags$div(style = "display:none;", 
             textInput("hierarchySelected", label = NULL, value = "")
    ),
    tabItems(
      # ---- Filters Tab ----
      tabItem(tabName = "filters",
              fluidRow(
                box(title = "Tab Description", width = 12,
                    "Filter sites based on various table filters.
                    These filters can be used in any order. The final available sites will reflect both these settings and any spatial selections made on the map.")
              ),
              fluidRow(
                box(title = "Always Included", width = 12,
                    "Metadata, Site, and Profile are always included.", status = "primary")
              ),
              fluidRow(
                box(title = "Additional Data Tables", width = 6,
                    checkboxGroupInput("add_tables", 
                                       label = "Select Additional Tables:",
                                       choices = c("Layer", "Flux", "Interstitial", "Fraction", "Incubation"),
                                       selected = NULL),
                    conditionalPanel(
                      condition = "input.add_tables.indexOf('Incubation') != -1 && (input.add_tables.indexOf('Layer') == -1 && input.add_tables.indexOf('Fraction') == -1)",
                      radioButtons("incubation_dependency", "For Incubation, select dependency:",
                                   choices = c("Layer", "Fraction", "Both"), selected = "Both")
                    ),
                    status = "info"
                ),
                box(title = "Hierarchical Selection Output", width = 6,
                    verbatimTextOutput("hierarchyOutput"),
                    status = "info")
              ),
              fluidRow(
                box(title = "Layer Table Filters", width = 6,
                    conditionalPanel(
                      condition = "input.hierarchySelected.indexOf('Layer') != -1",
                      uiOutput("yearUI"),
                      uiOutput("depthRangeUI")
                    ),
                    status = "primary"
                ),
                box(title = "Fraction Table Filters", width = 6,
                    conditionalPanel(
                      condition = "input.hierarchySelected.indexOf('Fraction') != -1",
                      uiOutput("fractionSchemeUI")
                    ),
                    status = "primary")
              ),
              fluidRow(
                box(title = "Sites Available Based on Current Filters",
                    width = 12,
                    textOutput("available_sites_text"),
                    status = "success")
              )
      ),
      # ---- Map Tab ----
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Tab Description", width = 12,
                    "View sites based on applied filters. You can also select sites by drawing polygons
                     or by using coordinate and country filters. These spatial selections update the final available sites, regardless of whether you filter first or later.")
              ),
              fluidRow(
                box(title = "Coordinate Filter", width = 12,
                    checkboxInput("activate_coord_filter", "Activate Coordinate Filter", value = FALSE),
                    conditionalPanel(
                      condition = "input.activate_coord_filter == true",
                      sliderInput("long_range", "Longitude Range:", min = -180, max = 180,
                                  value = c(-180, 180), step = 1),
                      sliderInput("lat_range", "Latitude Range:", min = -90, max = 90,
                                  value = c(-90, 90), step = 1)
                    ),
                    status = "warning")
              ),
              fluidRow(
                box(title = "Country Filter", width = 12,
                    checkboxInput("activate_country_filter", "Activate Country Filter", value = FALSE),
                    conditionalPanel(
                      condition = "input.activate_country_filter == true",
                      uiOutput("country_filter_ui")
                    ),
                    status = "warning")
              ),
              fluidRow(
                box(title = "Site Map (Spatial Filter)", width = 12,
                    leafletOutput("map_leaflet", height = "500px"),
                    br(),
                    actionButton("clear_shapes", "Clear Spatial Selection"),
                    verbatimTextOutput("map_info"),
                    status = "primary")
              )
      ),
      # ---- Summary Tab ----
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Tab Description", width = 12,
                    "View summary statistics of the filtered data.")
              ),
              fluidRow(
                box(title = "Summary Statistics", width = 12,
                    tableOutput("summary_table"),
                    status = "primary")
              )
      ),
      # ---- Download Tab ----
      tabItem(tabName = "download",
              fluidRow(
                box(title = "Tab Description", width = 12,
                    "Preview and download the flattened filtered data.")
              ),
              fluidRow(
                box(title = "Download Options", width = 6,
                    radioButtons("download_cols", "Columns to Download:",
                                 choices = c("All columns", "Only columns with data", "Only rows with radiocarbon values"),
                                 selected = "All columns"),
                    textInput("search_term", "Search across table:", value = ""),
                    status = "info"),
                box(title = "Flattened Data Preview", width = 12,
                    DTOutput("flat_table"),
                    status = "info"),
                box(title = "Download Data", width = 12,
                    downloadButton("download_csv", "Download CSV"),
                    status = "info")
              )
      ),
      # ---- Data Display Tab ----
      tabItem(tabName = "display",
              fluidRow(
                box(title = "Tab Description", width = 12,
                    "Display the flattened filtered data.")
              ),
              fluidRow(
                box(title = "Flattened Data (Filtered)", width = 12,
                    DTOutput("site_table"),
                    status = "primary")
              )
      )
    )
  )
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
  
  observe({
    updateTextInput(session, "hierarchySelected", value = paste(selected_tables(), collapse = ","))
  })
  
  output$hierarchyOutput <- renderPrint({
    cat("User selected additional tables:", paste(input$add_tables, collapse = ", "), "\n")
    cat("Actual hierarchical selection:", paste(selected_tables(), collapse = ", "), "\n")
  })
  
  #### Layer Filtering Section (Filters Tab) ####
  output$yearUI <- renderUI({
    layer_years <- as.numeric(as.character(ISRaD_data$layer$lyr_obs_date_y))
    layer_years <- layer_years[!is.na(layer_years) & !is.infinite(layer_years)]
    
    if (length(layer_years) == 0) return(helpText("No valid year data."))
    
    slider <- sliderInput("year_range", "Observation Year (lyr_obs_date_y):",
                          min = min(layer_years), max = max(layer_years),
                          value = c(min(layer_years), max(layer_years)),
                          step = 1, sep = "")
    
    year_inputs <- tagList(
      textInput("min_year", "Min Year", value = min(layer_years)),
      textInput("max_year", "Max Year", value = max(layer_years))
    )
    
    tagList(slider, year_inputs)
  })
  
  # Initialize state flag to avoid infinite loop during app initialization
  initialized <- reactiveVal(FALSE)
  
  # Observe changes to the year range slider
  observe({
    # Make sure we only update after the UI has been rendered once
    if (!initialized()) return()
    
    slider_vals <- input$year_range
    # Update text inputs only if the slider values have changed
    updateTextInput(session, "min_year", value = as.character(slider_vals[1]))
    updateTextInput(session, "max_year", value = as.character(slider_vals[2]))
  })
  
  # Observe changes to the min_year text input
  observeEvent(input$min_year, {
    # Ensure the min_year is valid and doesn't exceed max_year
    min_year <- as.numeric(input$min_year)
    max_year <- as.numeric(input$max_year)
    
    if (!is.na(min_year) && !is.na(max_year) && min_year <= max_year) {
      # Only update the slider value, not its min/max
      updateSliderInput(session, "year_range", 
                        value = c(min_year, max_year))
    }
  })
  
  # Observe changes to the max_year text input
  observeEvent(input$max_year, {
    # Ensure the max_year is valid and doesn't go below min_year
    min_year <- as.numeric(input$min_year)
    max_year <- as.numeric(input$max_year)
    
    if (!is.na(min_year) && !is.na(max_year) && min_year <= max_year) {
      # Only update the slider value, not its min/max
      updateSliderInput(session, "year_range", 
                        value = c(min_year, max_year))
    }
  })
  
  # Output selected year range
  output$selected_years <- renderPrint({
    input$year_range
  })
  
  # Set initialized state to TRUE after the first render
  observe({
    if (!initialized()) {
      initialized(TRUE)
    }
  })
  
  
  
  
  
  
  output$depthRangeUI <- renderUI({
    # Ensure we have valid year range data for filtering
    req(input$year_range)  # Ensure year_range input is available
    
    # Filter data based on the selected year range
    filtered_by_year <- ISRaD_data$layer[
      as.numeric(as.character(ISRaD_data$layer$lyr_obs_date_y)) >= input$year_range[1] &
        as.numeric(as.character(ISRaD_data$layer$lyr_obs_date_y)) <= input$year_range[2], ]
    
    # Extract and clean depth values (allow negative depths)
    top_vals <- as.numeric(as.character(filtered_by_year$lyr_top))
    bot_vals <- as.numeric(as.character(filtered_by_year$lyr_bot))
    top_vals <- top_vals[!is.na(top_vals) & !is.infinite(top_vals)]
    bot_vals <- bot_vals[!is.na(bot_vals) & !is.infinite(bot_vals)]
    
    # Combine the top and bottom depth values
    all_depths <- c(top_vals, bot_vals)
    
    # If there are no valid depth values, show a help text
    if (length(all_depths) == 0) return(helpText("No valid depth data available for the selected year range."))
    
    # Calculate valid min and max depths (include negative depths)
    min_depth <- floor(min(all_depths))
    max_depth <- ceiling(max(all_depths))
    
    # Ensure depth range is sensible
    if (min_depth > max_depth) {
      min_depth <- max_depth
    }
    
    # Create the slider input
    slider <- sliderInput("depth_range", "Sampling Depth Range:",
                          min = min_depth, max = max_depth,
                          value = c(min_depth, max_depth),
                          step = 1, sep = "")
    
    # Create text inputs for min and max depths
    depth_inputs <- tagList(
      numericInput("min_depth", "Min Depth", value = min_depth),
      numericInput("max_depth", "Max Depth", value = max_depth)
    )
    
    tagList(slider, depth_inputs)  # Return both the slider and text inputs as a tag list
  })
  
  # Initialize state flag for ensuring UI updates after the first render
  initialized_depth <- reactiveVal(FALSE)
  
  # Observe changes to the depth range slider
  observe({
    req(input$depth_range)  # Ensure depth_range input exists
    
    # Only update text inputs after the UI has been rendered
    if (!initialized_depth()) return()
    
    slider_vals <- input$depth_range
    
    # Update numeric inputs with the correct slider values
    updateNumericInput(session, "min_depth", value = slider_vals[1])
    updateNumericInput(session, "max_depth", value = slider_vals[2])
  })
  
  # Observe changes to the min_depth numeric input and update the slider
  observeEvent(input$min_depth, {
    req(input$min_depth, input$max_depth)  # Ensure both numeric inputs are available
    
    # Sanitize the min and max depth values before updating
    min_depth <- input$min_depth  # These are now numeric values directly
    max_depth <- input$max_depth  # These are now numeric values directly
    
    # Check if the values are valid before proceeding
    if (!is.na(min_depth) && !is.na(max_depth) && min_depth <= max_depth) {
      updateSliderInput(session, "depth_range", value = c(min_depth, max_depth))
    }
  })
  
  # Observe changes to the max_depth numeric input and update the slider
  observeEvent(input$max_depth, {
    req(input$min_depth, input$max_depth)  # Ensure both numeric inputs are available
    
    # Sanitize the min and max depth values before updating
    min_depth <- input$min_depth  # These are now numeric values directly
    max_depth <- input$max_depth  # These are now numeric values directly
    
    # Check if the values are valid before proceeding
    if (!is.na(min_depth) && !is.na(max_depth) && min_depth <= max_depth) {
      updateSliderInput(session, "depth_range", value = c(min_depth, max_depth))
    }
  })
  
  # Output the selected depth range
  output$selected_depths <- renderPrint({
    req(input$depth_range)  # Ensure depth_range input exists
    input$depth_range
  })
  
  # Set initialized state to TRUE after the first render
  observe({
    if (!initialized_depth()) {
      initialized_depth(TRUE)
    }
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
  
  #### Spatial Filtering from Map Interactions ####
  # Coordinate Filter Polygon
  coordinate_filter_polygon <- reactive({
    if (isTRUE(input$activate_coord_filter)) {
      req(input$long_range, input$lat_range)
      coords <- matrix(c(input$long_range[1], input$lat_range[1],
                         input$long_range[2], input$lat_range[1],
                         input$long_range[2], input$lat_range[2],
                         input$long_range[1], input$lat_range[2],
                         input$long_range[1], input$lat_range[1]),
                       ncol = 2, byrow = TRUE)
      poly <- st_polygon(list(coords))
      st_make_valid(st_sfc(poly, crs = 4326))
    } else {
      NULL
    }
  })
  
  #### Country Filter Reactive (Map Tab)
  available_countries <- reactive({
    req(filtered_site_sf())
    countries_sf <- ne_countries(scale = "medium", returnclass = "sf")
    joined <- st_join(filtered_site_sf(), countries_sf, join = st_intersects)
    sort(unique(joined$admin))
  })
  
  output$country_filter_ui <- renderUI({
    req(available_countries())
    selectInput("country_filter", "Select Country:",
                choices = available_countries(),
                selected = available_countries()[1])
  })
  
  country_filter_polygon <- reactive({
    if (isTRUE(input$activate_country_filter)) {
      req(input$country_filter)
      countries_sf <- ne_countries(scale = "medium", returnclass = "sf")
      poly <- countries_sf %>% filter(admin == input$country_filter)
      st_make_valid(st_union(st_geometry(poly)))
    } else {
      NULL
    }
  })
  
  #### Map Section: Dynamic Map Based on Filtered Layer ####
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
  
  # Calculate unique filtered sites
  filtered_sites_unique <- reactive({
    unique(filtered_sites()$entry_name)
  })
  
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
  
  # Overlay the coordinate filter polygon on the map.
  observe({
    poly <- coordinate_filter_polygon()
    if (!is.null(poly)) {
      leafletProxy("map_leaflet") %>% 
        clearGroup("coordFilter") %>%
        addPolylines(data = poly, color = "red", weight = 2, group = "coordFilter")
    } else {
      leafletProxy("map_leaflet") %>% clearGroup("coordFilter")
    }
  })
  
  # Overlay the country filter polygon on the map.
  observe({
    poly <- country_filter_polygon()
    if (!is.null(poly)) {
      leafletProxy("map_leaflet") %>% 
        clearGroup("countryFilter") %>%
        addPolylines(data = poly, color = "green", weight = 8, dashArray = "5,5", group = "countryFilter")
    } else {
      leafletProxy("map_leaflet") %>% clearGroup("countryFilter")
    }
  })
  
  #### Spatial (Map) Selection via Drawn Shapes ####
  drawn_shapes <- reactiveVal(list())
  
  polygonFeatureToSf <- function(feature) {
    geojson_txt <- toJSON(as.list(feature$geometry), auto_unbox = TRUE)
    sfobj <- geojson_sf(geojson_txt)
    st_make_valid(sfobj) %>% st_set_crs(4326)
  }
  
  observeEvent(input$map_leaflet_draw_new_feature, {
    feature <- input$map_leaflet_draw_new_feature
    if (feature$geometry$type %in% c("Polygon", "MultiPolygon")) {
      poly_sf <- polygonFeatureToSf(feature)
      old_shapes <- drawn_shapes()
      drawn_shapes(c(old_shapes, list(poly_sf)))
    }
  })
  
  observeEvent(input$clear_shapes, {
    drawn_shapes(list())
  })
  
  # Final spatial filtering: combine drawn shapes, coordinate filter, and country filter.
  final_entry_names <- reactive({
    sites <- filtered_sites()
    sites_sf <- st_as_sf(sites, coords = c("site_long", "site_lat"), crs = 4326, remove = FALSE)
    sites_sf <- st_make_valid(sites_sf)
    
    tol <- 1  # tolerance in degrees for bounding box comparisons
    
    poly_country <- country_filter_polygon()
    if (!is.null(poly_country)) {
      poly_country <- st_make_valid(poly_country)
      bbox_country <- st_bbox(poly_country)
      bbox_sites <- st_bbox(sites_sf)
      if (abs(bbox_country["xmin"] - bbox_sites["xmin"]) < tol &&
          abs(bbox_country["xmax"] - bbox_sites["xmax"]) < tol &&
          abs(bbox_country["ymin"] - bbox_sites["ymin"]) < tol &&
          abs(bbox_country["ymax"] - bbox_sites["ymax"]) < tol) {
        poly_country <- NULL  # Ignore if nearly global
      }
    }
    
    poly_coord <- coordinate_filter_polygon()
    if (!is.null(poly_coord)) {
      poly_coord <- st_make_valid(poly_coord)
      bbox_coord <- st_bbox(poly_coord)
      bbox_sites <- st_bbox(sites_sf)
      if (abs(bbox_coord["xmin"] - bbox_sites["xmin"]) < tol &&
          abs(bbox_coord["xmax"] - bbox_sites["xmax"]) < tol &&
          abs(bbox_coord["ymin"] - bbox_sites["ymin"]) < tol &&
          abs(bbox_coord["ymax"] - bbox_sites["ymax"]) < tol) {
        poly_coord <- NULL  # Ignore if nearly global
      }
    }
    
    poly_drawn <- if (length(drawn_shapes()) > 0) do.call(st_union, drawn_shapes()) else NULL
    if (!is.null(poly_drawn)) {
      poly_drawn <- st_make_valid(poly_drawn)
      bbox_drawn <- st_bbox(poly_drawn)
      bbox_sites <- st_bbox(sites_sf)
      if (abs(bbox_drawn["xmin"] - bbox_sites["xmin"]) < tol &&
          abs(bbox_drawn["xmax"] - bbox_sites["xmax"]) < tol &&
          abs(bbox_drawn["ymin"] - bbox_sites["ymin"]) < tol &&
          abs(bbox_drawn["ymax"] - bbox_sites["ymax"]) < tol) {
        poly_drawn <- NULL  # Ignore if nearly global
      }
    }
    
    if (!is.null(poly_country)) {
      sites_sf <- st_intersection(sites_sf, poly_country)
    }
    if (!is.null(poly_coord)) {
      sites_sf <- st_intersection(sites_sf, poly_coord)
    }
    if (!is.null(poly_drawn)) {
      sites_sf <- st_intersection(sites_sf, poly_drawn)
    }
    if (nrow(sites_sf) > 0) {
      unique(as.character(sites_sf$entry_name))
    } else {
      character(0)
    }
  })
  
  final_unique_entry_names <- reactive({
    unique(final_entry_names())
  })
  
  output$map_info <- renderPrint({
    if (length(drawn_shapes()) > 0 || !is.null(coordinate_filter_polygon()) || !is.null(country_filter_polygon())) {
      selected <- final_unique_entry_names()
      if (length(selected) == 0) {
        cat("No sites selected via spatial filtering.\n")
      } else {
        cat("Selected sites via spatial filtering (unique entry_names):\n")
        print(selected)
      }
    } else {
      cat("No spatial selection made.\n")
    }
  })
  
  # On the Filters tab, show the count of available sites (using unique entry_names).
  output$available_sites_text <- renderText({
    if (length(drawn_shapes()) > 0 || !is.null(coordinate_filter_polygon()) || !is.null(country_filter_polygon())) {
      sites_count <- length(final_unique_entry_names())
    } else {
      sites_count <- length(unique(filtered_sites()$entry_name))
    }
    paste("There are", sites_count, "sites available based on the current filters.")
  })
  
  #### Data Display Section: Flattened Data (Filtered) ####
  flattened_data <- reactive({
    final_entries <- final_unique_entry_names()
    
    dt_meta <- as.data.table(ISRaD_data$metadata)[entry_name %in% final_entries]
    dt_site <- as.data.table(ISRaD_data$site)[entry_name %in% final_entries]
    dt_prof <- as.data.table(ISRaD_data$profile)[entry_name %in% final_entries]
    
    setkey(dt_meta, entry_name)
    setkey(dt_site, entry_name)
    setkey(dt_prof, entry_name)
    
    dt_flat <- merge(dt_meta, dt_site, by = "entry_name", all.x = TRUE)
    dt_flat <- merge(dt_flat, dt_prof, by = "entry_name", all.x = TRUE)
    
    # Merge additional tables if selected
    if ("Layer" %in% additional_tables()) {
      dt_layer <- as.data.table(filtered_layer())[entry_name %in% final_entries]
      dt_layer_agg <- dt_layer[, .(
        lyr_names = paste(unique(as.character(lyr_name)), collapse = "; "),
        min_top = min(as.numeric(as.character(lyr_top)), na.rm = TRUE),
        max_bot = max(as.numeric(as.character(lyr_bot)), na.rm = TRUE),
        avg_14c = mean(as.numeric(as.character(lyr_14c)), na.rm = TRUE)
      ), by = entry_name]
      setkey(dt_layer_agg, entry_name)
      dt_flat <- merge(dt_flat, dt_layer_agg, by = "entry_name", all.x = TRUE, allow.cartesian = TRUE)
    }
    if ("Flux" %in% additional_tables() && "flux" %in% names(ISRaD_data)) {
      dt_flux <- as.data.table(ISRaD_data$flux)[entry_name %in% final_entries]
      dt_flux_agg <- dt_flux[, .(
        flx_name = paste(unique(as.character(flx_name)), collapse = "; ")
      ), by = entry_name]
      setkey(dt_flux_agg, entry_name)
      dt_flat <- merge(dt_flat, dt_flux_agg, by = "entry_name", all.x = TRUE, allow.cartesian = TRUE)
    }
    if ("Interstitial" %in% additional_tables() && "interstitial" %in% names(ISRaD_data)) {
      dt_ist <- as.data.table(ISRaD_data$interstitial)[entry_name %in% final_entries]
      dt_ist_agg <- dt_ist[, .(
        ist_name = paste(unique(as.character(ist_name)), collapse = "; ")
      ), by = entry_name]
      setkey(dt_ist_agg, entry_name)
      dt_flat <- merge(dt_flat, dt_ist_agg, by = "entry_name", all.x = TRUE, allow.cartesian = TRUE)
    }
    if ("Fraction" %in% additional_tables() && "fraction" %in% names(ISRaD_data)) {
      dt_frc <- as.data.table(ISRaD_data$fraction)[entry_name %in% final_entries]
      dt_frc_agg <- dt_frc[, .(
        frc_name = paste(unique(as.character(frc_name)), collapse = "; ")
      ), by = entry_name]
      setkey(dt_frc_agg, entry_name)
      dt_flat <- merge(dt_flat, dt_frc_agg, by = "entry_name", all.x = TRUE, allow.cartesian = TRUE)
    }
    if ("Incubation" %in% additional_tables() && "incubation" %in% names(ISRaD_data)) {
      dt_inc <- as.data.table(ISRaD_data$incubation)[entry_name %in% final_entries]
      dt_inc_agg <- dt_inc[, .(
        inc_name = paste(unique(as.character(inc_name)), collapse = "; ")
      ), by = entry_name]
      setkey(dt_inc_agg, entry_name)
      dt_flat <- merge(dt_flat, dt_inc_agg, by = "entry_name", all.x = TRUE, allow.cartesian = TRUE)
    }
    
    as.data.frame(dt_flat)
  })
  
  output$flat_table <- renderDT({
    datatable(flattened_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$site_table <- renderDT({
    datatable(flattened_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  #### Download Section
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
  
  #### Summary Section
  output$summary_table <- renderTable({
    flat <- flattened_data()
    # We count using unique(entry_name) for site counts.
    summary_stats <- data.frame(
      Table = c("Site", "Profile"),
      Entries = c(length(unique(flat$entry_name)),
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
  
  #### Diagnostic Check
  observe({
    cat("----- Diagnostic Info -----\n")
    cat("Unique entry_names in site table:", length(unique(ISRaD_data$site$entry_name)), "\n")
    cat("Unique entry_names in layer table:", length(unique(ISRaD_data$layer$entry_name)), "\n")
    cat("Unique entry_names in profile table:", length(unique(ISRaD_data$profile$entry_name)), "\n")
    cat("Filtered sites count (unique):", length(unique(filtered_sites()$entry_name)), "\n")
    cat("Final entry_names (after spatial filtering) count:", length(final_unique_entry_names()), "\n")
    dt_flat <- flattened_data()
    cat("Flattened data records:", nrow(dt_flat), "\n")
    cat("Unique entry_names in flattened data:", length(unique(dt_flat$entry_name)), "\n")
    cat("Bounding Box of Site sf:\n")
    print(st_bbox(filtered_site_sf()))
    cat("---------------------------\n")
  })
}

shinyApp(ui, server)

