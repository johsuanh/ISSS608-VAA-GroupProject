# Load necessary libraries
pacman::p_load(shiny, shinydashboard, highcharter, leaflet, bslib, yaml, bsicons,dplyr,lubridate,zoo,tidyr,ggplot2,ggridges,tibble,geofacet)


# Import Data + Add Region Info
dataset <- readRDS("www/station_data.rds") 



#========================================================== 
## brand.yml and css 
#========================================================== 

# Read the brand.yml file
brand_settings <- yaml.load_file("brand.yml")

# Extract branding values
brand_colors <- brand_settings$color$palette
primary_color <- brand_colors$`sky-blue`
secondary_color <- brand_colors$`light-purple`
background_color <- brand_colors$`off-white`

text_color <- brand_colors$`dark-grey`

# Extract visualization settings
viz_settings <- brand_settings$visualization
component_settings <- brand_settings$components

# Custom CSS based on brand.yml
custom_css <- paste0('
  /* Main Layout */
  body {
    font-family: "', brand_settings$typography$base$family, '", sans-serif;
    font-size: ', brand_settings$typography$base$size, ';
    background-color: ', background_color, ' !important;
    color: ', brand_colors$`dark-grey`, ';
    line-height: ', brand_settings$typography$base$line_height, ';
  }
  
  /* Statistics Boxes Styling */
  .stat-box {
    background-color: white;
    border-radius: 4px;
    padding: 15px;
    margin-bottom: 15px;
    border-left: 3px solid ', primary_color, ';
    box-shadow: 0 1px 3px rgba(0,0,0,0.08);
  }
  
  .stat-box h5 {
    color: ', brand_colors$`dark-grey`, ';
    font-family: "', brand_settings$typography$headings$family, '", sans-serif;
    font-weight: ', brand_settings$typography$headings$weight, ';
    margin-top: 0;
    margin-bottom: 10px;
    padding-bottom: 8px;
    border-bottom: 1px solid ', brand_colors$`light-grey`, ';
  }
  
  .stat-box strong {
    color: ', brand_colors$`dark-grey`, ';
    font-weight: 600;
  }
  
  /* Add spacing between stat items */
  .stat-box br {
    line-height: 2;
  }
  
  /* Style for the statistics container */
  .statistics-container {
    background-color: ', background_color, ';
    border-radius: 4px;
    padding: 15px;
    margin-top: 20px;
  }
  
  .statistics-container h4 {
    color: ', brand_colors$`dark-grey`, ';
    font-family: "', brand_settings$typography$headings$family, '", sans-serif;
    font-weight: ', brand_settings$typography$headings$weight, ';
    margin-top: 0;
    margin-bottom: 15px;
  }
  
  /* Remove padding between columns */
  .row {
    margin-left: 0 !important;
    margin-right: 0 !important;
  }
  
  .col-sm-3, .col-sm-9 {
    padding-left: 0 !important;
    padding-right: 0 !important;
  }

  /* Adjust box margins to compensate for removed padding */
  .box {
    margin-bottom: 10px !important;
  }
  
  /* Dashboard Header */
  .skin-blue .main-header .logo { 
    background-color: ', background_color, ' !important; 
    font-family: "', brand_settings$typography$headings$family, '", sans-serif;
    font-weight: ', brand_settings$typography$headings$weight, ';
    color: ', brand_colors$`dark-grey`, ' !important;
    height: 60px;
    line-height: 60px;
  }
  .skin-blue .main-header .navbar { 
    background-color: ', background_color, ' !important; 
    border-bottom: 1px solid ', brand_colors$`light-grey`, ';
    min-height: 60px;
  }
  
  /* Dashboard Sidebar */
  .skin-blue .main-sidebar { 
    background-color: ', background_color, ' !important;
    box-shadow: none !important;
  }

  /* Content wrapper adjustment for taller header */
  .content-wrapper, .right-side {
    background-color: ', background_color, ' !important;
  }
  
  /* Fix sidebar positioning */
  .main-sidebar {
    padding-top: 0 !important;
  }
  
  .sidebar {
    padding-top: 80px !important;  /* Match header height */
  }
  
  /* Adjust main header height */
  .main-header {
    height: 60px !important;
    position: fixed;
    width: 100%;
    z-index: 1000;
  }

  .main-header .navbar {
    height: 60px !important;
    margin-left: 150px;  /* Match titleWidth */
  }

  .main-header .logo {
    height: 60px !important;
    line-height: 60px !important;
    position: fixed;
    width: 150px;  /* Match titleWidth */
    z-index: 1001;
    display: flex;
    align-items: center;
  }

  .main-header .logo img {
    height: 60px;
    width: auto;
  }

  /* Adjust content area */
  .content-wrapper {
    padding-top: 60px !important;
    min-height: calc(100vh - 60px) !important;
  }

  /* Ensure sidebar menu items are visible */
  .sidebar-menu {
    margin-top: 0 !important;
  }

  /* Adjust toggle button position */
  .sidebar-toggle {
    height: 80px !important;
    line-height: 80px !important;
    padding: 0 15px !important;
    position: relative;
    float: left;
  }

  /* Adjust navbar items */
  .navbar-nav > li > a {
    height: 80px !important;
    line-height: 80px !important;
    padding-top: 0 !important;
    padding-bottom: 0 !important;
  }
  
  /* Dashboard Sidebar */
  .skin-blue .main-sidebar .sidebar-menu > li > a {
    color: ', brand_colors$`dark-grey`, ';
    font-family: "', brand_settings$typography$tabs$family, '";
    font-weight: ', brand_settings$typography$tabs$weight, ';
    font-size: ', brand_settings$typography$tabs$size, ';
    border-left: 3px solid transparent;
    padding: 10px 10px;
    transition: all 0.3s ease;
  }

  /* Style for both main menu items and subitems */
  .skin-blue .main-sidebar .sidebar-menu > li > a:hover,
  .skin-blue .main-sidebar .sidebar-menu > li .treeview-menu > li > a:hover {
    background-color: transparent !important;
    color: ', primary_color, ' !important;
    border-left-color: ', primary_color, ' !important;
  }
  
  .skin-blue .main-sidebar .sidebar-menu > li.active > a,
  .skin-blue .main-sidebar .sidebar-menu > li .treeview-menu > li.active > a {
    color: ', primary_color, ' !important;
    background-color: transparent !important;
    border-left-color: ', primary_color, ';
  }


  /* Specific styling for submenu items */
  .skin-blue .sidebar-menu .treeview-menu > li > a {
    color: ', brand_colors$`dark-grey`, ';
    font-family: "', brand_settings$typography$tabs$family, '";
    font-weight: ', brand_settings$typography$tabs$weight, ';
    font-size: calc(', brand_settings$typography$tabs$size, ' * 0.95);
    border-left: 3px solid transparent;
    padding: 8px 5px 8px 25px;
    transition: all 0.3s ease;
  }

  /* Remove default treeview menu background */
  .skin-blue .sidebar-menu .treeview-menu {
    background-color: transparent !important;
    padding-left: 0;
  }

  /* Adjust spacing for submenu items */
  .sidebar-menu .treeview-menu {
    margin: 0 !important;
  }

  /* Remove default arrow color change */
  .skin-blue .sidebar-menu > li > .treeview-menu {
    margin: 0;
    background: transparent;
  }

  .skin-blue .sidebar-menu > li:hover > a,
  .skin-blue .sidebar-menu > li.active > a {
    color: ', primary_color, ' !important;
    background-color: transparent !important;
    border-left-color: ', primary_color, ' !important;
  }
  
  /* Box styling - remove shadows */
  .box {
    border-radius: 5px !important;
    box-shadow: none !important;
    border: none !important;
    padding: ', component_settings$card$padding, ' !important;
    background-color: ', brand_colors$white, ' !important;
    overflow: hidden !important;
  }
  .box-primary {
    border: none !important;
  }
  .box-info {
    border: none !important;
  }

  /* Box header styling */
  .box-header {
    display: flex; /* Enables flexbox */
    align-items: center; /* Centers content vertically */
    justify-content: center; /* Centers content horizontally */
    border: none !important;
    border-radius: 5px !important;
    background-color: ', background_color, ' !important;
    padding: 15px !important;
    text-align: center !important;
    height: 20px; /* Set a height to properly center content */
  }

  .box-header h3 {
    color: ', brand_colors$`dark-grey`, ' !important;
    font-family: "', brand_settings$typography$headings$family, '", sans-serif;
    font-weight: ', brand_settings$typography$headings$weight, ';
    font-size: 14px !important;
    text-align: center !important;
  }


  .box-body {
    border: none !important;
    padding: 15px !important;
  }

  .box-footer {
    border: none !important;
    background-color: transparent !important;
    padding: 0 15px 15px 15px !important;
    border-radius: 5px !important;
  }

  /* Remove borders from all box types */
  .box.box-solid {
    border: none !important;
  }
  .box.box-solid.box-primary {
    border: none !important;
  }
  .box.box-solid.box-info {
    border: none !important;
  }
  .box.box-solid.box-success {
    border: none !important;
  }
  .box.box-solid.box-warning {
    border: none !important;
  }
  .box.box-solid.box-danger {
    border: none !important;
  }
  
  /* Form elements based on component settings */
  .selectize-input, .selectize-dropdown, .form-control {
    background: ', component_settings$dropdown$background, ' !important;
    border: ', component_settings$dropdown$border, ' !important;
    border-radius: ', component_settings$dropdown$border_radius, ' !important;
    font-family: "', brand_settings$typography$base$family, '", sans-serif;
  }
  
  /* Slider styling based on component settings */
  .irs-bar, .irs-bar-edge {
    background: ', component_settings$slider$thumb_color, ' !important;
    border-color: ', component_settings$slider$thumb_color, ' !important;
  }
  .irs-line {
    background: ', component_settings$slider$track_color, ' !important;
  }
  
  /* Tab styling based on typography settings */
  .nav-tabs-custom > .nav-tabs {
    border-bottom: 1px solid ', brand_colors$`light-grey`, ' !important;
    background-color: transparent !important; #tab-box header background
    margin-bottom: 0 !important;
    padding: 0 15px 0 0 !important;
  }

  .nav-tabs-custom > .nav-tabs > li.active > a {
    color: ', brand_colors$black, ' !important;
    font-weight: ', brand_settings$typography$tabs$weight, ';
    border: none !important;
    background-color: ', background_color, ' !important;
    padding: 12px 16px;
    margin-top: 0px !important;
  }
  
  .nav-tabs-custom > .nav-tabs > li > a {
    color: ', brand_colors$grey, ' !important;
    font-weight: ', brand_settings$typography$tabs$weight, ';
    background-color: transparent;
    border: none !important;
    padding: 12px 16px;
  }
  
  .nav-tabs-custom > .nav-tabs > li > a:hover {
    color: ', brand_colors$`dark-grey`, ' !important;
    border: none !important;
  }

  .nav-tabs-custom {
    border: none !important;
    background-color: transparent !important;
  }

  .tab-content {
    border: none !important;
    background-color: white !important;
    padding-top: 15px !important;
  }
  
  /* Plot styling */
  .shiny-plot-output {
    background-color: ', viz_settings$plots$background, ' !important;
    border-radius: 2px !important;
    padding: 10px;
    overflow: hidden !important;
  }

  /* Remove other shadows and adjust transitions */
  .navbar {
    box-shadow: none !important;
  }

  .content-wrapper {
    box-shadow: none !important;
  }

  .sidebar-toggle:hover {
    background-color: transparent !important;
  }

  /* Custom link styling */
  .sidebar-menu-item a {
    color: ', brand_colors$`dark-grey`, ' !important;
    transition: color 0.3s ease;
  }

  .sidebar-menu-item a:hover {
    color: ', primary_color, ' !important;
    background-color: transparent !important;
    text-decoration: none;
  }

  /* Style the collapse icon */
  .sidebar-toggle {
    color: ', primary_color, ' !important;
    transition: opacity 0.3s ease !important;
  }

  .sidebar-toggle:hover {
    background-color: transparent !important;
    opacity: 0.8;
  }

  /* Adjust the hamburger icon size and color */
  .sidebar-toggle .icon-bar {
    background-color: ', primary_color, ' !important;
  }

  /* Remove any title related spacing */
  .main-header .logo {
    justify-content: center !important;
    padding: 0 !important;
  }

  .main-header .logo img {
    height: 50px;
    width: auto;
    margin: 0 !important;
  }



  /* Button styling */
  .btn-primary {
    background-color: ', primary_color, ' !important;
    border-color: ', primary_color, ' !important;
    color: white !important;
    font-family: "', brand_settings$typography$base$family, '" !important;
    font-weight: 500 !important;
    padding: 6px 12px !important;
    border-radius: 4px !important;
    transition: all 0.3s ease !important;
  }

  .btn-primary:hover {
    opacity: 0.85 !important;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important;
  }

  .btn-primary:active {
    transform: translateY(1px) !important;
  }

  .action-button {
    width: 100% !important;
    margin-top: 10px !important;
    margin-bottom: 5px !important;
  }

  /* Update button specific styling */
  #update_overview, #update, #update_RidgePlot, #update_network {
    background-color: ', primary_color, ' !important;
    border: none !important;
    font-size: 13px !important;
    height: 34px !important;
    margin-top: 15px !important;
  }


  /* Ensure submenu appears on hover */
  .sidebar-mini.sidebar-collapse .sidebar-menu > li:hover > .treeview-menu {
    display: block !important;
    position: absolute !important;
    left: 50px !important;
    top: 44px !important;
    width: 180px !important;
    background-color: white !important;
  }

  /* Fix icon alignment in normal state */
  .sidebar-menu > li > a > i {
    width: 20px !important;
    text-align: center !important;
    margin-right: 10px !important;
  }

  /* Ensure proper icon colors */
  .sidebar-menu > li > a > i,
  .sidebar-menu > li > a > span {
    color: ', brand_colors$`dark-grey`, ' !important;
    transition: color 0.3s ease !important;
  }

  .sidebar-menu > li:hover > a > i,
  .sidebar-menu > li.active > a > i {
    color: ', primary_color, ' !important;
  }

  /* Make tab boxes rounder */
  .nav-tabs-custom {
    border-radius: 5px !important;
    overflow: hidden !important;
  }
  
  .tab-content {
    border-radius: 5px !important;
  }

  /* Dashboard Sidebar - make it rounder */
  .main-sidebar { 
    border-radius: 0 !important;
    overflow: hidden !important;
  }
  
  /* Ensure sidebar menu items are properly aligned */
  .sidebar-menu {
    margin-top: 0 !important;
    border-radius: 0 !important;
  }

  /* Remove any rounded corners from sidebar items */
  .sidebar-menu > li > a {
    border-radius: 0 !important;
  }

  .sidebar-menu .treeview-menu {
    border-radius: 0 !important;
  }

  /* Make all input elements rounder */
  .form-control, 
  .selectize-input,
  .selectize-dropdown,
  .daterangepicker,
  .btn,
  .action-button {
    border-radius: 10px !important;
  }

  /* Statistics container rounder corners */
  .statistics-container,
  .stat-box {
    border-radius: 5px !important;
  }



')

# --- Station-Region label choices for UI display (RidgePlot module only) ---
region_mapping_ui <- tibble(
  station = c("Changi", "Marina Barrage", "Ang Mo Kio", "Clementi",
              "Jurong (West)", "Paya Lebar", "Newton", "Pasir Panjang",
              "Tai Seng", "Admiralty"),
  region = c("East", "Central", "North-East", "West",
             "West", "East", "Central", "Central",
             "Central", "North")
)

region_label_choices <- setNames(
  region_mapping_ui$station,
  paste0(region_mapping_ui$station, " (", region_mapping_ui$region, ")")
)

dataset <- dataset %>%
  left_join(region_mapping_ui, by = "station")

#========================================================== 
## UI Components
#========================================================== 
# main header
header <- dashboardHeader(
  title = tags$span(
    tags$img(src = "Logo.png", height = "80px", style = "margin-right: 10px;")
  ),
  titleWidth = 300
)

# main sidebar
sidebar <- dashboardSidebar(
  collapsed = FALSE,
  sidebarMenu(
    id = "sidebar",
    menuItem("Home", tabName = "home", icon = icon("home")),
    
    # EDA Section
    menuItem("Exploratory Analysis", tabName = "EDA", icon = icon("glasses"),
      menuItem(HTML("<b>Time-series Analysis:</b>"), tabName = NULL,icon = icon("calendar-days")),
      menuSubItem("Line Chart", tabName = "LineChart"),
      menuSubItem("Ridge Plot", tabName = "RidgePlot"),
      menuSubItem("Geofacet", tabName = "Geofacet"),
      menuSubItem(HTML("<b>Geospatial Analysis:</b>"), tabName = NULL,icon = icon("map")),
      menuSubItem("Isoline Map", tabName = "geospatial")
    ),
    
    # CDA Section
    menuItem("Co-variation Analysis", tabName = "CDA", icon = icon("check-circle"),
      menuSubItem("Cross-Correlation Function", tabName = "CrossCorrelationFunction"),
      menuSubItem("Cointegration Analysis", tabName = "Cointegration")
      ),
    
    # Forecasting Section
    menuItem("Forecasting", tabName = "Forecasting", icon = icon("chart-line"),
      menuSubItem("Time Series Decomposition", tabName = "decomposition"),
      menuSubItem("Forecasting Models", tabName = "arima")
      ),
    
    # About section
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  )
)

# Define UI components for each tab
# Home tab content
homeTab <- fluidRow(
  box(width = 12,
      h2("Weather Or Not – Predicting the Unpredictable", 
         style = paste0("font-family: '", brand_settings$typography$headings$family, 
                       "', sans-serif; font-weight: ", brand_settings$typography$headings$weight, ";")),
      p("This dashboard provides comprehensive tools for analyzing Singapore's weather patterns, 
        with a focus on temperature and rainfall across different regions."),
      p("Use the sidebar to navigate through three main analysis components:"),
      tags$ul(
        tags$li(tags$strong("Exploratory Analysis:"), "Discover patterns and relationships in weather data through interactive visualizations"),
        tags$li(tags$strong("Confirmatory Analysis:"), "Validate hypotheses about weather patterns using statistical tests"),
        tags$li(tags$strong("Forecasting:"), "Predict future weather patterns using various time series models")
      )
  )
)

# EDA Components（改了）
LineChartTab <- fluidRow(
  column(width = 3,
         box(width = 12, title = "Data Selection", status = "info",
             selectInput("variable_ts", "Select Variable:", 
                         choices = c("Temperature", "Rainfall", "Wind Speed"),
                         selected = "Temperature"),
             selectInput("station_ts", "Select Stations:", 
                         choices = setNames(dataset$station, paste0(dataset$station, " (", dataset$region, ")")),
                         multiple = TRUE,
                         selected = "Changi"),
             selectInput("aggregation", "Time Aggregation:",
                         choices = c("Daily", "Weekly", "Monthly"),
                         selected = "Daily"),
             dateRangeInput("date_range_ts", "Date Range:",
                            start = "2019-01-01", 
                            end = "2025-01-31",
                            separator = " - ",
                            format = "yyyy-mm-dd"),                 
             div(style = "text-align: right;",
                 actionButton("update_ts", "Update View", 
                              class = "btn-primary",
                              icon = icon("refresh"))
             )
         ),
         box(width = 12, title = "Display Options", status = "info",
             checkboxInput("show_monsoon", "Show Monsoon Periods", value = TRUE)  
         )
  ),
  column(width = 9,
         box(width = 12,
             title = "Time Series Analysis: Line Chart",
             status = "primary",
             solidHeader = TRUE,
             fluidRow(
               column(width = 12,
                      highchartOutput("linechart", height = "400px")
               )
             ),
             fluidRow(
               column(width = 12,
                      div(style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 4px;",
                          h4("Summary Statistics"),
                          verbatimTextOutput("summary_stats")
                      )
               )
             )
         )
  )
)

  # Right column - Line Chart
  column(width = 9,
    box(width = 12,
      title = "Time Series Analysis: Line Chart",
      status = "primary",
      solidHeader = TRUE,
      # Line Chart
      fluidRow(
        column(width = 12,
          highchartOutput("linechart", height = "400px")
        )
      ),
      # Summary Statistics
      fluidRow(
        column(width = 12,
          div(style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 4px;",
            h4("Summary Statistics"),
            verbatimTextOutput("summary_stats")
          )
        )
      )
    )
  )


# Data Exploration subtab contents
overviewTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "Dataset Selection", status = "info",
      selectInput("dataset", "Select Dataset:", 
                    choices = c("Temperature", "Rainfall", "Wind", "Humidity"),
                    selected = "Temperature"),
      sliderInput("tempSlider", 
                "Temperature Range:", 
                min = -30, 
                max = 50, 
                value = c(0, 30),
                width = "100%"),
      dateRangeInput("date_range", "Date Range:",
                    start = "2010-01-01", 
                    end = "2020-12-31",
                    separator = " - ",
                    format = "yyyy-mm-dd",
                    width = "100%"),      
      div(style = "text-align: right;",
        actionButton("update_overview", "Update View", 
                    class = "btn-primary",
                    icon = icon("refresh"))
      )
    ),
    box(width = 12, title = "Display Options", status = "info",
      checkboxInput("show_summary", "Show Summary Statistics", value = TRUE),
      checkboxInput("show_missing", "Show Missing Data", value = TRUE)
    )
  ),
  # Right column - Visualizations with subtabs
  column(width = 9,
    tabBox(width = 12,
      title = "Data Analysis",
      id = "overview_tabs",
      tabPanel("Summary Statistics",
        highchartOutput("summary_chart")
      ),
      tabPanel("Data Quality",
        highchartOutput("quality_chart")
      ),
      tabPanel("Trends",
        highchartOutput("trend_chart")
      )
    )
  )
)

# RidgePlot Analysis Tab
RidgePlotTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "Data Selection", status = "info",
      selectInput("var_biv", "Select Variable:", 
                    choices = c("Temperature", "Rainfall", "Wind Speed"),
                    selected = "Temperature"),
      selectInput("station_biv", "Select Stations:", 
                  choices = region_label_choices,
                  multiple = TRUE,
                  selected = c("Changi", "Marina Barrage")),
      selectInput("aggregation_biv", "Time Aggregation:",
                  choices = c("Daily", "Weekly", "Monthly"),
                  selected = "Daily"),
      dateRangeInput("date_range_biv", "Date Range:",
                    start = "2020-01-01", 
                    end = "2025-01-31",
                    separator = " - ",
                    format = "yyyy-mm-dd"),                  
      div(style = "text-align: right;",
        actionButton("update_RidgePlot", "Update View", 
                    class = "btn-primary",
                    icon = icon("refresh"))
      )
    )
  ),
  # Right column - Density Plot and Statistics
  column(width = 9,
    box(width = 12,
      title = "RidgePlot Analysis",
      status = "primary",
      solidHeader = TRUE,
      # Density Plot
      fluidRow(
        column(width = 12,
               plotOutput("ridge_plot_biv", height = "500px"))
        )
      ),
      # Summary Statistics by Station
      fluidRow(
        column(width = 12,
          div(style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 4px;",
            h4("Summary Statistics by Station"),
            verbatimTextOutput("station_stats_biv")
          )
        )
      )
    )
  )


# geofacet:

GeofacetTab <- fluidRow(
  column(width = 12,
         tabBox(width = 12,
                id = "geofacet_tabs",
                title = "Geofacet Analysis",
                
                # Tab 1: All stations shown by region 
                tabPanel("All Stations",
                         fluidRow(
                           column(width = 3,
                                  box(width = 12, title = "Data Selection", status = "info",
                                      selectInput("station_multi", "Select Stations:", 
                                                  choices = setNames(dataset$station, paste0(dataset$station, " (", dataset$region, ")")),
                                                  multiple = TRUE,
                                                  selected = unique(dataset$station)),
                                      selectInput("var_geofacet", "Select Variable:",
                                                  choices = c("Temperature", "Rainfall", "Wind Speed"),
                                                  selected = "Temperature"),
                                      selectInput("aggregation_geofacet", "Time Aggregation:",
                                                  choices = c("Daily", "Weekly", "Monthly"),
                                                  selected = "Daily"),
                                      dateRangeInput("date_range_multi", "Date Range:",
                                                     start = "2020-01-01", 
                                                     end = "2025-01-31",
                                                     separator = " - ",
                                                     format = "yyyy-mm-dd"),
                                      div(style = "text-align: right;",
                                          actionButton("update_Geofacet", "Update View", 
                                                       class = "btn-primary",
                                                       icon = icon("refresh"))
                                      )
                                  ),
                                  box(width = 12, title = "Display Options", status = "info",
                                      checkboxInput("show_trend_multi", "Show Trend Lines", value = TRUE),
                                      checkboxInput("show_labels", "Show Labels", value = TRUE)
                                  )
                           ),
                           column(width = 9,
                                  box(width = 12,
                                      title = "Geofacet Visualization",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      plotOutput("geofacet_plot", height = "600px")
                                  )
                           )
                         )
                ),
                
                # Tab 2: User selects individual stations to view them separately
                tabPanel("Selected Stations",
                         fluidRow(
                           column(width = 3,
                                  box(width = 12, title = "Station Selection", status = "info",
                                      selectInput("station_single", "Select Stations:",
                                                  choices = unique(dataset$station),
                                                  multiple = TRUE,
                                                  selected = "Changi"),
                                      selectInput("var_single", "Select Variable:",
                                                  choices = c("Temperature", "Rainfall", "Wind Speed"),
                                                  selected = "Temperature"),
                                      dateRangeInput("date_range_single", "Date Range:",
                                                     start = "2020-01-01", 
                                                     end = "2025-01-31"),
                                      selectInput("aggregation_single", "Time Aggregation:",
                                                  choices = c("Daily", "Weekly", "Monthly"),
                                                  selected = "Daily"),
                                      div(style = "text-align: right;",
                                          actionButton("update_single", "Update View", 
                                                       class = "btn-primary",
                                                       icon = icon("refresh"))
                                      )
                                  )
                           ),
                           column(width = 9,
                                  box(width = 12,
                                      title = "Selected Station Trend(s)",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      plotOutput("station_line_plot", height = "600px")
                                  )
                           )
                         )
                )
         )
  )
)



###
geospatialTab <- fluidRow(
  column(width = 3,
         box(width = 12, title = "Map Settings", status = "info",
             selectInput("variable_map", "Select Variable:", 
                         choices = c("Temperature", "Rainfall", "Wind Speed"),
                         selected = "Temperature"),
             selectInput("time_period", "Time Period:",
                         choices = c("Daily", "Monthly", "Yearly"),
                         selected = "Monthly"),
             selectInput("station_map", "Select Stations:", 
                         choices = unique(dataset$station),
                         multiple = TRUE,
                         selected = "Changi"),
             dateRangeInput("date_range_map", "Date Range:",
                            start = "2020-01-01", 
                            end = "2025-01-31",
                            separator = " - ",
                            format = "yyyy-mm-dd"),
             div(style = "text-align: right;",
                 actionButton("update_map", "Update View", 
                              class = "btn-primary",
                              icon = icon("refresh"))
             )
         ),
         box(width = 12, title = "Layer Options", status = "info",
             checkboxInput("show_stations", "Show Weather Stations", value = TRUE),
             checkboxInput("show_planning_areas", "Show Planning Areas", value = TRUE),
             checkboxInput("show_income_layer", "Show Income Distribution", value = FALSE),
             checkboxInput("show_age_layer", "Show Age Distribution", value = FALSE)
         )
  ),
  column(width = 9,
         tabBox(width = 12,
                title = "Geospatial Analysis",
                id = "geospatial_tabs",
                tabPanel("Choropleth Map",
                         leafletOutput("choropleth_map", height = "600px")
                ),
                tabPanel("Station Analysis",
                         highchartOutput("station_comparison")
                ),
                tabPanel("Statistics",
                         verbatimTextOutput("spatial_stats")
                )
         )
  )
)


# Placeholder content for other tabs
CATabs <- fluidRow(
  box(width = 12, 
      h3("Confirmatory Analysis Content Here")
  )
)

ForecastSubTabs <- fluidRow(
  box(width = 12,
      h3("Forecasting Content Here")
  )
)

# CrossCorrelationFunction Tests Tab
CrossCorrelationFunctionTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "Test Settings", status = "info",
        selectInput("station", "Select Station:", choices = unique(dataset$station)),
        selectInput("var1", "Select First Variable:", choices = colnames(dataset)[5:12]),
        selectInput("var2", "Select Second Variable:", choices = colnames(dataset)[5:12]),
        dateRangeInput("date_range", "Select Date Range:", 
                       start = min(dataset$date), end = max(dataset$date)),
        checkboxInput("apply_diff", "Apply Differencing", value = FALSE),
        sliderInput("lag_max", "Max Lag:", min = 1, max = 60, value = 30),
      div(style = "text-align: right;"
        #actionButton("run_CrossCorrelationFunction", "Run Test", 
                    #class = "btn-primary",
                    #icon = icon("play"))
      )
    )
  ),
  # Right column - Results
  column(width = 9,
    tabBox(width = 12,
      title = "Cross Correlation Function Test Results",
      id = "CrossCorrelationFunction_tabs",
      tabPanel("Visual Analysis",
        fluidRow(
          column(width = 10, highchartOutput("qq_plot"))
        )
      ),
      tabPanel("Test Results",
        verbatimTextOutput("shapiro_test")
      ),
      tabPanel("Summary",
        verbatimTextOutput("CrossCorrelationFunction_summary")
      )
    )
  )
)

# Cointegration Tests Tab
CointegrationTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "Analysis Settings", status = "info",
      selectInput("variable1_Cointegration", "Select Variable:",
                 choices = c("Temperature", "Rainfall", "Wind Speed"),
                 selected = "Temperature"),
      selectInput("variable2_Cointegration", "Select Variable:", 
                    choices = c("Temperature", "Rainfall", "Wind Speed"),
                    selected = "Rainfall"),
      selectInput("station_Cointegration", "Select Stations:", 
                    choices = c("Changi", "Marina_Barrage", "Ang_Mo_Kio", "Clementi", 
                              "Jurong_West", "Paya_Lebar", "Newton", "Pasir_Panjang", 
                              "Tai_Seng", "Admiralty"),
                    multiple = TRUE,
                    selected = c("Changi", "Marina_Barrage")),
      conditionalPanel(
        condition = "input.Cointegration_type == 'Two-way Cointegration'",
        selectInput("second_factor", "Second Factor:",
                   choices = c("Month", "Season", "Year"),
                   selected = "Month")
      ),
      dateRangeInput("date_range_Cointegration", "Date Range:",
                    start = "2020-01-01", 
                    end = "2025-01-31",
                    separator = " - ",
                    format = "yyyy-mm-dd"),
      div(style = "text-align: right;",
        actionButton("run_Cointegration", "Run Test", 
                    class = "btn-primary",
                    icon = icon("play"))
      )
    ),
    box(width = 12, title = "Display Options", status = "info",
      checkboxInput("show_boxplots", "Show Box Plots", value = TRUE),
      checkboxInput("show_means", "Show Mean Plot", value = TRUE),
      checkboxInput("show_assumptions", "Show Test Assumptions", value = TRUE)
    )
  ),
  # Right column - Results
  column(width = 9,
    tabBox(width = 12,
      title = "Cointegration Results",
      id = "Cointegration_tabs",
      tabPanel("Visual Analysis",
        fluidRow(
          column(width = 6, highchartOutput("Cointegration_boxplot")),
          column(width = 6, highchartOutput("means_plot"))
        )
      ),
      tabPanel("Test Results",
        verbatimTextOutput("Cointegration_results")
      ),
      tabPanel("Post-hoc Analysis",
        verbatimTextOutput("posthoc_results")
      )
    )
  )
)

# Non-Parametric Tests Tab
nonparametricTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "Test Settings", status = "info",
      selectInput("nonparam_test", "Test Type:",
                 choices = c("Kruskal-Wallis Test"),
                 selected = "Kruskal-Wallis Test"),
      selectInput("variable_nonparam", "Select Variable:", 
                    choices = c("Temperature", "Rainfall", "Wind Speed"),
                    selected = "Temperature"),
      selectInput("station_nonparam", "Select Stations:", 
                    choices = c("Changi", "Marina_Barrage", "Ang_Mo_Kio", "Clementi", 
                              "Jurong_West", "Paya_Lebar", "Newton", "Pasir_Panjang", 
                              "Tai_Seng", "Admiralty"),
                    multiple = TRUE,
                    selected = c("Changi", "Marina_Barrage")),
      dateRangeInput("date_range_nonparam", "Date Range:",
                    start = "2020-01-01", 
                    end = "2025-01-31",
                    separator = " - ",
                    format = "yyyy-mm-dd"),
      div(style = "text-align: right;",
        actionButton("run_nonparam", "Run Test", 
                    class = "btn-primary",
                    icon = icon("play"))
      )
    ),
    box(width = 12, title = "Display Options", status = "info",
      checkboxInput("show_ranks", "Show Rank Plot", value = TRUE),
      checkboxInput("show_distributions", "Show Distributions", value = TRUE),
      checkboxInput("show_pairwise", "Show Pairwise Comparisons", value = TRUE)
    )
  ),
  # Right column - Results
  column(width = 9,
    tabBox(width = 12,
      title = "Non-Parametric Test Results",
      id = "nonparam_tabs",
      tabPanel("Visual Analysis",
        fluidRow(
          column(width = 6, highchartOutput("rank_plot")),
          column(width = 6, highchartOutput("dist_plot"))
        )
      ),
      tabPanel("Test Results",
        verbatimTextOutput("nonparam_results")
      ),
      tabPanel("Pairwise Comparisons",
        verbatimTextOutput("pairwise_results")
      )
    )
  )
)

# Time Series Decomposition Tab
decompositionTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "Decomposition Settings", status = "info",
      selectInput("variable_decomp", "Select Variable:", 
                    choices = c("Temperature", "Rainfall", "Wind Speed"),
                    selected = "Temperature"),
      selectInput("station_decomp", "Select Station:", 
                    choices = c("Changi", "Marina_Barrage", "Ang_Mo_Kio", "Clementi", 
                              "Jurong_West", "Paya_Lebar", "Newton", "Pasir_Panjang", 
                              "Tai_Seng", "Admiralty"),
                    selected = "Changi"),
      selectInput("decomp_method", "Decomposition Method:",
                 choices = c("STL", "Classical"),
                 selected = "STL"),
      dateRangeInput("date_range_decomp", "Date Range:",
                    start = "2020-01-01", 
                    end = "2025-01-31",
                    separator = " - ",
                    format = "yyyy-mm-dd"),
      div(style = "text-align: right;",
        actionButton("run_decomp", "Analyze", 
                    class = "btn-primary",
                    icon = icon("chart-line"))
      )
    ),
    box(width = 12, title = "Display Options", status = "info",
      checkboxInput("show_acf", "Show ACF Plot", value = TRUE),
      checkboxInput("show_pacf", "Show PACF Plot", value = TRUE),
      checkboxInput("show_components", "Show Components", value = TRUE)
    )
  ),
  # Right column - Results
  column(width = 9,
    tabBox(width = 12,
      title = "Time Series Decomposition",
      id = "decomp_tabs",
      tabPanel("Components",
        highchartOutput("stl_plot", height = "600px")
      ),
      tabPanel("Correlation Analysis",
        fluidRow(
          column(width = 6, highchartOutput("acf_plot")),
          column(width = 6, highchartOutput("pacf_plot"))
        )
      ),
      tabPanel("Statistics",
        verbatimTextOutput("decomp_stats")
      )
    )
  )
)

# ARIMA Models Tab
arimaTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "ARIMA Settings", status = "info",
      selectInput("variable_arima", "Select Variable:", 
                    choices = c("Temperature", "Rainfall", "Wind Speed"),
                    selected = "Temperature"),
      selectInput("station_arima", "Select Station:", 
                    choices = c("Changi", "Marina_Barrage", "Ang_Mo_Kio", "Clementi", 
                              "Jurong_West", "Paya_Lebar", "Newton", "Pasir_Panjang", 
                              "Tai_Seng", "Admiralty"),
                    selected = "Changi"),
      selectInput("arima_type", "Model Type:",
                 choices = c("Auto ARIMA", "STL ARIMA"),
                 selected = "Auto ARIMA"),
      dateRangeInput("date_range_arima", "Training Period:",
                    start = "2020-01-01", 
                    end = "2024-12-31",
                    separator = " - ",
                    format = "yyyy-mm-dd"),
      numericInput("forecast_period", "Forecast Period (days):",
                  value = 30, min = 1, max = 365),
      div(style = "text-align: right;",
        actionButton("run_arima", "Forecast", 
                    class = "btn-primary",
                    icon = icon("chart-line"))
      )
    ),
    box(width = 12, title = "Model Options", status = "info",
      checkboxInput("show_diagnostics", "Show Model Diagnostics", value = TRUE),
      checkboxInput("show_confidence", "Show Confidence Intervals", value = TRUE),
      checkboxInput("show_accuracy", "Show Accuracy Metrics", value = TRUE)
    )
  ),
  # Right column - Results
  column(width = 9,
    tabBox(width = 12,
      title = "ARIMA Forecasting",
      id = "arima_tabs",
      tabPanel("Forecast Plot",
        highchartOutput("arima_forecast", height = "500px")
      ),
      tabPanel("Model Diagnostics",
        highchartOutput("arima_diagnostics")
      ),
      tabPanel("Model Summary",
        verbatimTextOutput("arima_summary")
      )
    )
  )
)

# ETS Models Tab
etsTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "ETS Settings", status = "info",
      selectInput("variable_ets", "Select Variable:", 
                    choices = c("Temperature", "Rainfall", "Wind Speed"),
                    selected = "Temperature"),
      selectInput("station_ets", "Select Station:", 
                    choices = c("Changi", "Marina_Barrage", "Ang_Mo_Kio", "Clementi", 
                              "Jurong_West", "Paya_Lebar", "Newton", "Pasir_Panjang", 
                              "Tai_Seng", "Admiralty"),
                    selected = "Changi"),
      selectInput("ets_type", "Model Type:",
                 choices = c("Auto ETS", "Custom ETS"),
                 selected = "Auto ETS"),
      dateRangeInput("date_range_ets", "Training Period:",
                    start = "2020-01-01", 
                    end = "2024-12-31",
                    separator = " - ",
                    format = "yyyy-mm-dd"),
      conditionalPanel(
        condition = "input.ets_type == 'Custom ETS'",
        selectInput("error", "Error Type:", 
                   choices = c("Additive", "Multiplicative"),
                   selected = "Additive"),
        selectInput("trend", "Trend Type:", 
                   choices = c("None", "Additive", "Multiplicative"),
                   selected = "Additive"),
        selectInput("seasonal", "Seasonal Type:", 
                   choices = c("None", "Additive", "Multiplicative"),
                   selected = "Additive")
      ),
      numericInput("forecast_period_ets", "Forecast Period (days):",
                  value = 30, min = 1, max = 365),
      div(style = "text-align: right;",
        actionButton("run_ets", "Forecast", 
                    class = "btn-primary",
                    icon = icon("chart-line"))
      )
    ),
    box(width = 12, title = "Model Options", status = "info",
      checkboxInput("show_components_ets", "Show Components", value = TRUE),
      checkboxInput("show_confidence_ets", "Show Confidence Intervals", value = TRUE),
      checkboxInput("show_accuracy_ets", "Show Accuracy Metrics", value = TRUE)
    )
  ),
  # Right column - Results
  column(width = 9,
    tabBox(width = 12,
      title = "ETS Forecasting",
      id = "ets_tabs",
      tabPanel("Forecast Plot",
        highchartOutput("ets_forecast", height = "500px")
      ),
      tabPanel("Components",
        highchartOutput("ets_components")
      ),
      tabPanel("Model Summary",
        verbatimTextOutput("ets_summary")
      )
    )
  )
)

# About Tab
aboutTab <- fluidRow(
  box(width = 12,
      h2("About Weather Or Not", 
         style = paste0("font-family: '", brand_settings$typography$headings$family, 
                       "', sans-serif; font-weight: ", brand_settings$typography$headings$weight, ";")),
      p("Weather Or Not is a comprehensive weather analysis dashboard that helps users understand and predict weather patterns in Singapore.",
      style = "font-size: 13px;"),
      h3("Data Sources"),
      tags$ul(
        tags$li(HTML("<strong>Weather Data:</strong> Historical weather data from Meteorological Service Singapore (2020-2025)")),
        tags$li(HTML("<strong>Census Data:</strong> Singapore census data (2020), including:")),
        tags$ul(
          tags$li("Monthly Household Income from Work by Planning Area"),
          tags$li("Resident Population by Planning Area and Age Group")
        )
      ),
      h3("Analysis Components"),
      tags$ul(
        tags$li(HTML("<strong>Exploratory Analysis:</strong> Interactive visualizations for understanding weather patterns")),
        tags$li(HTML("<strong>Confirmatory Analysis:</strong> Statistical tests to validate weather-related hypotheses")),
        tags$li(HTML("<strong>Forecasting:</strong> Advanced time series models for weather prediction"))
      ),
      h3("Contact"),
      p("For more information or feedback, please visit our ", 
        tags$a(href = brand_settings$meta$link$home, "website", target = "_blank"), ".")
  )
)

# main body
body <- dashboardBody(
  # Apply custom CSS from brand settings
  tags$head(
    tags$style(HTML(custom_css)),
    # Import Google fonts specified in brand.yml
    tags$link(rel = "stylesheet", 
              href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;600;700&family=Roboto:wght@400;500&display=swap")
  ),
  
  # Content tabs
  tabItems(
    # Home tab
    tabItem(tabName = "home", homeTab),
    
    # EDA tabs
    tabItem(tabName = "LineChart", LineChartTab),
    tabItem(tabName = "RidgePlot", RidgePlotTab),
    tabItem(tabName = "Geofacet", GeofacetTab),
    tabItem(tabName = "geospatial", geospatialTab),
    
    # CA tabs
    tabItem(tabName = "CrossCorrelationFunction", CrossCorrelationFunctionTab),
    tabItem(tabName = "Cointegration", CointegrationTab),

    # Forecasting tabs
    tabItem(tabName = "decomposition", decompositionTab),
    tabItem(tabName = "arima", arimaTab),

    # About tab
    tabItem(tabName = "about", aboutTab)
  )
)

#========================================================== 
## Define UI and Server
#========================================================== 

# Define UI
# Using bslib for more customization based on brand.yml
ui <- dashboardPage(
  title = brand_settings$meta$name$full, 
  header = header,
  sidebar = sidebar,
  body = body
)

# Define server
server <- function(input, output) {
  date_min <- min(dataset$date, na.rm = TRUE)
  date_max <- max(dataset$date, na.rm = TRUE)
  # Configure highcharter theme based on visualization settings
  hc_theme_custom <- hc_theme(
    colors = c(primary_color, secondary_color, brand_colors$`medium-blue`, brand_colors$`dark-blue`),
    chart = list(
      backgroundColor = viz_settings$plots$background,
      borderRadius = component_settings$card$border_radius,
      style = list(
        fontFamily = brand_settings$typography$base$family
      )
    ),
    title = list(
      style = list(
        fontFamily = brand_settings$typography$headings$family,
        fontWeight = brand_settings$typography$headings$weight,
        color = brand_colors$`dark-grey`
      )
    ),
    xAxis = list(
      gridLineColor = viz_settings$plots$grid_lines,
      lineColor = viz_settings$plots$axis_color,
      labels = list(
        style = list(
          color = brand_colors$`dark-grey`,
          fontFamily = brand_settings$typography$data$family,
          fontSize = brand_settings$typography$data$size
        )
      )
    ),
    yAxis = list(
      gridLineColor = viz_settings$plots$grid_lines,
      lineColor = viz_settings$plots$axis_color,
      labels = list(
        style = list(
          color = brand_colors$`dark-grey`,
          fontFamily = brand_settings$typography$data$family,
          fontSize = brand_settings$typography$data$size
        )
      )
    ),
    legend = list(
      itemStyle = list(
        fontFamily = brand_settings$typography$labels$family,
        fontSize = brand_settings$typography$labels$size,
        fontWeight = brand_settings$typography$labels$weight
      )
    )
  )
  
  # Reactive data for LineChart analysis（改了）
  output$linechart <- renderHighchart({
    req(input$variable_ts, input$station_ts, input$date_range_ts)
    
    variable_column <- switch(input$variable_ts,
                              "Temperature" = "mean_temperature_c",
                              "Rainfall" = "daily_rainfall_total_mm",
                              "Wind Speed" = "mean_wind_speed_km_h")
    
    data_filtered <- dataset %>%
      filter(station %in% input$station_ts,
             date >= input$date_range_ts[1],
             date <= input$date_range_ts[2]) %>%
      select(date, station, value = all_of(variable_column)) %>%
      mutate(period = case_when(
        input$aggregation == "Weekly" ~ floor_date(date, "week"),
        input$aggregation == "Monthly" ~ floor_date(date, "month"),
        TRUE ~ date
      )) %>%
      group_by(station, period) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      rename(date = period) %>%
      mutate(date = as.Date(date))
    
    
    if (nrow(data_filtered) == 0) {
      showNotification("No data available for selected filters.", type = "error")
      return(NULL)
    }
    
    
    # Define monsoon bands only if checkbox is TRUE
    monsoon_bands <- NULL
    if (input$show_monsoon) {
      monsoon_bands <- list()
      years <- as.numeric(format(min(data_filtered$date), "%Y")):as.numeric(format(max(data_filtered$date), "%Y"))
      
      for (y in years) {
        monsoon_bands <- append(monsoon_bands, list(
          list(from = datetime_to_timestamp(as.Date(paste0(y - 1, "-12-01"))),
               to   = datetime_to_timestamp(as.Date(paste0(y, "-03-31"))),
               color = "rgba(135,206,250,0.2)"),  # NE Monsoon
          list(from = datetime_to_timestamp(as.Date(paste0(y, "-06-01"))),
               to   = datetime_to_timestamp(as.Date(paste0(y, "-09-30"))),
               color = "rgba(144,238,144,0.2)"),  # SW Monsoon
          list(from = datetime_to_timestamp(as.Date(paste0(y, "-04-01"))),
               to   = datetime_to_timestamp(as.Date(paste0(y, "-05-31"))),
               color = "rgba(255,228,181,0.2)"),  # Inter-monsoon I
          list(from = datetime_to_timestamp(as.Date(paste0(y, "-10-01"))),
               to   = datetime_to_timestamp(as.Date(paste0(y, "-11-30"))),
               color = "rgba(255,228,181,0.2)")   # Inter-monsoon II
        ))
      }
    }
    
    hc <- highchart() %>%
      hc_title(text = paste("Time Series of", input$variable_ts)) %>%
      hc_subtitle(
        text = "<span style='font-size:14px'>
           <span style='background-color:rgba(135,206,250,0.2); padding: 4px 8px; display: inline-block;'></span> NE Monsoon &nbsp;&nbsp;
           <span style='background-color:rgba(144,238,144,0.2); padding: 4px 8px; display: inline-block;'></span> SW Monsoon &nbsp;&nbsp;
           <span style='background-color:rgba(255,228,181,0.2); padding: 4px 8px; display: inline-block;'></span> Inter-monsoon
         </span>",
        useHTML = TRUE
      )%>%
      hc_xAxis(
        type = "datetime",
        title = list(text = "Date"),
        plotBands = monsoon_bands
      ) %>%
      hc_yAxis(title = list(text = input$variable_ts)) %>%
      hc_tooltip(shared = TRUE)
    
    for(st in unique(data_filtered$station)) {
      st_data <- data_filtered %>% filter(station == st)
      
      if (nrow(st_data) == 0) next
      
      hc <- hc %>%
        hc_add_series(
          data = list_parse2(data.frame(
            x = datetime_to_timestamp(st_data$date),
            y = st_data$value
          )),
          name = st,
          type = "line"
        )
    }
    
    hc %>% hc_add_theme(hc_theme_custom)
  })
  


  
  # Simple summary statistics
  output$summary_stats <- renderPrint({
    req(input$variable_ts, input$station_ts, input$date_range_ts)
    
    variable_column <- switch(input$variable_ts,
                              "Temperature" = "mean_temperature_c",
                              "Rainfall" = "daily_rainfall_total_mm",
                              "Wind Speed" = "mean_wind_speed_km_h")
    
    data_filtered <- dataset %>%
      filter(station %in% input$station_ts,
             date >= input$date_range_ts[1],
             date <= input$date_range_ts[2]) %>%
      pull(all_of(variable_column))
    
    cat("Summary statistics for", input$variable_ts, "\n\n")
    print(summary(data_filtered))
    cat("\nStandard Deviation:", round(sd(data_filtered, na.rm = TRUE), 2))
  })
  
  
  # Placeholder chart
  LineChart_data <- reactive({
    req(input$station_ts, input$variable_ts, input$date_range_ts)
    
    variable_column <- switch(input$variable_ts,
                              "Temperature" = "mean_temperature_c",
                              "Rainfall" = "daily_rainfall_total_mm",
                              "Wind Speed" = "mean_wind_speed_km_h")
    
    station %>%
      filter(station %in% input$station_ts,
             date >= input$date_range_ts[1],
             date <= input$date_range_ts[2]) %>%
      select(date, station, value = all_of(variable_column))
  })
  
  
  # Placeholder map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addMarkers(lng = 103.8198, lat = 1.3521, popup = "Singapore")
  })

  # Reactive data for RidgePlot analysis（改了）
  RidgePlot_data <- reactive({
    req(input$station_biv, input$var_biv, input$date_range_biv, input$aggregation_biv)
    
   
    region_mapping <- tibble(
      station = c("Changi", "Marina Barrage", "Ang Mo Kio", "Clementi",
                  "Jurong (West)", "Paya Lebar", "Newton", "Pasir Panjang",
                  "Tai Seng", "Admiralty"),
      region = c("East", "Central", "North-East", "West",
                 "West", "East", "Central", "Central",
                 "Central", "North"),
      region_code = c("ER", "CR", "NER", "WR",
                      "WR", "ER", "CR", "CR",
                      "CR", "NR")
    )
    

    dataset <- dataset %>%
      left_join(region_mapping, by = "station")
    
    
    variable_column <- switch(input$var_biv,
                              "Temperature" = "mean_temperature_c",
                              "Rainfall" = "daily_rainfall_total_mm",
                              "Wind Speed" = "mean_wind_speed_km_h")
    
 
    data_filtered <- dataset %>%
      filter(station %in% input$station_biv,
             date >= input$date_range_biv[1],
             date <= input$date_range_biv[2]) %>%
      select(date, station, value = all_of(variable_column)) %>%
      mutate(period = case_when(
        input$aggregation_biv == "Weekly" ~ floor_date(date, "week"),
        input$aggregation_biv == "Monthly" ~ floor_date(date, "month"),
        TRUE ~ date
      )) %>%
      group_by(station, period) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      rename(date = period)
    
    return(data_filtered)
  })
  
  
#import data to geofacet
  station_to_geo <- tibble(
    station = c("Changi", "Marina Barrage", "Ang Mo Kio", "Clementi",
                "Jurong (West)", "Paya Lebar", "Newton", "Pasir Panjang",
                "Tai Seng", "Admiralty"),
    lat = c(1.35735, 1.28062, 1.37011, 1.31511, 1.34039, 1.35917, 1.31141, 1.27642, 1.33554, 1.44066),
    lon = c(103.9874, 103.8718, 103.8490, 103.7650, 103.7054, 103.8944, 103.8369, 103.7919, 103.8885, 103.8000)
  )
  
  station_grid <- station_to_geo %>%
    mutate(
      code = station,
      name = station,
      row = c(4, 6, 2, 3, 2, 3, 4, 5, 4, 1),  
      col = c(5, 4, 3, 1, 1, 4, 3, 2, 4, 3)   
    ) %>%
    select(code, name, row, col)
  
  
  
  # Import data to Geofacet
  Geofacet_data <- reactive({
    req(input$station_multi, input$var_geofacet, input$date_range_multi, input$aggregation_geofacet)
    
    variable_column <- switch(input$var_geofacet,
                              "Temperature" = "mean_temperature_c",
                              "Rainfall" = "daily_rainfall_total_mm",
                              "Wind Speed" = "mean_wind_speed_km_h")
    
    dataset %>%
      filter(station %in% input$station_multi,
             date >= input$date_range_multi[1],
             date <= input$date_range_multi[2]) %>%
      mutate(date = case_when(
        input$aggregation_geofacet == "Weekly" ~ floor_date(date, "week"),
        input$aggregation_geofacet == "Monthly" ~ floor_date(date, "month"),
        TRUE ~ date
      )) %>%
      group_by(station, date) %>%
      summarise(value = mean(.data[[variable_column]], na.rm = TRUE), .groups = "drop") %>%
      drop_na(value)
  })
  
  
  output$geofacet_plot <- renderPlot({
    req(Geofacet_data())  
    df <- Geofacet_data()  
    
    ggplot(df, aes(x = date, y = value)) +
      geom_line(color = "#0072B2", size = 0.7) +
      facet_geo(~ station, grid = station_grid) +
      labs(title = paste("Weather Trends by Station -", input$var_geofacet),
           x = "Date", y = input$var_geofacet) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +   
      theme_minimal(base_size = 12) +
      theme(
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.title.x = element_text(margin = margin(t = 10)),  
        axis.text.y = element_text(size = 9)  
      )
    
  })
  
  
  
    
  
  
  
  
  
  # Density plot（改了）
  output$ridge_plot_biv <- renderPlot({
    req(RidgePlot_data())
    data <- RidgePlot_data()
    
    # Fill missing values using moving average
    data <- data %>%
      group_by(station) %>%
      arrange(date) %>%
      mutate(value = zoo::na.approx(value, na.rm = TRUE, maxgap = 30)) %>%
      drop_na(value)
    
    
    # Use ggridges to create ridgeline plot
    ggplot(data, aes(x = value, y = station, fill = station)) +
      ggridges::geom_density_ridges(
        scale = 2,
        alpha = 0.8,
        color = "white"
      ) +
      labs(
        title = paste("Ridgeline Plot of", input$var_biv, "by Station"),
        x = input$var_biv,
        y = "Station"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 16),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = margin(t = 10, r = 10, b = 40, l = 10)  
      )
  })
  
  
  # Summary statistics by station
  output$station_stats_biv <- renderPrint({
    req(RidgePlot_data())
    data <- RidgePlot_data()
    
    # Calculate statistics for each station
    cat(paste("Statistics for", input$var_biv, "by Station\n\n"))
    
    for(station in unique(data$station)) {
      station_data <- data$value[data$station == station]
      
      cat("\n=== ", station, " ===\n")
      cat("Number of observations:", length(station_data), "\n")
      cat("Mean:", round(mean(station_data, na.rm = TRUE), 2), "\n")
      cat("Standard Deviation:", round(sd(station_data, na.rm = TRUE), 2), "\n")
      cat("Minimum:", round(min(station_data, na.rm = TRUE), 2), "\n")
      cat("Maximum:", round(max(station_data, na.rm = TRUE), 2), "\n")
      cat("Quartiles:\n")
      print(round(quantile(station_data, probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 2))
      cat("\n")
    }
  })

  
  # Reactive data for Geofacet analysis（改了）
  output$station_line_plot <- renderPlot({
    req(input$station_single, input$var_single, input$date_range_single)
    
    variable_column <- switch(input$var_single,
                              "Temperature" = "mean_temperature_c",
                              "Rainfall" = "daily_rainfall_total_mm",
                              "Wind Speed" = "mean_wind_speed_km_h")
    
    df <- dataset %>%
      filter(station %in% input$station_single,
             date >= input$date_range_single[1],
             date <= input$date_range_single[2]) %>%
      select(date, station, value = all_of(variable_column)) %>%
      mutate(date = case_when(
        input$aggregation_single == "Weekly" ~ floor_date(date, "week"),
        input$aggregation_single == "Monthly" ~ floor_date(date, "month"),
        TRUE ~ date
      )) %>%
      group_by(station, date) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    
    ggplot(df, aes(x = date, y = value, color = station)) +
      geom_line() +
      labs(title = paste("Time Series by Station:", input$var_single),
           x = "Date", y = input$var_single) +
      theme_minimal(base_size = 13)
  })}
  



    



# Run the application 
shinyApp(ui = ui, server = server)
