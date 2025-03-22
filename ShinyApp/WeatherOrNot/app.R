# Load necessary libraries
pacman::p_load(shiny, shinydashboard, highcharter, leaflet, bslib, yaml, bsicons)


# Import Data
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
    height: 80px;
    line-height: 80px;
  }
  .skin-blue .main-header .navbar { 
    background-color: ', background_color, ' !important; 
    border-bottom: 1px solid ', brand_colors$`light-grey`, ';
    min-height: 80px;
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
    height: 80px !important;
    position: fixed;
    width: 100%;
    z-index: 1000;
  }

  .main-header .navbar {
    height: 80px !important;
    margin-left: 150px;  /* Match titleWidth */
  }

  .main-header .logo {
    height: 80px !important;
    line-height: 80px !important;
    position: fixed;
    width: 150px;  /* Match titleWidth */
    z-index: 1001;
    display: flex;
    align-items: center;
  }

  .main-header .logo img {
    height: 80px;
    width: auto;
  }

  /* Adjust content area */
  .content-wrapper {
    padding-top: 80px !important;
    min-height: calc(100vh - 80px) !important;
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
    height: 80px;
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

# EDA Components
LineChartTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "Data Selection", status = "info",
        selectInput("variable_ts", "Select Variable:", 
                    choices = c("Temperature", "Rainfall", "Wind Speed"),
                    selected = "Temperature"),
        selectInput("station_ts", "Select Stations:", 
                    choices = c("Changi", "Marina_Barrage", "Ang_Mo_Kio", "Clementi", 
                                "Jurong_West", "Paya_Lebar", "Newton", "Pasir_Panjang", 
                                "Tai_Seng", "Admiralty"),
                    multiple = TRUE,
                    selected = "Changi"),
        selectInput("aggregation", "Time Aggregation:",
                    choices = c("Daily", "Weekly", "Monthly"),
                    selected = "Daily"),
        dateRangeInput("date_range_ts", "Date Range:",
                       start = "2020-01-01", 
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
        checkboxInput("show_trend_ts", "Show Trend", value = TRUE),
        checkboxInput("show_seasonal", "Show Seasonality", value = TRUE),
        checkboxInput("show_outliers", "Show Outliers", value = TRUE)
    )
  ),
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
                    choices = c("Changi", "Marina_Barrage", "Ang_Mo_Kio", "Clementi", 
                              "Jurong_West", "Paya_Lebar", "Newton", "Pasir_Panjang", 
                              "Tai_Seng", "Admiralty"),
                    multiple = TRUE,
                    selected = c("Changi", "Marina_Barrage")),
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
          highchartOutput("density_plot_biv", height = "400px")
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
)

# Geofacet Analysis Tab
GeofacetTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "Data Selection", status = "info",
      selectInput("x_scatter_variable", "Select X-Axis Variable:", 
                    choices = c("Temperature", "Rainfall", "Wind Speed"),
                    selected = c("Temperature")),
      selectInput("y_scatter_variable", "Select Y-Axis Variable:", 
                    choices = c("Temperature", "Rainfall", "Wind Speed"),
                    selected = c("Rainfall")),
      selectInput("station_multi", "Select Stations:", 
                    choices = c("Changi", "Marina_Barrage", "Ang_Mo_Kio", "Clementi", 
                              "Jurong_West", "Paya_Lebar", "Newton", "Pasir_Panjang", 
                              "Tai_Seng", "Admiralty"),
                    multiple = TRUE,
                    selected = c("Changi", "Marina_Barrage")),
      selectInput("color_by", "Color By:",
                 choices = c("Station" = "station", 
                           "Region" = "region",
                           "None" = "NULL"),
                 selected = "NULL"),
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
  # Right column - Visualizations and Statistics
  column(width = 9,
    tabBox(width = 12,
      title = "Geofacet Analysis",
      id = "Geofacet_tabs",
      tabPanel("Scatter Plot",
        fluidRow(
          column(width = 12,
            highchartOutput("scatter_plot", height = "400px")
          )
        ),
        fluidRow(
          column(width = 12,
            div(style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 4px;",
              h4("Scatter Plot Statistics"),
              verbatimTextOutput("scatter_stats")
            )
          )
        )
      ),
      tabPanel("Correlation Matrix",
        fluidRow(
          column(width = 12,
            highchartOutput("correlation_matrix", height = "400px")
          )
        ),
        fluidRow(
          column(width = 12,
            div(style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 4px;",
              h4("Correlation Statistics"),
              verbatimTextOutput("correlation_stats")
            )
          )
        )
      )
    )
  )
)


# Geospatial Analysis Tab
geospatialTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "Map Settings", status = "info",
      selectInput("variable_map", "Select Variable:", 
                    choices = c("Temperature", "Rainfall", "Wind Speed"),
                    selected = "Temperature"),
      selectInput("time_period", "Time Period:",
                 choices = c("Daily", "Monthly", "Yearly"),
                 selected = "Monthly"),
      selectInput("station_map", "Select Stations:", 
                    choices = c("Changi", "Marina_Barrage", "Ang_Mo_Kio", "Clementi", 
                              "Jurong_West", "Paya_Lebar", "Newton", "Pasir_Panjang", 
                              "Tai_Seng", "Admiralty"),
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
  # Right column - Visualizations
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
  
  # Reactive data for LineChart analysis
  LineChart_data <- reactive({
    req(input$station, input$variable, input$date_range)
    # Placeholder for data retrieval
    # This should be replaced with actual data retrieval logic
    data.frame(
      value = rnorm(1000),
      station = sample(input$station, 1000, replace = TRUE)
    )
  })
  
  # Simple histogram
  output$histogram <- renderHighchart({
    req(LineChart_data())
    data <- LineChart_data()
    
    hc <- highchart() %>%
      hc_title(text = paste("Distribution of", input$variable)) %>%
      hc_xAxis(title = list(text = input$variable)) %>%
      hc_yAxis(title = list(text = "Frequency")) %>%
      hc_add_series(
        data = data$value,
        type = "histogram",
        name = "Frequency",
        color = primary_color
      ) %>%
      hc_add_theme(hc_theme_custom)
    
    hc
  })
  
  # Simple summary statistics
  output$summary_stats <- renderPrint({
    req(LineChart_data())
    data <- LineChart_data()
    
    # Calculate basic statistics
    stats <- summary(data$value)
    cat("Basic Statistics:\n\n")
    print(stats)
    
    cat("\nStandard Deviation:", round(sd(data$value), 2))
  })
  
  # Placeholder chart
  output$line_chart <- renderHighchart({
    highchart() %>% 
      hc_title(text = "Sample Line Chart") %>%
      hc_add_theme(hc_theme_custom)
  })
  
  # Placeholder map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addMarkers(lng = 103.8198, lat = 1.3521, popup = "Singapore")
  })

  # Reactive data for RidgePlot analysis
  RidgePlot_data <- reactive({
    req(input$station_biv, input$var_biv, input$date_range_biv)
    # Placeholder for data retrieval
    # This should be replaced with actual data retrieval logic
    data.frame(
      value = c(rnorm(500, mean = 25, sd = 2), rnorm(500, mean = 26, sd = 2.5)),
      station = rep(input$station_biv, each = 500)
    )
  })
  
  # Density plot
  output$density_plot_biv <- renderHighchart({
    req(RidgePlot_data())
    data <- RidgePlot_data()
    
    # Create density estimates for each station
    densities <- lapply(unique(data$station), function(s) {
      d <- density(data$value[data$station == s])
      data.frame(x = d$x, y = d$y, station = s)
    })
    
    # Combine all densities
    all_densities <- do.call(rbind, densities)
    
    # Create the plot
    hc <- highchart() %>%
      hc_title(text = paste("Distribution of", input$var_biv, "by Station")) %>%
      hc_xAxis(title = list(text = input$var_biv)) %>%
      hc_yAxis(title = list(text = "Density")) %>%
      hc_tooltip(shared = TRUE)
    
    # Add a line for each station
    for(station in unique(all_densities$station)) {
      station_data <- subset(all_densities, station == station)
      hc <- hc %>%
        hc_add_series(
          data = list_parse2(data.frame(x = station_data$x, y = station_data$y)),
          name = station,
          type = "line"
        )
    }
    
    hc %>% hc_add_theme(hc_theme_custom)
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
      cat("Mean:", round(mean(station_data), 2), "\n")
      cat("Standard Deviation:", round(sd(station_data), 2), "\n")
      cat("Minimum:", round(min(station_data), 2), "\n")
      cat("Maximum:", round(max(station_data), 2), "\n")
      cat("Quartiles:\n")
      print(round(quantile(station_data, probs = c(0.25, 0.5, 0.75)), 2))
      cat("\n")
    }
  })

  # Reactive data for Geofacet analysis
  Geofacet_data <- reactive({
    req(input$variables, input$station_multi, input$date_range_multi)
    # Placeholder for data retrieval
    # This should be replaced with actual data retrieval logic
    data <- data.frame(
      Temperature = rnorm(1000, mean = 25, sd = 2),
      Rainfall = rnorm(1000, mean = 100, sd = 30),
      Wind_Speed = rnorm(1000, mean = 15, sd = 5),
      station = sample(input$station_multi, 1000, replace = TRUE),
      region = sample(c("North", "South", "East", "West", "Central"), 1000, replace = TRUE)
    )
    return(data)
  })
  
  # Scatter plot
  output$scatter_plot <- renderHighchart({
    req(Geofacet_data())
    data <- Geofacet_data()
    
    if(ncol(data) < 2) {
      return(NULL)
    }
    
    var1 <- input$variables[1]
    var2 <- input$variables[2]
    
    hc <- highchart() %>%
      hc_title(text = paste("Scatter Plot:", var1, "vs", var2)) %>%
      hc_xAxis(title = list(text = var1)) %>%
      hc_yAxis(title = list(text = var2)) %>%
      hc_tooltip(pointFormat = paste(
        "{point.x}", var1, "<br>",
        "{point.y}", var2, "<br>",
        if(input$color_by != "NULL") paste("{point.", input$color_by, "}", sep="")
      ))
    
    if(input$color_by == "NULL") {
      # Single color for all points
      hc <- hc %>%
        hc_add_series(
          data = list_parse2(data.frame(x = data[[var1]], y = data[[var2]])),
          type = "scatter",
          marker = list(symbol = "circle", radius = 4),
          name = "Observations",
          color = primary_color
        )
    } else {
      # Color by group
      groups <- unique(data[[input$color_by]])
      for(group in groups) {
        group_data <- data[data[[input$color_by]] == group, ]
        hc <- hc %>%
          hc_add_series(
            data = list_parse2(data.frame(
              x = group_data[[var1]], 
              y = group_data[[var2]],
              name = group
            )),
            type = "scatter",
            marker = list(symbol = "circle", radius = 4),
            name = group
          )
      }
    }
    
    if(input$show_trend_multi) {
      fit <- lm(data[[var2]] ~ data[[var1]])
      pred_x <- seq(min(data[[var1]]), max(data[[var1]]), length.out = 100)
      pred_y <- predict(fit, newdata = data.frame(x = pred_x))
      
      hc <- hc %>%
        hc_add_series(
          data = list_parse2(data.frame(x = pred_x, y = pred_y)),
          type = "line",
          name = "Trend Line",
          color = secondary_color,
          dashStyle = "Dash"
        )
    }
    
    hc %>% 
      hc_add_theme(hc_theme_custom) %>%
      hc_legend(
        align = "right",
        verticalAlign = "top",
        layout = "vertical"
      )
  })
  
  # Correlation matrix
  output$correlation_matrix <- renderHighchart({
    req(Geofacet_data())
    data <- Geofacet_data()
    
    if(ncol(data) < 2) {
      return(NULL)
    }
    
    cor_matrix <- cor(data)
    
    hc <- highchart() %>%
      hc_title(text = "Correlation Matrix") %>%
      hc_chart(type = "heatmap") %>%
      hc_xAxis(categories = names(data)) %>%
      hc_yAxis(categories = names(data)) %>%
      hc_add_series(
        data = map(1:nrow(cor_matrix), function(i) {
          map(1:ncol(cor_matrix), function(j) {
            list(x = j-1, y = i-1, value = cor_matrix[i,j])
          })
        }) %>% unlist(recursive = FALSE),
        dataLabels = list(enabled = TRUE)
      ) %>%
      hc_colorAxis(
        stops = list(
          list(0, "#FF0000"),
          list(0.5, "#FFFFFF"),
          list(1, "#0000FF")
        ),
        min = -1,
        max = 1
      ) %>%
      hc_add_theme(hc_theme_custom)
    
    hc
  })
  
  # Scatter plot statistics
  output$scatter_stats <- renderPrint({
    req(Geofacet_data())
    data <- Geofacet_data()
    
    if(ncol(data) < 2) {
      cat("Please select at least two variables for analysis.\n")
      return(NULL)
    }
    
    var1 <- names(data)[1]
    var2 <- names(data)[2]
    
    # Calculate correlation
    cor_test <- cor.test(data[[1]], data[[2]])
    
    cat("=== Relationship Analysis ===\n\n")
    cat("Variables:", var1, "vs", var2, "\n")
    cat("\nCorrelation coefficient (r):", round(cor_test$estimate, 3))
    cat("\np-value:", format.pval(cor_test$p.value, digits = 3))
    cat("\n95% Confidence Interval:", 
        paste0("[", paste(round(cor_test$conf.int, 3), collapse = ", "), "]"))
    
    # Linear regression summary
    fit <- lm(data[[2]] ~ data[[1]])
    cat("\n\n=== Linear Regression ===\n")
    cat("\nIntercept:", round(coef(fit)[1], 3))
    cat("\nSlope:", round(coef(fit)[2], 3))
    cat("\nR-squared:", round(summary(fit)$r.squared, 3))
  })
  
  # Correlation matrix statistics
  output$correlation_stats <- renderPrint({
    req(Geofacet_data())
    data <- Geofacet_data()
    
    if(ncol(data) < 2) {
      cat("Please select at least two variables for analysis.\n")
      return(NULL)
    }
    
    # Calculate correlation matrix
    cor_matrix <- cor(data)
    
    cat("=== Correlation Matrix Analysis ===\n\n")
    
    # Print correlations with significance tests
    for(i in 1:(ncol(data)-1)) {
      for(j in (i+1):ncol(data)) {
        cor_test <- cor.test(data[[i]], data[[j]])
        cat(names(data)[i], "vs", names(data)[j], ":\n")
        cat("  Correlation:", round(cor_test$estimate, 3), "\n")
        cat("  p-value:", format.pval(cor_test$p.value, digits = 3), "\n")
        cat("  95% CI:", paste0("[", 
            paste(round(cor_test$conf.int, 3), collapse = ", "), "]\n"))
        cat("\n")
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)