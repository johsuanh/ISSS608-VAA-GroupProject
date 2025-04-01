# Load necessary libraries
library(gstat)
library(shiny)
library(shinydashboard)
library(highcharter)
library(leaflet)
library(bslib)
library(yaml)
library(bsicons)
library(tidyverse)
library(lubridate)
library(zoo)
library(tidyr)
library(ggplot2)
library(ggridges)
library(tibble)
library(geofacet)
library(terra)
library(sf)
library(tmap)

pacman::p_load(DT,tseries,knitr,tsibble,fable,feasts,ggthemes,forecast,kableExtra,shinybusy)

# Load spatial data
weekly_station_sf <- readRDS("www/station_weekly_sf.rds")
daily_station_sf <- readRDS("www/station_daily_sf.rds")
monthly_station_sf <- readRDS("www/station_monthly_sf.rds")
mpsz <- readRDS("www/mpsz.rds")
daily_station <- readRDS("www/station_daily_data.rds")
monthly_station <- readRDS("www/station_daily_data.rds")

# Print the structure of daily_station to verify its contents
print(str(daily_station))


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
    background-color: #f5f5f5 !important;
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
  }  .main-header .logo img {
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

  /* Box header styling for sidebar */
  .col-sm-3 .box-header {
    display: flex;
    align-items: center;
    justify-content: center;
    border: none !important;
    border-radius: 5px !important;
    background-color: ', background_color, ' !important;
    padding: 15px !important;
    text-align: center !important;
    height: 20px;
  }

  /* Box header styling for visualization area */
  .col-sm-9 .box-header {
    display: flex;
    align-items: center;
    justify-content: center;
    border: none !important;
    border-bottom: 1px solid ', brand_colors$`light-grey`, ' !important;
    border-radius: 5px 5px 0 0 !important;
    background-color: white !important;
    padding: 15px !important;
    text-align: center !important;
    height: 20px;
  }

  .col-sm-3 .box-header h3 {
    color: ', brand_colors$`dark-grey`, ' !important;
    font-family: "', brand_settings$typography$headings$family, '", sans-serif;
    font-weight: ', brand_settings$typography$headings$weight, ';
    font-size: 11px !important;
    text-align: center !important;
  }

  .col-sm-9 .box-header h3 {
    color: ', brand_colors$`sky-blue`, ' !important;
    font-family: "', brand_settings$typography$headings$family, '", sans-serif;
    font-weight: ', brand_settings$typography$headings$weight, ';
    font-size: 11px !important;
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

  .selectize-dropdown {
      max-height: 100px;  /* Set a max height for the dropdown */
      overflow-y: auto;   /* Enable vertical scrolling */
  }
  
  /* Slider styling based on component settings */
  .irs-bar, .irs-bar-edge {
    background: "#8AA4FF"  !important;
    border-color: "#8AA4FF" !important;
  }
  .irs-line {
    background: "#8AA4FF" !important;
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
    font-size: 12px !important;
    padding: 4px 8px !important;
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
    font-size: 10px !important;
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
    border-radius: 5px !important;
  }


.daterangepicker input {
      font-size: 10px;
    }
  /* Statistics container rounder corners */
  .statistics-container,
  .stat-box {
    border-radius: 5px !important;
  }



')

# --- Station-Region label choices for UI display (RidgePlot module only) ---
region_mapping_ui <- tibble(
  station = c("Changi",  "Ang Mo Kio", "Clementi",
              "Jurong (West)", "Paya Lebar", "Newton", "Pasir Panjang",
              "Tai Seng", "Admiralty"),
  region = c("East",  "North-East", "West",
             "West", "East", "Central", "Central",
             "Central", "North")
)

region_label_choices <- setNames(
  region_mapping_ui$station,
  paste0(region_mapping_ui$station, " (", region_mapping_ui$region, ")")
)

daily_station <- daily_station %>%
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
      menuSubItem("Line Chart", tabName = "LineChart"),
      menuSubItem("Ridge Plot", tabName = "RidgePlot"),
      menuSubItem("Geofacet", tabName = "Geofacet"),
      menuSubItem("Isohyet Map", tabName = "Isohyet")
    ),
    
    # Time Series Analysis Section
    menuItem("Time-Series Analysis", tabName = "Time", icon = icon("check-circle"),
      menuSubItem("Time-Series Decomposition", tabName = "Decomposition"),
      menuSubItem("Correlograms", tabName = "Correlogram")
      ),
    
    # Forecasting Section
    menuItem("Univariate Forecasting", tabName = "Forecasting", icon = icon("chart-line"),
      menuSubItem("Step 1: Model Training", tabName = "Training"),
      menuSubItem("Step 2: Model Forecasting", tabName = "Model")
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
        tags$li(tags$strong("Forecasting:"), "Predict future weather patterns using various time series models")
      )
  )
)

# EDA Components
LineChartTab <- fluidRow(
  column(width = 3,
         box(width = 12, title = "Data Selection", status = "info",
             selectInput("variable_ts", "Select Variable:", 
                         choices = c("Mean Temperature", "Min Temperature", "Max Temperature", 
                                     "Rainfall", "Mean Wind Speed", "Max Wind Speed"),
                         selected = "Mean Temperature"),
             selectInput("station_ts", "Select Stations:", 
                         choices = setNames(daily_station$station, paste0(daily_station$station, " (", daily_station$region, ")")),
                         multiple = TRUE,
                         selected = "Changi"),
             selectInput("aggregation", "Time Resolution:",
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
                          h4("Summary Statistics", style = "text-align: center;"),
                          div(style = "display: flex; justify-content: center;",
                              tableOutput("summary_stats_table")
                          )
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



# RidgePlot Analysis Tab
RidgePlotTab <- fluidRow(
  column(width = 12,
    tabBox(
      width = 12,
      id = "ridge_tabs",
      title = "RidgePlot Analysis",
      
      # Tab 1: Plot
      tabPanel("Ridgeline Plot",
        fluidRow(
          column(width = 3,
            box(width = 12, title = "Data Selection", status = "info",
              selectInput("var_biv", "Select Variable:", 
                          choices = c("Mean Temperature", "Min Temperature", "Max Temperature", 
                                      "Rainfall", "Mean Wind Speed", "Max Wind Speed"),
                          selected = "Mean Temperature"),
              selectInput("station_biv", "Select Stations:", 
                          choices = setNames(daily_station$station, paste0(daily_station$station, " (", daily_station$region, ")")),
                          multiple = TRUE,
                          selected = c("Changi", "Newton", "Clementi", "Jurong (West)")),
              
              tags$script(HTML("
  $(document).on('shiny:connected', function() {
    var maxItems = 4;
    var selectize = $('#station_biv')[0].selectize;

    selectize.on('change', function() {
      if (selectize.items.length > maxItems) {
        selectize.removeItem(selectize.items[maxItems]);
        alert('You can select up to 4 stations only!');
      }
    });
  });
")),
              selectInput("aggregation_biv", "Time Resolution:",
                          choices = c("Monthly"),
                          selected = "Monthly"),
              dateRangeInput("date_range_biv", "Date Range:",
                            start = "2019-01-01", 
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
          column(width = 9,
            box(width = 12,
              plotOutput("ridge_plot_biv", height = "500px"),
              div(style = "margin-top: 15px;",
                  h5("Station Colors"),
                  uiOutput("color_legend_ui")
            )
          )
        )
      )),
      
      # Tab 2: Summary
      tabPanel("Summary Statistics",
        fluidRow(
          column(width = 12,
            box(width = 12,
                h4("Summary Statistics by Station", 
                   style = "text-align: center; font-weight: bold; font-size: 20px;"),
                div(style = "display: flex; justify-content: center;",
                tableOutput("station_summary_table")
              )
            )
          )
        )
      )
    )
  )
)

# Geofacet tab:
GeofacetTab <- fluidRow(
  column(width = 12,
    tabBox(
      width = 12,
      id = "geofacet_tabs",
      title = "Geofacet Analysis",

      # Tab 1: All stations shown by region 
      tabPanel("All Stations",
        fluidRow(
          column(width = 3,
            box(
              width = 12,
              title = "Data Selection",
              status = "info",
              selectInput(
                "station_multi", "Select Stations:", 
                choices = setNames(daily_station$station, paste0(daily_station$station, " (", daily_station$region, ")")),
                multiple = TRUE,
                selected = unique(daily_station$station)
              ),
              selectInput(
                "var_geofacet", "Select Variable:",
                choices = c("Mean Temperature", "Min Temperature", "Max Temperature", 
                            "Rainfall", "Mean Wind Speed", "Max Wind Speed"),
                selected = "Mean Temperature"
              ),
              selectInput(
                "aggregation_geofacet", "Time Resolution:",
                choices = c("Daily", "Weekly", "Monthly"),
                selected = "Daily"
              ),
              dateRangeInput(
                "date_range_multi", "Date Range:",
                start = "2019-01-01", 
                end = "2025-01-31",
                separator = " - ",
                format = "yyyy-mm-dd"
              ),
              div(
                style = "text-align: right;",
                actionButton(
                  "update_Geofacet", "Update View", 
                  class = "btn-primary",
                  icon = icon("refresh")
                )
              )
            ),
            box(
              width = 12,
              title = "Display Options",
              status = "info",
              checkboxInput("show_trend_multi", "Show Trend Lines", value = TRUE),
              checkboxInput("show_labels", "Show Labels", value = TRUE)
            )
          ),
          column(width = 9,
            box(
              width = 12,
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
            box(
              width = 12,
              title = "Station Selection",
              status = "info",
              selectInput(
                "station_single", "Select Stations:",
                choices = unique(daily_station$station),
                multiple = TRUE,
                selected = "Changi"
              ),
              selectInput(
                "var_single", "Select Variable:",
                choices = c("Mean Temperature", "Min Temperature", "Max Temperature", 
                            "Rainfall", "Mean Wind Speed", "Max Wind Speed"),
                selected = "Mean Temperature"
              ),
              dateRangeInput(
                "date_range_single", "Date Range:",
                start = "2019-01-01", 
                end = "2025-01-31"
              ),
              selectInput(
                "aggregation_single", "Time Resolution:",
                choices = c("Daily", "Weekly", "Monthly"),
                selected = "Daily"
              ),
              div(
                style = "text-align: right;",
                actionButton(
                  "update_single", "Update View", 
                  class = "btn-primary",
                  icon = icon("refresh")
                )
              )
            )
          ),
          column(width = 9,
            box(
              width = 12,
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




## Isohyet Map
IsohyetmapTab <- fluidRow(
  # Left sidebar with settings
  column(width = 3,
    # Collapsible Info Box
    box(width = 12,
      title = "About Spatial Interpolation",
      status = "info",
      height = "auto",
      collapsible = TRUE,
      collapsed = TRUE,
      p("Spatial interpolation helps us estimate values at unsampled locations based on known measurements from nearby weather stations."),
      tags$ul(
        tags$li(strong("IDW (Inverse Distance Weighting):"), "A deterministic method that assumes closer points have more influence."),
        tags$li(strong("Kriging:"), "A geostatistical method that considers both distance and spatial patterns."),
        tags$li(strong("Variogram:"), "Helps understand spatial correlation and optimize Kriging parameters.")
      ),
      p("Use the settings below to customize your analysis.")
    ),
    
    # Step 1: Map Settings
    box(width = 12, 
      title = "Step 1: Map Settings", 
      status = "info",
      height = "260px",
      selectInput("variable_map", 
                 "Select Variable:", 
                 choices = c("Mean Temperature", "Min Temperature", "Max Temperature", 
                           "Rainfall", "Mean Wind Speed", "Max Wind Speed"),
                 selected = "Mean Temperature"),
      selectInput("time_resolution", 
                 "Time Resolution:",
                 choices = c("Daily", "Weekly", "Monthly"),
                 selected = "Monthly"),
      dateRangeInput("date_range_map", 
                    "Date Range:",
                    start = "2023-01-01",
                    end = "2024-02-01",
                    format = "yyyy-mm-dd")
    ),
    
    # Step 2: IDW Settings
    box(width = 12,
      title = "Step 2: IDW Settings",
      status = "info",
      height = "140px",
      sliderInput("idw_nmax", 
                  "IDW Nmax (groups):",
                  value = 6,
                  min = 1,
                  max = 20,
                  step = 1)
    ),
    
    # Step 3: Variogram Settings
    box(width = 12,
      title = "Step 3: Variogram Settings",
      status = "info",
      height = "190px",
      selectInput("variogram_model",
                 "Select Model:",
                 choices = c("Gau", "Mat", "Per"),
                 selected = "Gau")
    ),
    
    # Run Model Button
    box(width = 12,
      status = "primary",
      height = "80px",
      div(style = "text-align: center; padding-top: 20px;",
        actionButton("run_map", 
                    "Run Model", 
                    class = "btn-primary",
                    icon = icon("play"))
      )
    )
  ),
  
  # Main content area with 4 boxes in a grid
  column(width = 9,
    fluidRow(
      # Step 1: Variable Selection Map
      box(width = 6,
        title = "Step 1: Select a Variable of Interest :)",
        status = "primary",
        solidHeader = TRUE,
        height = "350px",
        tmapOutput("station_map", height = "300px")
      ),
      
      # Step 2: IDW Results
      box(width = 6,
        title = "Step 2: IDW - Determine the Max No. of Groups for Spatial Interpolation",
        status = "primary",
        solidHeader = TRUE,
        height = "350px",
        tmapOutput("idw_map", height = "300px")
      )
    ),
    fluidRow(
      # Step 3: Variogram Plot
      box(width = 6,
        title = "Step 3: Set Up Variogram for Ordinary Kriging Interpolation",
        status = "primary",
        solidHeader = TRUE,
        height = "350px",
        plotOutput("variogram_plot", height = "300px")
      ),
      
      # Step 4: Kriging Results
      box(width = 6,
        title = "Step 4: Ordinary Kriging - Comparing Results with IDW",
        status = "primary",
        solidHeader = TRUE,
        height = "350px",
        tmapOutput("kriging_map", height = "300px")
      )
    )
  )
)


## Decomposition Tab
DecompositionTab <- fluidRow(
  # Left sidebar with settings
  column(width = 3,
    # Collapsible Info Box
    box(width = 12,
      title = "About STL Decomposition",
      status = "info",
      height = "auto",
      collapsible = TRUE,
      collapsed = TRUE,
      p("STL (Seasonal-Trend decomposition using LOESS) decomposes a time series into three components:"),
      tags$ul(
        tags$li(strong("Trend:"), "The long-term progression of the series"),
        tags$li(strong("Seasonal:"), "Repeating short-term cycles"),
        tags$li(strong("Remainder:"), "The residuals after removing trend and seasonal components")
      ),
      p("Use the settings below to customize your analysis.")
    ),
    
    # Settings
    box(width = 12, 
      title = "Settings", 
      status = "info",
      height = "380px",
      selectInput("variable_decomp", 
                 "Select Variable:", 
                 choices = c("Mean Temperature", "Min Temperature", "Max Temperature",
                           "Rainfall", "Mean Wind Speed", "Max Wind Speed"),
                 selected = "Mean Temperature"),
      selectInput("station_decomp", 
                 "Select Station:", 
                 choices = unique(daily_station$station),
                 selected = "Clementi"),                 
      selectInput("time_resolution_decomp", 
                 "Time Resolution:",
                 choices = c("Daily", "Weekly", "Monthly"),
                 selected = "Monthly"),
      dateRangeInput("date_range_decomp", 
                    "Date Range:",
                    start = "2019-01-01",
                    end = "2025-01-31",
                    format = "yyyy-mm-dd")
    ),
    
    # Run Model Button
    box(width = 12,
      status = "primary",
      height = "100px",
      div(style = "text-align: center; padding-top: 20px;",
        actionButton("run_decomp", 
                    "Run Model", 
                    class = "btn-primary",
                    icon = icon("play"))
      )
    )
  ),
  
  # Main content area with plots
  column(width = 9,
    fluidRow(
      # STL Decomposition Plot
      box(width = 12,
        title = "Step 1: Break Down the Time Series into Trend, Seasonal and Remainder",
        status = "primary",
        solidHeader = TRUE,
        height = "350px",
        plotOutput("stl_plot", height = "300px")
      ),
      
      # Seasonal Plot
      box(width = 12,
        title = "Step 2: Seasonal Decomposition by Month",
        status = "primary", 
        solidHeader = TRUE,
        height = "350px",
        plotOutput("seasonal_plot", height = "300px")
      )
    )
  )
)



# Time Series Correlogram Tab
CorrelogramTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    # Collapsible Info Box
    box(width = 12,
      title = "About Correlogram Analysis",
      status = "info",
      height = "auto",
      collapsible = TRUE,
      collapsed = TRUE,
      p("Correlogram analysis helps us understand the temporal dependencies in time series data through:"),
      tags$ul(
        tags$li(strong("Autocorrelation (ACF):"), "Shows correlation between observations at different time lags"),
        tags$li(strong("Partial Autocorrelation (PACF):"), "Shows correlation between observations after removing the effects of intermediate lags"),
        tags$li(strong("Trend Differencing:"), "Helps identify and remove trend components"),
        tags$li(strong("Seasonal Differencing:"), "Helps identify and remove seasonal patterns")
      ),
      p("Use the settings below to customize your analysis.")
    ),
    
    # Settings
    box(width = 12, 
      title = "Settings", 
      status = "info",
      height = "auto",
      selectInput("variable_corr", 
                 "Select Variable:", 
                 choices = c("Mean Temperature", "Min Temperature", "Max Temperature",
                           "Rainfall", "Mean Wind Speed", "Max Wind Speed"),
                 selected = "Mean Temperature"),
      selectInput("station_corr", 
                 "Select Station:", 
                 choices = unique(daily_station$station),
                 selected = "Clementi"),
      selectInput("time_resolution_corr", 
                 "Time Resolution:",
                 choices = c("Daily", "Weekly", "Monthly"),
                 selected = "Monthly"),
      dateRangeInput("date_range_corr", 
                    "Date Range:",
                    start = "2019-01-01",
                    end = "2025-01-31",
                    format = "yyyy-mm-dd"),
      selectInput("plot_type", 
                 "Plot Type:",
                 choices = c("partial", "scatter", "histogram"),
                 selected = "partial"),      
      sliderInput("lag_max", 
                  "Maximum Lag:",
                  min = 1, 
                  max = 52, 
                  value = 24, 
                  step = 1)
    ),
    
    # Run Model Button
    box(width = 12,
      status = "primary",
      height = "100px",
      div(style = "text-align: center; padding-top: 20px;",
        actionButton("run_corr", 
                    "Run Analysis", 
                    class = "btn-primary",
                    icon = icon("chart-line"))
      )
    )
  ),
  
  # Main content area with plots
  column(width = 9,
    # Step 1: Original Series
    box(width = 12,
      title = "Step 1: Check Autocorrelation with Different Plots",
      status = "primary",
      solidHeader = TRUE,
      height = "300px",
      plotOutput("original_plot", height = "300px")
    ),
    
    # Step 2 and 3 side by side
    fluidRow(
      # Step 2: Trend Differencing
      column(width = 6,
        box(width = 12,
          title = "Step 2: Trend Differencing",
          status = "primary",
          solidHeader = TRUE,
          height = "300px",
          plotOutput("trend_diff_plot", height = "280px")
        )
      ),
      
      # Step 3: Seasonal Differencing
      column(width = 6,
        box(width = 12,
          title = "Step 3: Seasonal Differencing",
          status = "primary",
          solidHeader = TRUE,
          height = "300px",
          plotOutput("seasonal_diff_plot", height = "280px")
        )
      )
    )
  )
)

# Training Models Tab
TrainingTab <- fluidRow(
  add_busy_spinner(spin = "fading-circle"),
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "Step 1: Training Settings", status = "info",
      selectInput("variable_train", "Select Variable:", 
                    choices = c("Mean Temperature", "Min Temperature", "Max Temperature",
                              "Rainfall", "Mean Wind Speed", "Max Wind Speed"),
                    selected = "Mean Temperature"),
      selectInput("station_train", "Select Station:", 
                    choices = unique(daily_station$station),
                    selected = "Clementi"),
      selectInput("time_resolution_train", "Time Resolution:",
                 choices = c("Daily", "Weekly", "Monthly"),
                 selected = "Monthly"),
      dateRangeInput("date_range_train", "Training Period:",
                    start = "2022-01-01", 
                    end = "2025-01-31",
                    separator = " - ",
                    format = "yyyy-mm-dd"),
      selectInput("models_train", "Select Models:",
                        choices = c(
                          "ETS(AAA)" = "ETS_AAA",
                          "ETS(MAM)" = "ETS_MAM",
                          "ETS(MMM)" = "ETS_MMM",
                          "ETS(AAN)" = "ETS_AAN",
                          "ETS(MMN)" = "ETS_MMN",
                          "ETS(ANN)" = "ETS_ANN",
                          "Auto ETS" = "Auto_ETS",
                          "Auto ARIMA" = "Auto_ARIMA"
                        ),
                        multiple = TRUE,
                        selected = c("ETS_AAA", "ETS_MAM", "ETS_MMM", "ETS_AAN", "ETS_MMN", "ETS_ANN", "Auto_ETS", "Auto_ARIMA")),
      div(style = "text-align: right;",
        actionButton("run_train", "Run Training", 
                    class = "btn-primary",
                    icon = icon("play"))
      )
    )
  ),
  # Right column - Results
  column(width = 9,
    # Step 1-1: Model Training and Evaluation
    box(width = 6,
      title = "Step 1-1: Model Training & Fitting (Train-Test Split: 80%-20%)",
      status = "primary",
      solidHeader = TRUE,
      # Forecasting Performance Plot
      plotOutput("forecast_plot_train", height = "400px")
    ),
    # Step 1-2: Model Evaluation Metrics
    box(width = 6,
      title = "Step 1-2: Model Evaluation: AIC, BIC and RMSE",
      status = "primary",
      solidHeader = TRUE,
      DTOutput("model_metrics_table")
    )
  )
)

# Model Selection Tab
ModelTab <- fluidRow(
  # Left column - Controls
  column(width = 3,
    box(width = 12, title = "Step 2: Forecasting Settings", status = "info",
      selectInput("variable_model", "Select Variable:", 
                    choices = c("Mean Temperature", "Min Temperature", "Max Temperature",
                              "Rainfall", "Mean Wind Speed", "Max Wind Speed"),
                    selected = "Mean Temperature"),
      selectInput("station_model", "Select Station:", 
                    choices = unique(daily_station$station),
                    selected = "Clementi"),
      selectInput("time_resolution_model", "Time Resolution:",
                 choices = c("Daily", "Weekly", "Monthly"),
                 selected = "Monthly"),
      dateRangeInput("date_range_model", "Training Period:",
                    start = "2022-01-01", 
                    end = "2025-01-31",
                    separator = " - ",
                    format = "yyyy-mm-dd"),
      sliderInput("forecast_period_model", "Forecast Period:",
                  value = 12,
                  min = 1,
                  max = 24,
                  step = 1),
      selectInput("models_final", "Select Final Models:",
                        choices = c(
                          "ETS(AAA)" = "ETS_AAA",
                          "ETS(MAM)" = "ETS_MAM",
                          "ETS(MMM)" = "ETS_MMM",
                          "ETS(AAN)" = "ETS_AAN",
                          "ETS(MMN)" = "ETS_MMN",
                          "ETS(ANN)" = "ETS_ANN",
                          "Auto ETS" = "Auto_ETS",
                          "Auto ARIMA" = "Auto_ARIMA"
                        ),
                        multiple = TRUE,
                        selected = c("Auto_ETS", "Auto_ARIMA")),
      div(style = "text-align: right;",
        actionButton("run_model", "Generate Forecast", 
                    class = "btn-primary",
                    icon = icon("chart-line"))
      )
    )
  ),
  # Right column - Results
  column(width = 9,
    # Step 2-1: Model Training and Evaluation
    box(width = 6,
      title = "Step 2-1: Model Fitting and Forecasting",
      status = "primary",
      solidHeader = TRUE,
      plotOutput("forecast_plot_final", height = "300px")
    ),
    # Step 2-2: Residual Analysis
    box(width = 6,
      title = "Step 2-2: Residual Analysis",
      status = "primary",
      solidHeader = TRUE,
      plotOutput("residuals_plot", height = "300px")
    ),
    fluidRow(
      # Parameters table
      column(width = 12,
        box(width = 12,
          title = "Table: Parameters of the Selected Forecasting Models",
          status = "primary",
          solidHeader = TRUE,
          DTOutput("model_parameters_table", height = "280px")
        )
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
        tags$li(HTML("<strong>Weather Data:</strong> Historical weather data from Meteorological Service Singapore (2019-2025)"))
      ),
      h3("Analysis Components"),
      tags$ul(
        tags$li(HTML("<strong>Exploratory Analysis:</strong> Interactive visualizations for understanding weather patterns")),
        tags$li(HTML("<strong>Forecasting:</strong> Advanced time series models for weather prediction"))
      ),
      h3("Contact"),
      p("For more information or feedback, please visit our ", 
        tags$a(href = brand_settings$meta$link$home, "website", target = "_blank"), ".")
  )
)

# main body
body <- dashboardBody(
  add_busy_spinner(spin = "fading-circle"),
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
    tabItem(tabName = "Isohyet", IsohyetmapTab),
    
    # Time-Series Tab
    tabItem(tabName = "Decomposition", DecompositionTab),
    tabItem(tabName = "Correlogram", CorrelogramTab),

    # Forecasting tabs
    tabItem(tabName = "Training", TrainingTab),
    tabItem(tabName = "Model", ModelTab),

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
  # Print structure of daily_station for debugging
  print("Structure of daily_station:")
  print(str(daily_station))
  print("Column names of daily_station:")
  print(colnames(daily_station))
  
  date_min <- min(daily_station$date, na.rm = TRUE)
  date_max <- max(daily_station$date, na.rm = TRUE)
  
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
  output$linechart <- renderHighchart({
    req(input$variable_ts, input$station_ts, input$date_range_ts)
    
    variable_column <- input$variable_ts
    
    data_filtered <- daily_station %>%
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
               color = "rgba(144,238,144,0.2)")  # SW Monsoon
        ))
      }
    }
    
    hc <- highchart() %>%
      hc_title(text = paste("Time Series of", input$variable_ts)) %>%
      hc_subtitle(
        text = "<span style='font-size:14px'>
           <span style='background-color:rgba(135,206,250,0.2); padding: 4px 8px; display: inline-block;'></span> NE Monsoon &nbsp;&nbsp;
           <span style='background-color:rgba(144,238,144,0.2); padding: 4px 8px; display: inline-block;'></span> SW Monsoon &nbsp;&nbsp;
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
  output$summary_stats_table <- renderTable({
    req(input$variable_ts, input$station_ts, input$date_range_ts)
    
    variable_column <- input$variable_ts
    
    data_filtered <- daily_station %>%
      filter(station %in% input$station_ts,
             date >= input$date_range_ts[1],
             date <= input$date_range_ts[2]) %>%
      group_by(station) %>%
      summarise(
        Min = round(min(.data[[variable_column]], na.rm = TRUE), 2),
        Q1 = round(quantile(.data[[variable_column]], 0.25, na.rm = TRUE), 2),
        Q2 = round(median(.data[[variable_column]], na.rm = TRUE), 2),
        Mean = round(mean(.data[[variable_column]], na.rm = TRUE), 2),
        Q3 = round(quantile(.data[[variable_column]], 0.75, na.rm = TRUE), 2),
        Max = round(max(.data[[variable_column]], na.rm = TRUE), 2),
        SD = round(sd(.data[[variable_column]], na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    data_filtered
  })
  
  
  # Placeholder chart
  LineChart_data <- reactive({
    req(input$station_ts, input$variable_ts, input$date_range_ts)
    
    variable_column <- input$variable_ts
    
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

  # Reactive data for RidgePlot analysis
  RidgePlot_data <- reactive({
    req(input$station_biv, input$var_biv, input$date_range_biv, input$aggregation_biv)
    
   
    region_mapping <- tibble(
      station = c("Changi", "Ang Mo Kio", "Clementi",
                  "Jurong (West)", "Paya Lebar", "Newton", "Pasir Panjang",
                  "Tai Seng", "Admiralty"),
      region = c("East",  "North-East", "West",
                 "West", "East", "Central", "Central",
                 "Central", "North"),
      region_code = c("ER", "NER", "WR",
                      "WR", "ER", "CR", "CR",
                      "CR", "NR")
    )
    

    daily_station <- daily_station %>%
      left_join(region_mapping, by = "station")
    
    
    variable_column <- switch(input$var_biv,
                              "Temperature" = "mean_temperature_c",
                              "Rainfall" = "daily_rainfall_total_mm",
                              "Wind Speed" = "mean_wind_speed_km_h")
    
 
    data_filtered <- daily_station %>%
      filter(station %in% input$station_biv,
             date >= input$date_range_biv[1],
             date <= input$date_range_biv[2]) %>%
      select(date, station, value = all_of(input$var_biv)) %>%
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
    station = c("Changi", "Ang Mo Kio", "Clementi",
                "Jurong (West)", "Paya Lebar", "Newton", "Pasir Panjang",
                "Tai Seng", "Admiralty"),
    lat = c(1.35735,  1.37011, 1.31511, 1.34039, 1.35917, 1.31141, 1.27642, 1.33554, 1.44066),
    lon = c(103.9874, 103.8490, 103.7650, 103.7054, 103.8944, 103.8369, 103.7919, 103.8885, 103.8000)
  )
  
  station_grid <- station_to_geo %>%
    mutate(
      code = station,
      name = station,
      row = c(4, 2, 3, 2, 3, 4, 5, 4, 1),  
      col = c(5, 3, 1, 1, 4, 3, 2, 4, 3)   
    ) %>%
    select(code, name, row, col)
  
  
  
  # Import data to Geofacet
  Geofacet_data <- reactive({
    req(input$station_multi, input$var_geofacet, input$date_range_multi, input$aggregation_geofacet)
    
    variable_column <- input$var_geofacet
    
    daily_station %>%
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
      geom_line(color = "#8AA4FF", size = 0.6) +
      facet_geo(~ station, grid = station_grid) +
      labs(title = paste("Weather Trends by Station -", input$var_geofacet),
           x = "Date", y = input$var_geofacet) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +   
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.title.x = element_text(margin = margin(t = 10)),  
        axis.text.y = element_text(size = 9),
        plot.margin = margin(t = 10, r = 10, b = 40, l = 10)
        
      )
  })

  
  # ridge plot
  # Summary Table
  output$station_summary_table <- renderTable({
    req(RidgePlot_data())
    data <- RidgePlot_data()
    
    summary_df <- data %>%
      group_by(station) %>%
      summarise(
        Min = round(min(value, na.rm = TRUE), 2),
        Q1 = round(quantile(value, 0.25, na.rm = TRUE), 2),
        Median = round(median(value, na.rm = TRUE), 2),
        Mean = round(mean(value, na.rm = TRUE), 2),
        Q3 = round(quantile(value, 0.75, na.rm = TRUE), 2),
        Q4 = round(quantile(value, 1.00, na.rm = TRUE), 2),
        Max = round(max(value, na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    summary_df
  })
  
  # RidgePlot with Boxplot
  # 颜色定义
  station_colors <- c(
    "Changi" = "#66C2A5",
    "Newton" = "#8DA0CB",
    "Clementi" = "#FC8D62",
    "Jurong (West)" = "#E78AC3",
    "Ang Mo Kio" = "#A6D854",
    "Admiralty" = "#FFD92F",
    "Paya Lebar" = "#E5C494",
    "Pasir Panjang" = "#B3B3B3",
    "Tai Seng" = "#66C2A5"
  )
  
  # Color Legend 
  output$color_legend_ui <- renderUI({
    tags$div(
      style = "display: flex; flex-direction: column; align-items: center; margin-top: 20px;",
      
      tags$h4("Station Colors", style = "font-weight: bold; text-align: center; margin-bottom: 15px;"),
      
      tags$div(
        style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 20px;",
        lapply(names(station_colors), function(station) {
          tags$div(
            style = "display: flex; align-items: center;",
            tags$div(style = sprintf("width: 15px; height: 15px; background-color: %s; margin-right: 8px; border: 1px solid #aaa;", station_colors[[station]])),
            tags$span(station)
          )
        })
      )
    )
  })
  
  
  
  output$ridge_plot_biv <- renderPlot({
    req(RidgePlot_data())
    
    data <- RidgePlot_data()   
    
    data <- data %>%
      arrange(date) %>%
      mutate(
        value = zoo::na.approx(value, na.rm = TRUE, maxgap = 30),
        month = lubridate::month(date, label = TRUE, abbr = FALSE),
        month_num = as.numeric(forcats::fct_rev(month))
      ) %>%
      drop_na(value)
    
    ggplot(data, aes(x = value, y = forcats::fct_rev(month), fill = station)) +  # ✅ fill = station
      ggridges::geom_density_ridges(
        scale = 2,
        alpha = 0.6
      ) +
      geom_boxplot(
        aes(group = month_num),
        position = position_nudge(y = -0.25),
        width = 0.15,
        outlier.shape = NA,
        fill = "white",
        color = "gray40",
        linewidth = 0.5
      ) +
      scale_fill_manual(values = station_colors) +
      facet_wrap(~ station, ncol = 2) +  
      labs(
        title = paste("Ridgeline Plot with Boxplots of", input$var_biv, "per Month by Station"),
        x = input$var_biv,
        y = "Month"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
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

  
  # Reactive data for Geofacet analysis
  output$station_line_plot <- renderPlot({
    req(input$station_single, input$var_single, input$date_range_single)
    
    df <- daily_station %>%
      filter(station %in% input$station_single,
             date >= input$date_range_single[1],
             date <= input$date_range_single[2]) %>%
      select(date, station, value = all_of(input$var_single)) %>%
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
      theme_minimal(base_size = 13)+
      theme(plot.title = element_text(hjust = 0.5),
            plot.margin = margin(t = 10, r = 10, b = 40, l = 10)
            )
    
  })
  
  # Isohyet Map  Server Functions
#==========================================================

# Store results in reactive values
results <- reactiveValues(
  station_data = NULL,
  idw_pred = NULL,
  variogram_fit = NULL,
  kriging_pred = NULL
)

# Function to prepare station data
prepare_station_data <- function() {
  req(input$variable_map, input$time_resolution, input$date_range_map)
  
  # Get the appropriate daily_station based on time resolution
  time_resolution_table <- switch(tolower(input$time_resolution),
                                "daily" = daily_station_sf,
                                "weekly" = weekly_station_sf,
                                "monthly" = monthly_station_sf)
  
  if (is.null(time_resolution_table)) {
    stop("Invalid time resolution selected")
  }
  
  # Filter and summarize data
  station_summary <- time_resolution_table %>%
    filter(date >= input$date_range_map[1] & date <= input$date_range_map[2]) %>%
    group_by(station) %>%
    summarize(value = mean(get(input$variable_map), na.rm = TRUE),
              .groups = "drop")
  
  if (nrow(station_summary) == 0) {
    stop("No data available for the selected date range")
  }
  
  return(station_summary)
}

# Function to perform IDW interpolation
perform_idw <- function(station_data) {
  if (is.null(station_data) || nrow(station_data) == 0) {
    stop("No station data available for interpolation")
  }
  
  # Create interpolation grid
  grid <- terra::rast(mpsz, nrows = 345, ncols = 537)
  xy <- terra::xyFromCell(grid, 1:ncell(grid))
  coop <- st_as_sf(as.data.frame(xy), 
                  coords = c("x", "y"),
                  crs = st_crs(mpsz))
  coop <- st_filter(coop, mpsz)
  
  # IDW interpolation
  res <- gstat(formula = value ~ 1, 
              locations = station_data, 
              nmax = input$idw_nmax,
              set = list(idp = 0))
  resp <- predict(res, coop)
  
  # Create raster from predictions
  resp$x <- st_coordinates(resp)[,1]
  resp$y <- st_coordinates(resp)[,2]
  resp$pred <- resp$var1.pred
  idw_pred <- terra::rasterize(resp, grid, field = "pred")
  
  return(idw_pred)
}

# Function to perform kriging
perform_kriging <- function(station_data) {
  if (is.null(station_data) || nrow(station_data) == 0) {
    stop("No station data available for kriging")
  }
  
  # Create interpolation grid
  grid <- terra::rast(mpsz, nrows = 345, ncols = 537)
  xy <- terra::xyFromCell(grid, 1:ncell(grid))
  coop <- st_as_sf(as.data.frame(xy), 
                  coords = c("x", "y"),
                  crs = st_crs(mpsz))
  coop <- st_filter(coop, mpsz)
  
  # Calculate and fit variogram
  v <- variogram(value ~ 1, data = station_data)
  if (nrow(v) == 0) {
    stop("Could not compute variogram - insufficient data points")
  }
  
  fv <- fit.variogram(object = v,
                     model = vgm(model = input$variogram_model))
  
  # Perform kriging
  k <- gstat(formula = value ~ 1, 
            data = station_data, 
            model = fv)
  resp <- predict(k, coop)
  
  # Create raster from predictions
  resp$x <- st_coordinates(resp)[,1]
  resp$y <- st_coordinates(resp)[,2]
  resp$pred <- resp$var1.pred
  kriging_pred <- terra::rasterize(resp, grid, field = "pred")
  
  return(list(kriging_pred = kriging_pred, variogram_fit = fv))
}

# Run Model button observer


observeEvent(input$run_map, {
  withProgress(message = 'Running isohyet map analysis...', {
    tryCatch({
      # Prepare station data
      results$station_data <- prepare_station_data()
      
      # Perform IDW interpolation
      results$idw_pred <- perform_idw(results$station_data)
      
      # Perform kriging
      kriging_results <- perform_kriging(results$station_data)
      results$kriging_pred <- kriging_results$kriging_pred
      results$variogram_fit <- kriging_results$variogram_fit
      
      showNotification("Analysis completed successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      print("Error in isohyet map analysis:")
      print(e)
    })
  })
})

# Step 1: Station Map
output$station_map <- renderPlot({
  req(results$station_data)
  
  
  tmap_mode("plot")
  
  legend_text <- paste(input$variable_map)
  
  tm_shape(mpsz) +
    tm_borders(lwd = 0.1,alpha = 0.6) +
    tm_shape(results$station_data) +
    tm_dots(col = "value", 
            palette = "brewer.purples", 
            size = 0.8, 
            alpha = 0.9, 
            title = legend_text)+
    tm_layout(
            frame = FALSE,
            legend.frame = FALSE,
            legend.outside = TRUE,
            legend.outside.position = "right")
})

# Step 2: IDW Map
output$idw_map <- renderPlot({
  req(results$idw_pred)
  
  tmap_mode("plot")
  
  legend_text <- paste(input$variable_map)
  
  tm_shape(results$idw_pred) + 
    tm_raster(col_alpha = 0.8, 
              col.scale = tm_scale(
                values = "brewer.purples"),
              col.legend = tm_legend(legend_text)) +
    tm_layout(
            frame = FALSE,
            legend.frame = FALSE,
            legend.outside = TRUE,
            legend.outside.position = "right")
})

# Step 3: Variogram Plot
output$variogram_plot <- renderPlot({
  req(results$station_data, results$variogram_fit)
  
  # Calculate variogram
  v <- variogram(value ~ 1, data = results$station_data)
  
  # Plot variogram with improved styling
  plot(v, results$variogram_fit, 
       main = "Variogram with Fitted Model",
       xlab = "Distance",
       ylab = "Semivariance",
       col = "#4D4D4D",
       pch = 19)
})

# Step 4: Kriging Map
output$kriging_map <- renderPlot({
  req(results$kriging_pred)
  
  
  tmap_mode("plot")
  
  legend_text <- paste(input$variable_map)
  
  tm_shape(results$kriging_pred) + 
    tm_raster(col_alpha = 0.8, 
              col.scale = tm_scale_continuous(
                values = "brewer.purples"),
              col.legend = tm_legend(legend_text)) +
    tm_layout(
            frame = FALSE,
            legend.frame = FALSE,
            legend.outside = TRUE,
            legend.outside.position = "right")
})


  # Server logic for DecompositionTab
  observeEvent(input$run_decomp, {
    tryCatch({
      # Ensure all required inputs are provided
      req(input$time_resolution_decomp, input$date_range_decomp, input$variable_decomp, input$station_decomp)
      
      # Filter and prepare data
      data_filtered <- daily_station %>%
        filter(
          date >= input$date_range_decomp[1],
          date <= input$date_range_decomp[2],
          station == input$station_decomp
        ) %>%
        select(date, value = all_of(input$variable_decomp)) %>%
        pivot_longer(cols = value, names_to = "type", values_to = "value") %>%
        drop_na(value)
      
      # Convert to tsibble and aggregate based on time resolution
      ts_data <- as_tsibble(data_filtered, index = date, key = type)
      
      if (input$time_resolution_decomp == "Weekly") {
        ts_data <- ts_data %>%
          mutate(year_week = yearweek(date)) %>%
          group_by(type, year_week) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          distinct(type, year_week, .keep_all = TRUE) %>%
          as_tsibble(index = year_week, key = type)
      } else if (input$time_resolution_decomp == "Monthly") {
        ts_data <- ts_data %>%
          mutate(year_month = yearmonth(date)) %>%
          group_by(type, year_month) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          distinct(type, year_month, .keep_all = TRUE) %>%
          as_tsibble(index = year_month, key = type)
      }
      
      # Perform STL decomposition
      stl_results <- ts_data %>%
        model(stl = STL(value ~ season(window = "periodic"))) %>%
        components()
      
      # Render STL plot with improved styling
      output$stl_plot <- renderPlot({
        autoplot(stl_results) + 
          theme_classic() + 
          scale_color_manual(values = c("#4D4D4D")) +
          labs(x="")+
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5, color = "#4D4D4D"),
            plot.subtitle = element_text(hjust = 0.5, color = "#4D4D4D"),
            axis.title = element_text(size = 8),
            axis.title.y = element_text(color = "#4D4D4D", margin = margin(r = 10)),
            strip.background = element_rect(fill = "#C4C8FF", color = "white"),
            strip.text = element_text(color = "#1A1A1A", size = 8),
            legend.position = 'top',
            legend.background = element_blank(),
            legend.key = element_blank()
          )
      })
      
      # Render seasonal plot with improved styling
      output$seasonal_plot <- renderPlot({
        # Convert data to monthly resolution for cycle plot
        monthly_data <- ts_data %>%
          mutate(year_month = yearmonth(date)) %>%
          index_by(year_month) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          as_tsibble(index = year_month)
        
        monthly_data %>%
          gg_subseries(value) +
            labs(title = paste("Cycle Plot of", input$variable_decomp),
                 subtitle = paste("Station:", input$station_decomp),
                 y = input$variable_decomp,
                 x = "") +
            theme_classic() +
            scale_color_manual(values = c("#4D4D4D")) +
            theme(
              plot.title = element_text(face = "bold", hjust = 0.5, color = "#4D4D4D"),
              plot.subtitle = element_text(hjust = 0.5, color = "#4D4D4D"),
              panel.background = element_rect(fill = "#f5f5f5", color = NA),
              axis.title = element_text(size = 8),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_text(color = "#4D4D4D", margin = margin(r = 10)),
              strip.background.x = element_rect(fill = "#C4C8FF", color = "white"),
              strip.background.y = element_rect(color = "white"),
              strip.text.x = element_text(color = "#1A1A1A", size = 8),
              strip.text.y = element_text(color = "white", size = 7),
              axis.text.x = element_blank()
            )
      })
      
    }, error = function(e) {
      print("Error in decomposition:")
      print(e)
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Server logic for CorrelogramTab
  observeEvent(input$run_corr, {
    tryCatch({
      # Ensure all required inputs are provided
      req(input$time_resolution_corr, input$date_range_corr, input$variable_corr, 
          input$station_corr, input$lag_max, input$plot_type)
      
      # Filter and prepare data
      data_filtered <- daily_station %>%
        filter(
          date >= input$date_range_corr[1],
          date <= input$date_range_corr[2],
          station == input$station_corr
        ) %>%
        group_by(date) %>%
        select(date, value = all_of(input$variable_corr)) %>%
        mutate(value = as.numeric(value)) %>%  # Ensure numeric type
        pivot_longer(cols = value, 
                    names_to = "type", 
                    values_to = "value") %>%
        as_tsibble(index = date, key = type)
      
      # Convert to tsibble and aggregate based on time resolution
      if (input$time_resolution_corr == "Weekly") {
        ts_data <- data_filtered %>%
          mutate(year_week = yearweek(date)) %>%
          group_by(type, year_week) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          distinct(type, year_week, .keep_all = TRUE) %>%
          as_tsibble(index = year_week, key = type)
      } else if (input$time_resolution_corr == "Monthly") {
        ts_data <- data_filtered %>%
          mutate(year_month = yearmonth(date)) %>%
          group_by(type, year_month) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          distinct(type, year_month, .keep_all = TRUE) %>%
          as_tsibble(index = year_month, key = type)
      } else {
        ts_data <- data_filtered
      }
      
      
      # Ensure value is numeric
      ts_data$value <- as.numeric(ts_data$value)
      
      # Render original series plot
      output$original_plot <- renderPlot({
        tryCatch({
          ts_data %>%
            gg_tsdisplay(
              y = value,
              plot_type = input$plot_type,
              lag_max = input$lag_max
            )
        }, error = function(e) {
          print("Error in original plot:")
          print(e)
          print("Data structure at error:")
          print(str(ts_data))
        })
      })
      
      # Render trend differencing plot
      output$trend_diff_plot <- renderPlot({
        tryCatch({
          ts_data %>%
            gg_tsdisplay(
              difference(value, lag = 1),
              plot_type = "partial",
              lag_max = input$lag_max
            )
        }, error = function(e) {
          print("Error in trend differencing plot:")
          print(e)
          print("Data structure at error:")
          print(str(ts_data))
        })
      })
      
      # Render seasonal differencing plot
      output$seasonal_diff_plot <- renderPlot({
        tryCatch({
          ts_data %>%
            gg_tsdisplay(
              difference(value, difference = 12),
              plot_type = "partial",
              lag_max = input$lag_max
            ) 
        }, error = function(e) {
          print("Error in seasonal differencing plot:")
          print(e)
          print("Data structure at error:")
          print(str(ts_data))
        })
      })
      
    }, error = function(e) {
      print("Error in correlogram analysis:")
      print(e)
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Server logic for TrainingTab
  observeEvent(input$run_train, {
    tryCatch({
      req(input$variable_train, input$station_train, input$date_range_train, 
          input$time_resolution_train, input$models_train)
      
      # Filter and prepare data
      data_filtered <- daily_station %>%
        filter(
          date >= as.Date("2022-01-01"),
          date <= input$date_range_train[2],
          station == input$station_train
        ) %>%
        select(date, value = all_of(input$variable_train)) %>%
        pivot_longer(cols = value, names_to = "type", values_to = "value") %>%
        drop_na(value)
      
      # Convert to tsibble and aggregate based on time resolution
      ts_data <- as_tsibble(data_filtered, index = date, key = type)
      
      if (input$time_resolution_train == "Weekly") {
        ts_data <- ts_data %>%
          mutate(year_week = yearweek(date)) %>%
          group_by(type, year_week) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          distinct(type, year_week, .keep_all = TRUE) %>%
          as_tsibble(index = year_week, key = type)
      } else if (input$time_resolution_train == "Monthly") {
        ts_data <- ts_data %>%
          mutate(year_month = yearmonth(date)) %>%
          group_by(type, year_month) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          distinct(type, year_month, .keep_all = TRUE) %>%
          as_tsibble(index = year_month, key = type)
      }
      
      # Split into training and test sets
      cutoff_row <- floor(0.8 * nrow(ts_data))
      train_data <- ts_data %>% slice(1:cutoff_row)
      test_data <- ts_data %>% slice((cutoff_row + 1):n())
      
      # Define models
      models <- list(
        ETS_AAA = ETS(value ~ error("A") + trend("A") + season("A")), 
        ETS_MAM = ETS(value ~ error("M") + trend("A") + season("M")),
        ETS_MMM = ETS(value ~ error("M") + trend("M") + season("M")),
        ETS_AAN = ETS(value ~ error("A") + trend("A") + season("N")),
        ETS_MMN = ETS(value ~ error("M") + trend("M") + season("N")),
        ETS_ANN = ETS(value ~ error("A") + trend("N") + season("N")),
        Auto_ETS = ETS(value),
        Auto_ARIMA = ARIMA(value)
      )
      
      # Fit selected models with error handling
      tryCatch({
        # Ensure we have valid model selections
        if (length(input$models_train) == 0) {
          stop("Please select at least one model")
        }
        
        # Create a list of selected models
        selected_models <- list()
        for (model_name in input$models_train) {
          if (model_name %in% names(models)) {
            selected_models[[model_name]] <- models[[model_name]]
          } else {
            warning(paste("Model", model_name, "not found in available models"))
          }
        }
        
        # Fit models if we have valid selections
        if (length(selected_models) > 0) {
          fit_models <- train_data %>%
            model(!!!selected_models)
          
          # Generate forecasts
          forecast_results <- fit_models %>%
            forecast(h = nrow(test_data))
          
          # Model metrics table
          matrix <- glance(fit_models) %>%
            select(.model, AIC, BIC)
          
          acc <- accuracy(forecast_results, test_data) %>%
            select(.model, RMSE)
          
          model_matrix <- matrix %>%
            left_join(acc, by = ".model") %>%
            rename(RMSE_ts = RMSE, AIC_tr = AIC, BIC_tr = BIC, Model = .model)%>%
            mutate(RMSE_ts = round(RMSE_ts, 3),
                   AIC_tr = round(AIC_tr, 3),
                   BIC_tr = round(BIC_tr, 3))
          
          # Render model metrics table
          output$model_metrics_table <- renderDT({
            datatable(model_matrix,
                      options = list(
                        scrollY = "400px",
                        scrollCollapse = TRUE,
                        paging = FALSE,
                        searching = TRUE,
                        headerCallback = JS(
                          "function(thead, data, start, end, display) {",
                          "  $(thead).find('th').css({'background-color': '#C4C8FF', 'color': '#4D4D4D', 'font-weight': 'bold'});",
                          "}"
                        )
                      ))
          })
          
          # Comparison table
          if (input$time_resolution_train == "Monthly") {
            comparison_table <- forecast_results %>%
              select(year_month, .model, value, .mean) %>%
              rename(Actual = value, `Forecasted Value` = .mean) %>%
              select(year_month, .model, `Forecasted Value`) %>%
              rename(Model = .model) %>%
              left_join(test_data %>% as_tibble(), by = "year_month") %>%
              mutate(
                `Forecasted Value` = round(`Forecasted Value`, 2),
                Year = year(year_month),
                Month = month(year_month),
                `Actual Value` = round(value, 2)
              ) %>%
              select(Year, Month, Model, `Forecasted Value`, `Actual Value`) %>%
              select(-year_month)
          } else if (input$time_resolution_train == "Weekly") {
            comparison_table <- forecast_results %>%
              select(year_week, .model, value, .mean) %>%
              rename(Actual = value, `Forecasted Value` = .mean) %>%
              select(year_week, .model, `Forecasted Value`) %>%
              rename(Model = .model) %>%
              left_join(test_data %>% as_tibble(), by = "year_week") %>%
              mutate(
                `Forecasted Value` = round(`Forecasted Value`, 2),
                Year = year(year_week),
                Week = week(year_week),
                `Actual Value` = round(value, 2)
              ) %>%
              select(Year, Week, Model, `Forecasted Value`, `Actual Value`) %>%
              select(-year_week)
          } else {  # Daily resolution
            comparison_table <- forecast_results %>%
              select(date, .model, value, .mean) %>%
              rename(Actual = value, `Forecasted Value` = .mean) %>%
              select(date, .model, `Forecasted Value`) %>%
              rename(Model = .model) %>%
              left_join(test_data %>% as_tibble(), by = "date") %>%
              mutate(
                `Forecasted Value` = round(`Forecasted Value`, 2),
                Year = year(date),
                Month = month(date),
                Day = day(date),
                `Actual Value` = round(value, 2)
              ) %>%
              select(Year, Month, Day, Model, `Forecasted Value`, `Actual Value`) %>%
              select(-date)
          }
          
          
          # Forecast plot
          output$forecast_plot_train <- renderPlot({
            autoplot(forecast_results, level = c(95)) +
              autolayer(ts_data, series = "Actual", color = "#4D4D4D") +
              labs(title = "Model Training Results",
                   y = input$variable_train,
                   x = "") +
              theme_classic() +
              theme(
                plot.title = element_text(face = "bold", hjust = 0.5, color = "#4D4D4D"),
                axis.line.y = element_blank(),
                axis.line.x = element_line(color = "#4D4D4D", size = 0.5),
                axis.title.y = element_text(color = "#4D4D4D", margin = margin(r = 10)),
                legend.position = "bottom",
                legend.key = element_blank(),
                legend.key.size = unit(0.2, "cm"),
                legend.title = element_text(size = 9),
                legend.text = element_text(size = 8)
              )
          })
        } else {
          stop("No valid models selected")
        }
      }, error = function(e) {
        print("Error in model fitting:")
        print(e)
        showNotification(paste("Error:", e$message), type = "error")
      })
    }, error = function(e) {
      print("Error in model training:")
      print(e)
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Server logic for ModelTab
  observeEvent(input$run_model, {
    tryCatch({
      req(input$variable_model, input$station_model, input$date_range_model, 
          input$time_resolution_model, input$models_final, input$forecast_period_model)
      
      # Filter and prepare data
      data_filtered <- daily_station %>%
        filter(
          date >= as.Date("2022-01-01"),
          date <= input$date_range_model[2],
          station == input$station_model
        ) %>%
        select(date, value = all_of(input$variable_model)) %>%
        pivot_longer(cols = value, names_to = "type", values_to = "value") %>%
        drop_na(value)
      
      # Convert to tsibble and aggregate based on time resolution
      ts_data <- as_tsibble(data_filtered, index = date, key = type)
      
      if (input$time_resolution_model == "Weekly") {
        ts_data <- ts_data %>%
          mutate(year_week = yearweek(date)) %>%
          group_by(type, year_week) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          distinct(type, year_week, .keep_all = TRUE) %>%
          as_tsibble(index = year_week, key = type)
      } else if (input$time_resolution_model == "Monthly") {
        ts_data <- ts_data %>%
          mutate(year_month = yearmonth(date)) %>%
          group_by(type, year_month) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          distinct(type, year_month, .keep_all = TRUE) %>%
          as_tsibble(index = year_month, key = type)
      }
      
      # Define models
      models <- list(
        ETS_AAA = ETS(value ~ error("A") + trend("A") + season("A")), 
        ETS_MAM = ETS(value ~ error("M") + trend("A") + season("M")),
        ETS_MMM = ETS(value ~ error("M") + trend("M") + season("M")),
        ETS_AAN = ETS(value ~ error("A") + trend("A") + season("N")),
        ETS_MMN = ETS(value ~ error("M") + trend("M") + season("N")),
        ETS_ANN = ETS(value ~ error("A") + trend("N") + season("N")),
        Auto_ETS = ETS(value),
        Auto_ARIMA = ARIMA(value)
      )
      
      # Fit selected models
      models_to_fit <- models[input$models_final]
      fit_models <- ts_data %>%
        model(!!!models_to_fit)
      
      # Generate forecasts
      forecast_results <- fit_models %>%
        forecast(h = input$forecast_period_model)
      
      # Forecast plot
      output$forecast_plot_final <- renderPlot({
        autoplot(forecast_results, level = c(95)) +
          autolayer(ts_data, series = "Actual", color = "#4D4D4D") +
          labs(title = "Final Forecast Results",
               y = input$variable_model,
               x = "") +
          theme_classic() +
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5, color = "#4D4D4D"),
            axis.line.y = element_blank(),
            axis.line.x = element_line(color = "#4D4D4D", size = 0.5),
            axis.title.y = element_text(color = "#4D4D4D", margin = margin(r = 10)),
            legend.position = "bottom",
            legend.key = element_blank(),
            legend.key.size = unit(0.2, "cm"),
            legend.title = element_text(size = 9),
            legend.text = element_text(size = 8)
          )
      })
      
      # Model parameters table
      parameters <- fit_models %>%
        tidy() %>%
        mutate(
          estimate = round(estimate, 3),
          std.error = round(std.error, 3),
          statistic = round(statistic, 3),
          p.value = round(p.value, 3)
        )
      
      output$model_parameters_table <- renderDT({
        datatable(parameters,
                  options = list(
                    scrollY = "250px",
                    scrollCollapse = TRUE,
                    paging = FALSE,
                    searching = TRUE,
                    headerCallback = JS(
                      "function(thead, data, start, end, display) {",
                      "  $(thead).find('th').css({'background-color': '#C4C8FF', 'color': '#4D4D4D', 'font-weight': 'bold'});",
                      "}"
                    )
                  ))
      })
      
      # Residuals plot
      output$residuals_plot <- renderPlot({
        residuals <- residuals(fit_models)
        
        autoplot(residuals, .vars = .resid) +
          labs(title = "Residuals of Final Models",
               x = "",
               y = "Residuals") +
          theme_classic() +
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5, color = "#4D4D4D"),
            axis.line.y = element_blank(),
            axis.line.x = element_line(color = "#4D4D4D", size = 0.5),
            axis.title.y = element_text(color = "#4D4D4D", margin = margin(r = 10)),
            legend.position = "bottom",
            legend.key = element_blank(),
            legend.key.size = unit(0.2, "cm"),
            legend.title = element_text(size = 9),
            legend.text = element_text(size = 8)
          )
      })
      
    }, error = function(e) {
      print("Error in final model:")
      print(e)
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
