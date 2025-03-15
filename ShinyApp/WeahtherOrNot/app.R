library(shiny)

#========================================================== 
## UI Components
#========================================================== 

# main header ---
header <- dashboardHeader(
  title = tags$img(src = "images/Logo.png", height = "50px")
)

# main sidebar ---
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", href = "https://vaagroup13.netlify.app", target = "_blank"),
    menuItem("Data Exploration", tabName = "Data Exploration"),
    menuItem("Confirmatory Ananlysis", tabName = "Confirmatory Ananlysis"),
    menuItem("Forecasting", tabName = "Forecasting")
  )
)


# main body ---
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Data Exploration",
            EDASubTabs
    ),
    tabItem(tabName = "Confirmatory Ananlysis",
            CDASubTabs 
    ),
    tabItem(tabName = "Forecasting",
            ForecastSubTabs
    )))

# fluidRows ---

# aspatial analysis tab
EDAUnivariate <-  fluidRow(
  highchartOutput(), # display line chart
  sliderInput(), # select year range
  selectizeInput(), # select event (allows multiple selection)
  radioButtons(), # select administrative region level
  selectizeInput(), # select administrative region (allows multiple selection)
  actionButton(), # action button
  checkboxInput(), # conditional panel (option to display line chart)
  highchartOutput(), # display line chart
  checkboxInput(), # conditional panel (option to display map information)
  leafletOutput(), # display point spatial map
)

#========edit to here========#

AspatialOverviewrow2 <-  fluidRow(
  DT::dataTableOutput() # display datatable
)

AspatialDistributionrow1 <-  fluidRow(
  sliderInput(), # select year range
  radioButtons(), # select administrative region level
  radioButtons(), # select display rates
  selectInput(), # select colour palette
  checkboxInput(), # Customise Spatial Map
  selectInput(), # Spatial Map Classification Type
  sliderInput(), # Number of Classes
  checkboxInput(), # Customise Density Ridge Plot
  selectInput(), # Density Ridge Style
  tmapOutput(), # choropleth map
  plotOutput() # density ridge plot
)

AspatialDistributionrow2 <-  fluidRow(
  textOutput()
)

# geospatial analysis tab
Cluster2 <- fluidRow(
  selectInput(), # select period
  selectInput(), # select Moran Event Type
  radioButtons(), # select Contiguity method 
  selectInput(), # select Spatial Weights Style
  sliderInput(), # Number of Simulations
  actionButton(), # action button 
  radioButtons(), # select confidence interval 
  selectInput(), # select Lisa Classification
  selectInput(), # select Local Moran's Stat
  plotOutput(), # LocalMoranMap
  plotOutput(), # LisaMap 
  textOutput(),
  DT::dataTableOutput()
)

