

# This Shiny app allows users to interactively explore CO2, Nitrous Oxide, Methane, and GHG emissions data across different continents and 
# countries over a selected time range. It dynamically filters countries based on continent/group selection and visualizes the emissions using
# scatter, line, or bar plots. Users can choose emission types (CO2, Nitrous Oxide, etc.), select a year range, and visualize the data for
# specific countries.




# Importing required Libraries and packages
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinydashboard)
library(bslib)

# Load  data set
co2_data <- read.csv("owid-co2-data.csv")

# Separating Different countries based on continents

# African countries
africa <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cape Verde", "Cameroon", 
  "Central African Republic", "Chad", "Comoros", "Congo", "Democratic Republic of Congo", "Djibouti", 
  "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", 
  "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", 
  "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", 
  "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
  "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe", 
  "Africa", "Africa (GCP)"
)

# Asian countries
asia <- c(
  "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei", "Cambodia", 
  "China", "Cyprus", "East Timor", "Georgia", "India", "Indonesia", "Iran", "Iraq", "Israel", 
  "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Maldives", 
  "Mongolia", "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan", "Palestine", "Philippines", 
  "Qatar", "Saudi Arabia", "Singapore", "South Korea", "Sri Lanka", "Syria", "Taiwan", "Tajikistan", 
  "Thailand", "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen", 
  "Asia", "Asia (GCP)", "Asia (excl. China and India)"
)

# European countries
europe <- c(
  "Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", 
  "Croatia", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
  "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", 
  "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", 
  "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", 
  "Sweden", "Switzerland", "Ukraine", "United Kingdom", "Vatican City", 
  "Europe", "Europe (GCP)", "Europe (excl. EU-27)", "Europe (excl. EU-28)", 
  "European Union (27)", "European Union (28)"
)

# North American countries
north_america <- c(
  "Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Bermuda", "Canada", "Costa Rica", 
  "Cuba", "Dominica", "Dominican Republic", "El Salvador", "Grenada", "Guatemala", "Haiti", 
  "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Saint Kitts and Nevis", 
  "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad and Tobago", "United States", 
  "North America", "North America (GCP)", "North America (excl. USA)"
)

# South American countries
south_america <- c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", 
  "Peru", "Suriname", "Uruguay", "Venezuela", "South America", "South America (GCP)"
)

# Oceania countries
oceania<- c(
  "Australia", "Fiji", "Kiribati", "Marshall Islands", "Micronesia (country)", "Nauru", 
  "New Zealand", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu", 
  "Vanuatu", "Oceania", "Oceania (GCP)"
)

# Antarctica
antarctica <- c("Antarctica")

# Other special groups
special_groups <- c(
  "High-income countries", "Least developed countries (Jones et al.)", "Low-income countries", 
  "Lower-middle-income countries", "Middle East (GCP)", "Non-OECD (GCP)", "OECD (GCP)", 
  "OECD (Jones et al.)", "Panama Canal Zone", "Panama Canal Zone (GCP)", 
  "St. Kitts-Nevis-Anguilla", "St. Kitts-Nevis-Anguilla (GCP)", 
  "Upper-middle-income countries", "World"
)

# UI Definition with tab-set Panel for CO2, Nitrous Oxide and Methane

ui <- fluidPage(
  theme = bs_theme(bootswatch = "cyborg"),  # Add a theme 
  sidebarLayout(
    sidebarPanel(
      # Drop down to select multiple continents / groups
      selectInput("continent_group", "Select Continent or Group:", 
                  choices = c("Africa", "Asia", "Europe", "North America", "South America", "Oceania", "Antarctica", "Special Groups"), 
                  multiple = TRUE),
      
      # Drop down to select countries dynamically based on continent / group selection
      uiOutput("country_select"),
      
      
      radioButtons("plot_type", "Select Plot Type:", 
                   choices = c("Scatter Plot" = "scatter", 
                               "Line Plot" = "line", 
                               "Bar Plot" = "bar")),
      
      # Slider to select a range of years
      sliderInput("year_range", "Select Year Range:", 
                  min = min(co2_data$year), 
                  max = max(co2_data$year),
                  value = c(min(co2_data$year), max(co2_data$year)),
                  step = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        id = "emissions_tabs",
        tabPanel("CO2 Emissions", plotlyOutput("co2_time_plot")),
        tabPanel("Nitrous Oxide Emissions", plotlyOutput("n2o_time_plot")),
        tabPanel("Methane Emissions", plotlyOutput("ch4_time_plot")),
        tabPanel("Total GHG Emissions", plotlyOutput("ghg_time_plot")),
        tabPanel("GHG per Capita", plotlyOutput("ghg_per_capita_time_plot"))
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Reactive expression for the countries list based on continent/group selection
  countries_reactive <- reactive({
    selected_continents <- input$continent_group
    countries <- c()
    
    if ("Africa" %in% selected_continents) {
      countries <- c(countries, africa)
    }
    if ("Asia" %in% selected_continents) {
      countries <- c(countries, asia)
    }
    if ("Europe" %in% selected_continents) {
      countries <- c(countries, europe)
    }
    if ("North America" %in% selected_continents) {
      countries <- c(countries, north_america)
    }
    if ("South America" %in% selected_continents) {
      countries <- c(countries, south_america)
    }
    if ("Oceania" %in% selected_continents) {
      countries <- c(countries, oceania)
    }
    if ("Antarctica" %in% selected_continents) {
      countries <- c(countries, antarctica)
    }
    if ("Special Groups" %in% selected_continents) {
      countries <- c(countries, special_groups)
    }
    
    return(countries)
  })
  
  # Dynamically update country selection based on continent / group selection using reactive
  output$country_select <- renderUI({
    selectInput("country", "Select Country:", choices = countries_reactive(), multiple = TRUE)
  })
  
  # Render CO2 Emissions Plot
  output$co2_time_plot <- renderPlotly({
    filtered_data <- co2_data %>% 
      filter(country %in% input$country, year >= input$year_range[1], year <= input$year_range[2])
    
    if (input$plot_type == "scatter") {
      # Scatter plot
      p <- plot_ly(filtered_data, x = ~year, y = ~co2, color = ~country, type = 'scatter', mode = 'markers')
    } else if (input$plot_type == "line") {
      # Line plot
      p <- plot_ly(filtered_data, x = ~year, y = ~co2, color = ~country, type = 'scatter', mode = 'lines')
    } else if (input$plot_type == "bar") {
      # Bar plot
      p <- plot_ly(filtered_data, x = ~year, y = ~co2, color = ~country, type = 'bar')
    }
    
    p 
  })
  
  # Render Nitrous Oxide Emissions Plot
  output$n2o_time_plot <- renderPlotly({
    filtered_data <- co2_data %>% 
      filter(country %in% input$country, year >= input$year_range[1], year <= input$year_range[2])
    
    if (input$plot_type == "scatter") {
      # Scatter plot
      p <- plot_ly(filtered_data, x = ~year, y = ~nitrous_oxide, color = ~country, type = 'scatter', mode = 'markers')
    } else if (input$plot_type == "line") {
      # Line plot
      p <- plot_ly(filtered_data, x = ~year, y = ~nitrous_oxide, color = ~country, type = 'scatter', mode = 'lines')
    } else if (input$plot_type == "bar") {
      # Bar plot
      p <- plot_ly(filtered_data, x = ~year, y = ~nitrous_oxide, color = ~country, type = 'bar')
    }
    
    p 
  })
  
  # Render Methane Emissions Plot
  output$ch4_time_plot <- renderPlotly({
    filtered_data <- co2_data %>% 
      filter(country %in% input$country, year >= input$year_range[1], year <= input$year_range[2])
    
    if (input$plot_type == "scatter") {
      # Scatter plot
      p <- plot_ly(filtered_data, x = ~year, y = ~methane, color = ~country, type = 'scatter', mode = 'markers')
    } else if (input$plot_type == "line") {
      # Line plot
      p <- plot_ly(filtered_data, x = ~year, y = ~methane, color = ~country, type = 'scatter', mode = 'lines')
    } else if (input$plot_type == "bar") {
      # Bar plot
      p <- plot_ly(filtered_data, x = ~year, y = ~methane, color = ~country, type = 'bar')
    }
    
    p 
  })
  
  # Render Total GHG Emissions Plot
  output$ghg_time_plot <- renderPlotly({
    filtered_data <- co2_data %>%
      filter(country %in% input$country, year >= input$year_range[1], year <= input$year_range[2])
    
    if (input$plot_type == "scatter") {
      # Scatter plot
      p <- plot_ly(filtered_data, x = ~year, y = ~total_ghg, color = ~country, type = 'scatter', mode = 'markers')
    } else if (input$plot_type == "line") {
      # Line plot
      p <- plot_ly(filtered_data, x = ~year, y = ~total_ghg, color = ~country, type = 'scatter', mode = 'lines')
    } else if (input$plot_type == "bar") {
      # Bar plot
      p <- plot_ly(filtered_data, x = ~year, y = ~total_ghg, color = ~country, type = 'bar')
    }
    
    p
  })
  
  # Render GHG per Capita Plot
  output$ghg_per_capita_time_plot <- renderPlotly({
    filtered_data <- co2_data %>%
      filter(country %in% input$country, year >= input$year_range[1], year <= input$year_range[2]) %>%
      mutate(ghg_per_capita = total_ghg / population)
    
    if (input$plot_type == "scatter") {
      # Scatter plot
      p <- plot_ly(filtered_data, x = ~year, y = ~ghg_per_capita, color = ~country, type = 'scatter', mode = 'markers')
    } else if (input$plot_type == "line") {
      # Line plot
      p <- plot_ly(filtered_data, x = ~year, y = ~ghg_per_capita, color = ~country, type = 'scatter', mode = 'lines')
    } else if (input$plot_type == "bar") {
      # Bar plot
      p <- plot_ly(filtered_data, x = ~year, y = ~ghg_per_capita, color = ~country, type = 'bar')
    }
    
    p
  })
}

# Run the app
shinyApp(ui = ui, server = server)