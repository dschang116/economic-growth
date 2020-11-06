#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)

# Load in IMF rGDP data. Rename first column to country 
# Drop NA values.

imf_rGDP_growth <- read_excel("raw_data/imf-dm-export-20201014.xls") %>%
    rename(country = "Real GDP growth (Annual percent change)") %>%
    drop_na()
    
# Replace values of no data with NA.

imf_rGDP_growth[imf_rGDP_growth == "no data"] <- NA


# Rename China and South Korea.

imf_rGDP_growth$country[imf_rGDP_growth$country == 
                            "China, People's Republic of"] <- "China"
imf_rGDP_growth$country[imf_rGDP_growth$country == 
                            "Korea, Republic of"] <- "South Korea"

# Filter to G20 countries and order alphabetically. 
# Change first column country as factor and rest as numeric column type. 

G20_countries <- c("Argentina", "Australia", "Brazil", "Canada", 
                   "China", "France", 
                   "Germany", "India", "Indonesia", "Italy", "Japan", 
                   "South Korea", "Mexico", "Russian Federation", 
                   "Saudi Arabia", "South Africa", "Turkey", 
                   "United Kingdom", "United States", "European Union",
                   "Austria", "Belgium", "Bulgaria",  "Croatia",  
                   "Republic of Cyprus", "Czech Republic", "Denmark", 
                   "Estonia", "Finland", "Greece", 
                   "Hungary", "Ireland", "Latvia", "Lithuania", 
                   "Luxembourg", "Malta", "Netherlands", "Poland", 
                   "Portugal", "Romania", "Slovakia", "Slovenia", 
                   "Spain", "Sweden") %>%
    sort()

G20_rGDP_growth <- imf_rGDP_growth %>% 
    filter(country %in% G20_countries) %>% 
    mutate_at(1, as.factor) %>%
    mutate_at(2:47, as.numeric) %>% 
    select(1:41)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Factors of Economic Growth",
    tabPanel("Distribution by Year",
             fluidPage(
                 
                 # Application title
                 titlePanel("Real GDP Growth Distribution by Year"),
                 
                 selectInput("year", "Choose a year:",
                             list(`Year` = c(seq(1980,2019)))
                 ),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("bins",
                                     "Number of bins:",
                                     min = 1,
                                     max = 20,
                                     value = 10)
                     ),
                     
                     
                     
                     # Show a plot of the generated distribution
                     mainPanel(plotOutput("GDPDistributionByYear"))
                 )
                 
             )),
    tabPanel("Real GDP Growth by Country",
             fluidPage(
                 
                 # Application title
                 titlePanel("G20 Countries and European Union"),
                 
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         checkboxGroupInput("country", 
                                            ("Pick a country:"), 
                                            choiceNames =
                                                G20_countries,
                                            choiceValues =
                                                G20_countries,
                                            selected = "United States"
                         )),
                     
                     # Show a plot of the generated distribution
                     mainPanel(plotOutput("GDPByCountry")),
                     
                 )
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project. I have begun to
               clean the real GDP growth data from the IMF. I have also found
               a second dataset from the World Bank which includes data on 
               HH market concentration index. I have created a histogram of
               growth rates for countries in 1980. This project's GitHub 
               repository lives here: 
               https://github.com/dschang116/Final-Project.") ,
             h3("About Me"),
             p("My name is Derek Chang, and I am a first-year undergraduate at 
             Harvard University studying Economics.
             You can reach me at dschang@college.harvard.edu.")),
    
    tags$head(
        tags$style(type = 'text/css', 
                   HTML('.navbar { background-color: aqua;}
                          .navbar-default .navbar-brand{color: white;}
                          .tab-panel{ background-color: red; color: white}
                          .navbar-default .navbar-nav > .active > a, 
                           .navbar-default .navbar-nav > .active > a:focus, 
                           .navbar-default .navbar-nav > .active > a:hover {
                                color: #555;
                                background-color: blue;
                            }')
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$GDPDistributionByYear <- renderPlot({
        year <- as.numeric(input$year)
        plotData <- G20_rGDP_growth[[year - 1980 + 2]]
        
        bins <- seq(min(plotData, na.rm = TRUE), max(plotData, na.rm = TRUE), 
                    length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(plotData, breaks = bins, col = 'darkmagenta', border = 'white',
             main = paste("Real GDP Growth Distribution for Year", year),
             xlab = "GDP Growth %",
             ylab = "Number")
    })
    
    output$GDPByCountry = renderPlot({
        countrySelected <- input$country
        
        plotData <- G20_rGDP_growth %>% filter(country %in% countrySelected) %>%
            pivot_longer(cols = - country,
                         names_to = "year", 
                         values_to = "gdp")
        
        ggplot(plotData, aes(x = year, y = gdp, 
                             group = country, color = country)) +
            geom_line() +
            labs (x = "Year", y = "GDP Growth %", color = "Country", 
                  title = paste("GDP Growth for", 
                                paste(countrySelected, collapse = ', '), 
                                "Through Years")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
