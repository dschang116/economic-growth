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
library(readr)
library(ggplot2)
library(lubridate)
library(usmap)

# Load in IMF rGDP data. Rename first column to country 
# Drop NA values.

imf_ten_econ <- read_excel("raw_data/imf-rgdp.xls") %>%
    rename(country = "Real GDP growth (Annual percent change)") %>%
    drop_na()

# Replace values of no data with NA.

imf_ten_econ[imf_ten_econ == "no data"] <- NA

# Rename China.

imf_ten_econ$country[imf_ten_econ$country == 
                         "China, People's Republic of"] <- "China"

# Filter to G20 countries and order alphabetically. 
# Change first column country as factor and rest as numeric column type. 

countries <- c("Brazil", "Canada", "China", "France", 
               "Germany", "India", "Italy", "Japan", 
               "United Kingdom", "United States") %>%
    sort()

ten_econ <- imf_ten_econ %>% 
    filter(country %in% countries) %>% 
    mutate_at(1, as.factor) %>%
    mutate_at(2:47, as.numeric) %>% 
    select(c(1:1, 22:42))

# Load in first time unemployment claims data.

unemploy_state <- read_excel("raw_data/unemploy-state.xlsx") %>% 
    rename(full = "State",
           wk_ended = `Filed week ended`,
           in_claims = `Initial Claims`) %>% 
    select(full, wk_ended, in_claims) %>% 
    group_by(full) %>% 
    summarize(tot_claims = sum(in_claims), 
              .groups = "drop")

# Load in data

unemploy_perc_state <- read_excel("raw_data/unemploy-perc-state.xlsx") %>% 
    mutate(avg_perc = rowMeans(.[2:4])) %>% 
    rename(full = "State")

# Load in COVID-19 data.

covid <- read_csv("raw_data/states-covid.csv", 
                  col_types = cols(
                      .default = col_double(),
                      date = col_date(format = ""),
                      state = col_character(),
                      dataQualityGrade = col_character()
                  )) %>% 
    filter(! state %in% c("AS", "DC", "GU", "MP", "PR", "VI")) %>% 
    select(state, death, positive, totalTestResults) %>% 
    rename(abbr = "state")

covid[is.na(covid)] <- 0

covid_summ <- covid %>%
    group_by(abbr) %>% 
    summarize(tot_death = sum(death),
              tot_pos = sum(positive),
              tot_test = sum(totalTestResults),
              pos_rate = tot_pos / tot_test,
              .groups = "drop")

metrics <- c("First-Time Unemployment Claims", "Average Percent Unemployed (July-September)", 
             "Total Deaths", "Total Positive Cases", "Total Tests", "Positivity Rate")

fips_unemploy_state <- inner_join(unemploy_state, statepop, by = "full") %>% 
    filter(full != "District of Columbia") %>% 
    select(fips, tot_claims)

fips_unemploy_perc_state <- inner_join(unemploy_perc_state, statepop, by = "full") %>% 
    filter(full != "District of Columbia") %>% 
    select(fips, avg_perc)

fips_covid <- inner_join(covid_summ, statepop, by = "abbr") %>% 
    select(fips, tot_death, tot_pos, tot_test, pos_rate)

plot_usmap(data = fips_unemploy_state, values = "tot_claims", color = "black") + 
    scale_fill_continuous(low = "white", high = "green", 
                          name = "Total Unemployment Claims (2020)", 
                          label = scales::comma) + 
    theme(legend.position = "right")



# Define UI for application that draws a histogram
ui <- navbarPage(
    "Economic Growth in the Twenty-First Century: A Look Back, and a Look Forward",
    tabPanel("A Closer Look at the U.S.",
             fluidPage(
                 
                 # Application title
                 titlePanel("Growth of the World's Ten Largest Economies"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         checkboxGroupInput("country", 
                                            ("Pick a country:"), 
                                            choiceNames =
                                                countries,
                                            choiceValues =
                                                countries,
                                            selected = "United States"
                         )),
                     
                     # Show a plot of the generated distribution
                     mainPanel(plotOutput("GDPByCountry")),
                     
                 )
             )),
    tabPanel("A Closer Look at the U.S.",
             fluidPage(
                 
                 # Application title
                 titlePanel("Growth of the World's Ten Largest Economies"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("metric", 
                                        ("Pick a metric:"), 
                                        choiceNames =
                                            metrics,
                                        choiceValues =
                                            metrics,
                                        selected = "First-Time Unemployment Claims"
                     )),
                     
                     # Show a plot of the generated distribution
                     mainPanel(plotOutput("covid_us_map")),
                     
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
                   HTML('.navbar { background-color: white;}
                          .navbar-default .navbar-brand{color: black;}
                          .tab-panel{ background-color: red; color: red}
                          .navbar-default .navbar-nav > .active > a, 
                           .navbar-default .navbar-nav > .active > a:focus, 
                           .navbar-default .navbar-nav > .active > a:hover {
                                color: black;
                                background-color: aqua;
                            }')
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$GDPByCountry = renderPlot({
        countrySelected <- input$country
        
        plotData <- ten_econ %>% filter(country %in% countrySelected) %>%
            pivot_longer(cols = - country,
                         names_to = "year", 
                         values_to = "gdp")
        
        ggplot(plotData, aes(x = year, y = gdp, 
                             group = country, color = country)) +
            geom_line() +
            labs (x = "Year", y = "GDP Growth %", color = "Country", 
                  title = paste("GDP Growth for", 
                                paste(countrySelected, collapse = ', '), 
                                "in Twenty-First Century")) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        
    })
    
    # COVID-19 US map.
    
    output$covid_us_map = renderPlot({
        metricSelected <- input$metric
        
        plotData <- fips_unemploy_state
        
            #case_when(
            #metricSelected == "First-Time Unemployment Claims" ~ fips_unemploy_state 
            #metricSelected[1] == "Average Percent Unemployed (July-September)" ~ fips_unemploy_perc_state, 
            #metricSelected == "Total Deaths" ~ fips_covid, 
            #metricSelected == "Total Positive Cases" ~ fips_covid, 
            #metricSelected == "Total Tests" ~ fips_covid , 
            #metricSelected == "Positivity Rate" ~ fips_covid
        #)
        
        plot_usmap(data = plotData, values = "tot_claims", color = "black") + 
            scale_fill_continuous(low = "white", high = "green", 
                                  name = "Total Unemployment Claims (2020)", 
                                  label = scales::comma) + 
            theme(legend.position = "right")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
