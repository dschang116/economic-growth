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

# Load in IMF rGDP data.
# Drop NA values.

imf_rGDP_growth <- read_excel("raw_data/imf-dm-export-20201014.xls") %>%
    drop_na()

# Replace values of no data with NA.

imf_rGDP_growth[imf_rGDP_growth == "no data"] <- NA

# Rename first column to country and change column types 

imf_rGDP_growth <- imf_rGDP_growth %>% 
    rename("country" = "Real GDP growth (Annual percent change)") %>% 
    mutate_at(1, as.factor) %>%
    mutate_at(2:46, as.numeric)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Title",
    tabPanel("Model",
             fluidPage(
                 
                 # Application title
                 titlePanel("IMF Real GDP Growth Data in 1980"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("bins",
                                     "Number of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 30)
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("distPlot")
                     )
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
               https://github.com/dschang116/Final-Project."),
             h3("About Me"),
             p("My name is Derek Chang, and I am a first-year undergraduate at 
             Harvard University studying Economics.
             You can reach me at dschang@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- imf_rGDP_growth[[2]]
        
        bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), 
                    length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
