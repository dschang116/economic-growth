############## PREP ##############

library(shiny)
library(tidyverse)
library(shinythemes)

# Read in datasets created using the 'data_clearning' script.

ten_econ <- readRDS("ten_econ.RDS")
state_metrics <- readRDS("state_metrics.RDS")
us_yr <- readRDS("us_yr.RDS")

# Ten biggest economies and order alphabetically
countries <- c("Brazil", "Canada", "China", "France", 
               "Germany", "India", "Italy", "Japan", 
               "United Kingdom", "United States") %>% 
  sort()

# Define metrics.

metrics <- c("First-Time Unemployment Claims", "Average Percent Unemployed (July-September)", 
             "Deaths Per Thousand", "Total Positive Cases", "Tests Per Thousand", "Positivity Rate")

# Define trends.

trends <- c("Real GDP per Capita", "Real GDP per Capita Growth (Yearly)")

shinyUI(
  navbarPage(
    theme = shinytheme("cerulean"),
    "U.S. Economic Growth: A Look Back and a Look Forward",
    
    ############## FIRST PAGE ##############
    
    tabPanel(
      "A Snapshot",
      h2("U.S. Economic Trends Over the Last Three Decades"),
      p(
        "The economy is one of the foundation of American society, an indicator of the nation's
        prosperity and wealth. It touches every aspect of our lives, and economic policies can 
        have reverbating factors across the country. In the past three decades, the American 
        people have lived through four recesssions, with the Great Recession (2007-2009) and the 
        present COVID-19 recession as the ones that have defined the era. 
        In this project, we aim to build an understanding of the current economic environment admist 
        COVID-19 and the policies that are most effective in spurring growth going forward thorugh an 
        examination of America's recent economic history. By analyzing economic trends over the last 
        three decades, the consequencies of government and regulatory action (or inaction) come to 
        light."
        ),
      
      # Generate a 2x2 column/row layout.
      fluidPage(
       
        fluidRow(
          column(8,
               
               # Plot Leaflet map.
               
               # Sidebar with a slider input for number of bins 
               sidebarLayout(
                 sidebarPanel(
                   selectInput("trend", "Pick a measure:", 
                               choices = list("Real GDP per Capita", 
                                              "Real GDP per Capita Growth (Yearly)",
                                              "Corporate Profits After Tax",
                                              "S&P 500 Index",
                                              "Patents Granted",
                                              "Mergers and Acquistions",
                                              "Trade Balance"))
                   ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(plotOutput("us_trends")),
                 
               )),
          
          column(
            4,
            h3("Variabiliy in Different Economic Measures and Policy Actions"),
            p(
              "As can be seen from the graph on the right, there has been a great amount of 
            variabiliy in different economic measures and policy actions. Overall, many
            indicators of growth are upward trending, a sign that government and regulatory
            actions have, at the very least, not had a hindering impact. In this timeframe, the
            economic system of the U.S has generally proven to be successful, thoough
            it has its shortcomings."
            )
          )
        ),
            
        fluidRow(style = 'margin-top:5em',
            column(
              4,
              h3("U.S. and the World"),
              p(
                "Being the world's largest economy has given America a prominent role in 
                       the global economy. Evaluating GDP growth, an important sign of the overall 
                       health and direction of a nation's economy, can help contexualize this position.
                       As the graph on the left shows, the growth of advanced economies (those 
                       other than Brazil, China, and India) seem strongly correlated with the U.S.'s,
                       even similar in magnitude over this time period. Although the growth of the
                       developing economies (Brazil, China, India) do not align as closely with US's
                       as the advanced economies, there is still a noticeable relation. 
                       Furthering our understanding of the U.S. economy is essential to ensure the 
                       proper policies are implemenented to boost American proseprity, and in turn,
                       the world's."
              )
            ),
        
             column(8,
                    
                    # Plot histogram of water conflict events over time.
                    
                    sidebarLayout(
                      sidebarPanel(
                        checkboxGroupInput("country", 
                                           ("Pick a country:"), 
                                           choiceNames =
                                             countries,
                                           choiceValues =
                                             countries,
                                           selected = c("United States", "United Kingdom")
                        )),
                      
                      # Show a plot of the generated distribution
                      mainPanel(plotOutput("ten_econ_plot")),
                      
                    ))
        )
      )),
    
    ############################
    
    ############## SECOND TAB ##############
    
    tabPanel(
      "Growth Trend",
      h2("U.S. Economy Growth Trend"),
      p(
        "This is to take a look U.S. economy growth trend. Have a model to do analysis"
      )
    ),
    ############################
    
    ############## Third PAGE ##############
    
    tabPanel(
      "Covid Impact",
      h2("Metrics for U.S. States"),
      p(
        "This is to take a closer look at U.S. and each state's metrics. Have a model"
      ),
      fluidPage(
        
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
    ############################
    
    ############## FOURTH PAGE ##############
    
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    
    ############################
    
    
    ############## FIFTH PAGE ##############
    
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
             You can reach me at dschang@college.harvard.edu."))
    
    ############################
    
    
  )
)
