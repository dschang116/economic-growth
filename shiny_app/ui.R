library(shiny)
library(tidyverse)
library(shinythemes)
library(lubridate)
library(DT)
library(rstanarm)
library(tidymodels)

# Read in datasets created from 'data_cleaning' script.

ten_econ <- readRDS("ten_econ.RDS")
state_metrics <- readRDS("state_metrics.RDS")
us_by_year <- readRDS("us_by_year.RDS")
weekly <- readRDS("weekly.RDS")

# Define economic measure list

measure <- list(
  "Real GDP Growth",
  "Real GDP per Capita",
  "Trade Balance",
  "Corporate Profits After Tax",
  "S&P 500 Index",
  "Patents Granted",
  "Mergers and Acquistions"
  )

# Define ten largest economies then order alphabetically

countries <- c(
  "Brazil",
  "Canada",
  "China",
  "France",
  "Germany",
  "India",
  "Italy",
  "Japan",
  "United Kingdom",
  "United States"
) %>%
  sort()

# Define model methods

methods <- c(
  "Linear Model", 
  "Loess Model")

# Define economic metrics

metrics <- c(
  "Weekly Initial Claims",
  "Unemployment Rate",
  "Total Positive Cases",
  "Positivity Rate",
  "Tests Per Thousand",
  "Deaths Per Thousand")

# Define UI for app

shinyUI(
  navbarPage(
    theme = shinytheme("cerulean"),
    "U.S. Economic Growth: A Look Back and a Look Forward",
    
    ############## FIRST TAB ##############
    
    tabPanel(
      "A Snapshot",
      h2("Economic Trends Over the Last Three Decades"),
      p(
        "The economy is one of the foundations of American society, an indicator of the nation's
        prosperity and wealth. It touches every aspect of our lives, and economic policies can
        have reverberating factors across the country. In the past three decades, the American
        people have lived through four recessions, with the Great Recession (2007-2009) and the
        present COVID-19 recession as the ones that have defined the era."),
      
      p("In this project, we aim to build an understanding of the current economic environment amidst
        COVID-19 and the policies that are most effective in spurring growth going forward through an
        examination of America's recent economic history. By analyzing economic trends over the last
        three decades, the consequences of government and regulatory action (or inaction) come to
        light."),
      
      br(),
      
      fluidPage(
        
        # 2x2 column/row layout
        
        fluidRow(
      
          # Row 1 Column 1
          
          column(
            8,
            
            # Plot us_trend_plot
            
            sidebarLayout(sidebarPanel(
              selectInput(
                "measure",
                "Pick a measure:",
                choices = measure
              )
            ),
          
            mainPanel(plotOutput("us_trend_plot")))
          ),
        
          # Row 1 Column 2
        
          column(
            4,
            h3("Variability in Different Economic Measures and Policy Actions"),
            p(
              "As can be seen from the graph on the left, there has been a great amount of
              variability in different economic measures and policy actions."),
            
            p("Overall, many
              indicators of growth are upward trending, a sign that government and regulatory
              actions have, at the very least, not had a hindering impact. In this timeframe, the
              economic system of the U.S. has generally proven to be successful, though
              it has its shortcomings.")
          )
        ),
        
        fluidRow(style = 'margin-top:3em',
                 
          # Row 2 Column 1
                 
          column(
            4,
            h3("U.S. and the World"),
            p(
             "Being the world's largest economy has given America a prominent role in
               the global economy. Evaluating GDP growth, an important sign of the overall
               health and direction of a nation's economy, can help contextualize this position."),
            
            p("As the graph on the right shows, the growth of advanced economies (those
               other than Brazil, China, and India) seem strongly correlated with the U.S.'s,
               even similar in magnitude over this time period. Although the growth of the
               developing economies (Brazil, China, India) do not align as closely with US's
               as the advanced economies, there is still a noticeable relation."),
            
            p("Furthering our understanding of the U.S. economy is essential to ensure the
               proper policies are implemented to boost American prosperity, and in turn,
               the world's.")
          ),
                 
          # Row 2 Column 2
          
          column(
            8,
            
            # Plot ten_econ_plot
            
            sidebarLayout(sidebarPanel(
             checkboxGroupInput(
               "country",
               ("Pick a country:"),
               choiceNames =
                 countries,
               choiceValues =
                 countries,
               selected = c("United States", "United Kingdom")
             )
            ),
            
            mainPanel(plotOutput("ten_econ_plot")))
          )
        )
      )
    ),
    
    ############## SECOND TAB ##############
    
    tabPanel(
      "Economic Trends",
      h2("Finding Connections"),
      
      p(
        "The Federal Reserve Economic Data (FRED), along with datasets from the Institute 
        for Mergers, Acquisitions and Alliances (IMAA), the U.S. Patent and Trademark 
        Office (USPTO), Yahoo! Finance, and Macrotrends, have an abundance of annualized economic 
        data from 1988-2018 that can be used to draw conclusions about correlations between different 
        trends."),
      
      p("Of particular interest are the relations among consumer welfare, private sector 
        performance, and government policies. Representative measures of the first include", strong("Real GDP 
        Growth"), "and", strong("Real GDP per Capita."), "Business environments are captured by", 
        strong("Corporate Profits after Tax, the S&P 500 Index, and the Number of Patents Granted;"), 
        "all are indicators of the level of productivity and intellectual innovation. The 
        public sector plays an important role in impacting the", strong("Trade Balance"), 
        "as well as", strong("Mergers and Acquisitions,"), " both of which are parameters we can 
        examine to understand implications of different public policies through a predictive lense.
        Ultimately, we can perform regressions to draw predictive conclusions and analyze different 
        hypotheses."),
      
      p("Note that in some instances, there is a fairly weak or ambiguous relation between a pair of parameters,
        as can be shown in the graph and table; this is not unexpected given the complexity of the modern 
        economic system. Such a trend could be an indication of the lack of correlation between the two 
        parameters, an important result to consider in the world of policy."),
      
      br(),
      
      sidebarLayout(
        sidebarPanel(
          h4("Construct the Model:"),
          
          # Model X variable
          
          selectInput(
            "x_var",
            "X Variable:",
            choices = measure,
            multiple = FALSE,
            selected = "Trade Balance"
          ),
          
          # Model Y variable
          
          selectInput(
            "y_var",
            "Y Variable:",
            choices = measure,
            selected = "Real GDP per Capita"
          ),
          
          # Model method
          radioButtons(
            "method",
            ("Method:"),
            choiceNames = methods,
            choiceValues = methods
          )
        ),
        
        mainPanel(
          plotOutput("econ_regression_plot"),
          br(),
          HTML("<strong>Fitting Model Summary:</strong>"),
          verbatimTextOutput(outputId = "econ_regression_summary")
        )
      )
    ),
    
    ############## THIRD TAB ##############
    
    tabPanel(
      "Covid Impact",
      h2("Economic Metrics for U.S. States"),
      p(
        "We now turn to understand the economy in the era of COVID-19. The Trump Administration's response
        to the pandemic has largely been a decentralized approach in respect to public policy, 
        granting the governors much authority over individual states' actions and measures."),
      
      p("The map below shows the different challenges and responses of each governor faced, 
        as well as his or her relative success both in terms of managing the pandemic and 
        minimizing negative economic effects. It is important to keep in mind the nuances 
        and uniqueness of each state and the challenges they face, as such considerations 
        may not be reflected in the map."),
      
      br(),
      
      fluidPage(fluidRow(column(
        12,
        
        # Sidebar
        sidebarLayout(
          sidebarPanel(
            radioButtons(
              "metric",
              ("Pick a metric:"),
              choiceNames =
                metrics,
              choiceValues =
                metrics,
              selected = "Weekly Initial Claims"
            ),
            width = 3
          ),
          
          # Show a plot of the generated distribution
          mainPanel(plotOutput("covid_us_map"), width = 9)
        )
      )),
      
      fluidRow(
        column(
          6,
          h3("States with Democratic Governors"),
          
          # Plot data table
          dataTableOutput("d_state_metrics_table")
        ),
        column(
          6,
          h3("States with Repulican Governors"),
          
          # Plot data table
          dataTableOutput("r_state_metrics_table")
        )
      ))
    ),
    
    ############## FOURTH TAB ##############
    
    tabPanel(
      "Growth in a Pandemic",
      titlePanel("A Forecast of Economic Health"),
      
      p("The Weekly Economic Index (WEI) is an index of inflation-adjusted economic activity
        composed of ten different daily and weekly measures that capture consumer behavior, 
        labor market trends, and overall productivity. The WEI is scaled to the yearly
        GDP growth rate (if the WEI reads -2 percent and this level 
        persists for an entire quarter, we would expect the average GDP that 
        quarter to be 2 percent lower compared to the previous year)."),
        
      p("The WEI can be used to assess rapidly changing economic conditions and the overall
        health of the U.S. economy amidst the pandemic. Use the dashboard to adjust the parameters of 
        the model to predict the WEI of a week after March 1, 2020.", strong("Weeks Since March 1"), 
        "is a relatively straightforward measure, with 1 representing the week of March 1 and 
        the week of November 14 as the default selection.", strong("New Deaths (Weekly)"),
        "is the number of new deaths nationally in a certain week as a result of COVID-19.", strong("Initial
        Unemployment Claims (Weekly)"), "reports the number of people making first-time unemployment
        claims, representative of labor market conditions. With these three parameters,
        we can create a distribution of the likely values for the WEI."),
      
      br(),
        
      # Sidebar
      sidebarLayout(
        sidebarPanel(
          sliderInput("week", "Weeks since March 1",
                      min = 1, max = 100, value = 38),
          sliderInput("deaths", "New Deaths (Weekly)",
                      min = 0, max = 25000, value = 10083),
          sliderInput("claims", "Initial Unemployment Claims (Weekly)",
                      min = 200000, max = 6000000, value = 778000)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput("wei_posterior"),
                  fluidPage(
                    withMathJax(),
                    helpText("The formula is WEI = -3.2674801459 + 0.0676051920 * week - 
                             0.0005422682 * new_deaths - 0.0000004916 * claims."),
                    helpText("The 95% confidence interval for the WEI is shown below:")),
                  verbatimTextOutput(outputId = "confidence_interval")
        )
      )
    ),
    
    ############## FIFTH TAB ##############
    
    tabPanel(
      "About",
      titlePanel("About"),
      h3("Background and Motivations"),
      p(
        "The pandemic has brought to the forefront of national discussion the difficulties and importance of
        maintaining a strong economy amidst a public health crisis. This project seeks to contextualize
        the COVID-19 economic situation with insights from the last three decades and a state-by-state
        framework for analyzing the U.S.'s response. In doing so, we can find public policies
        that have had a positive impact on overall growth and wealth, as well as notice relations 
        between different measures (economic and COVID-19 related) suggestive of proper policy
        moving forward. With this background understanding of the economy and the impact of the pandemic, 
        we turn to make predictions about growth for the next year and when the economy 
        is expected to recover to the pre-pandemic level."
      ),
      p(
        "There are many challenges that lie ahead given the unpredictable nature and scarring effects 
        of the pandemic. This project aims to develop a framework and timeline for economic recovery 
        primarily dependent on the country's approach to managing the pandemic and the approval of an 
        effective vaccine."
      ),
      p(
        "This project's GitHub repository lives 
        ",
        a("here", href = "https://github.com/dschang116/economic-growth"),
        "."
      ),
      h3("The Data"),
      p(
        "This project incorporates data from a variety of sources:", 
        tags$ol(
          tags$li(
            a("The Federal Reserve Economic Data (FRED)", href = "https://fred.stlouisfed.org/"), 
            "provides data on U.S. corporate profits, initial unemployment claims, real GDP growth, and real 
            GDP per capita."
          ),
          tags$li(
            a("The COVID Tracking Project", href = "https://covidtracking.com/"), "provides data 
            on weekly national COVID-19 death counts and COVID-19 data for states."
          ),
          tags$li(
            a("Institute for Mergers, Acquisitions and Alliances (IMAA)", href = "https://imaa-institute.org/"), 
            "provides data on the yearly number
            of mergers and acquisitions."
          ),
          tags$li(
            a("U.S. Patent and Trademark Office (USPTO)", href = "https://www.uspto.gov/"), 
            "provides data on the number of 
            patents granted on an annual basis."
          ),
          tags$li(
            a("International Monetary Fund (IMF)", href = "https://www.imf.org/external/index.htm"), 
            "provides data on real GDP growth for the world's ten largest economies."
          ),
          tags$li(
            a("U.S. Bureau of Labor Statistics", href = "https://www.bls.gov/"), "provides data 
            on the unemployment rate for states."
          ),
          tags$li(
            a("U.S. Department of Labor", href = "https://www.dol.gov/"), "provides 
            data on initial unemployment claims for states."
          ),
          tags$li(
            a("The Federal Reserve Bank of New York", href = "https://www.newyorkfed.org/research/policy/weekly-economic-index#/overview"), 
            "provides data on the Weekly Economic Index (WEI)."
          ),
          tags$li(
            a("U.S. Census Bureau", href = "https://www.census.gov/"), "provides data 
            on state populations."
          ),
          tags$li(
            a("Yahoo! Finance", href = "https://finance.yahoo.com/"), "provides historical 
            data on the S&P 500 Index."),
          tags$li(
            a("Macrotrends", href = "https://www.macrotrends.net/"), "provides historical 
            data on the U.S. trade balance."
          ),
        )
      ),
      h3("About Me"),
      p(
        "My name is Derek Chang, and I am a first-year undergraduate at Harvard University studying Economics.
        You can reach me at", a("dschang@college.harvard.edu", href = "mailto:dschang@college.harvard.edu"),
        "."
      )
    )
  )
)