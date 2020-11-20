############## PREP ##############

library(shiny)
library(tidyverse)
library(ggplot2)
#library(sjPlot)

# Read in datasets created using the 'data_clearning' script.

ten_econ <- readRDS("ten_econ.RDS")
state_metrics <- readRDS("state_metrics.RDS")
us_yr <- readRDS("us_yr.RDS")

d_state_metrics <- as_tibble(state_metrics) %>% 
  filter(DemGov == "1")

r_state_metrics <- as_tibble(state_metrics) %>% 
  filter(DemGov == "0")

# Define server logic required to draw a 
server <- function(input, output) {
  
  output$us_trends = renderPlot({
    trendSelected <- input$trend
    
    trendCol <- case_when(
      trendSelected == "Real GDP per Capita" ~ us_yr$rgdp_cap,
      trendSelected == "Real GDP per Capita Growth (Yearly)" ~ us_yr$rgdp_cap_gr,
      trendSelected == "Corporate Profits After Tax" ~ us_yr$profits,
      trendSelected == "S&P 500 Index" ~ us_yr$close,
      trendSelected == "Patents Granted" ~ us_yr$tot_patents,
      trendSelected == "Mergers and Acquistions" ~ us_yr$value_m_a,
      trendSelected == "Trade Balance" ~ us_yr$trade_deficit_perc
    )
    
    
    ggplot(data = us_yr, aes(x = obs_date, y = trendCol)) +
      geom_line(size = 1) +
      labs(y = trendSelected,
           x = "Year",
           title = "U.S. Growth Over Years") +
      scale_y_continuous(labels = scales::number_format()) +
      theme_bw()
  })
  
  output$ten_econ_plot = renderPlot({
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
      scale_x_discrete(breaks = seq(1980, 2020, 5)) + 
      theme_bw() 
      # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$covid_us_map = renderPlot({
    metricSelected <- input$metric
    
    d_metricCol <- case_when(
      metricSelected == "First-Time Unemployment Claims" ~ d_state_metrics$tot_claims,
      metricSelected == "Average Percent Unemployed (July-September)" ~ d_state_metrics$avg_perc, 
      metricSelected == "Deaths Per Thousand" ~ d_state_metrics$deaths_thous, 
      metricSelected == "Total Positive Cases" ~ d_state_metrics$tot_pos, 
      metricSelected == "Tests Per Thousand" ~ d_state_metrics$tests_thous, 
      metricSelected == "Positivity Rate" ~ d_state_metrics$pos_rate
    )
    
    r_metricCol <- case_when(
      metricSelected == "First-Time Unemployment Claims" ~ r_state_metrics$tot_claims,
      metricSelected == "Average Percent Unemployed (July-September)" ~ r_state_metrics$avg_perc, 
      metricSelected == "Deaths Per Thousand" ~ r_state_metrics$deaths_thous, 
      metricSelected == "Total Positive Cases" ~ r_state_metrics$tot_pos, 
      metricSelected == "Tests Per Thousand" ~ r_state_metrics$tests_thous, 
      metricSelected == "Positivity Rate" ~ r_state_metrics$pos_rate
    )
    
    ggplot() + 
      geom_polygon(data = d_state_metrics, aes(x = long, y = lat, fill = d_metricCol, group = group), 
                   color = "blue") + 
      geom_polygon(data = r_state_metrics, aes(x = long, y = lat, fill = r_metricCol, group = group), 
                   color = "red") + 
      coord_fixed(1.5) +
      scale_fill_gradient(low = "white", high = "black", label = scales::comma) +
      theme_bw() +
      labs(title = metricSelected) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.title = element_blank(),
            legend.background = element_rect(fill="gray",
                                             size=0.5, linetype="solid", 
                                             color ="darkblue"))
    
  })
  
  output$econ_regression = renderPlot({
    methodSelected <- input$method
    xSelected <- input$x_var
    ySelected <- input$y_var
    
    p <- us_yr %>%
      ggplot(aes_string(x = xSelected, y = ySelected)) +
      geom_point() +
      theme_classic() +
      geom_jitter() +
      labs(title = "U.S Economic and Policy Metrics",
           subtitle = "Summarized yearly from 1988-2018")
    
    if (xSelected == "trade_deficit_perc")
      p <- p + xlab("Trade Balance")
    
    if (ySelected == "rgdp_cap")
      p <- p + ylab("GDP per Capita")
    
    
    if (methodSelected == "Linear Model")
      p <- p + geom_smooth(method = "lm",
                           se = TRUE,
                           formula = y ~ x)
    if (methodSelected == "Loess Model")
      p <- p + geom_smooth(method = "loess",
                           se = TRUE,
                           formula = y ~ x)
    p
  })
  
  #.
  output$RegSum <- renderPrint({
    methodSelected <- input$method
    xSelected <- input$x_var
    ySelected <- input$y_var
    
    if (methodSelected == "Linear Model")
      model_summ <-
        reactive({
          lm(reformulate(xSelected, ySelected),
             data = us_yr)
        })

    if (methodSelected == "Loess Model")
      model_summ <-
        reactive({
          lm(
            reformulate(xSelected, ySelected),
            data = us_yr,
            method = "qr"
          )
        })
    
    # Print a summary of the model. 
    
    print(summary(model_summ()))
  })
}