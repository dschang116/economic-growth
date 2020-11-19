############## PREP ##############

library(shiny)
library(tidyverse)
library(ggplot2)

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
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$us_trends = renderPlot({
    trendSelected <- input$trend
    
    trendCol <- case_when(
      trendSelected == 1 ~ us_yr$rgdp_cap,
      trendSelected == 2 ~ us_yr$rgdp_cap_gr,
      trendSelected == 3 ~ us_yr$profits,
      trendSelected == 4 ~ us_yr$close,
      trendSelected == 5 ~ us_yr$tot_patents,
      trendSelected == 6 ~ us_yr$value_m_a,
      trendSelected == 7 ~ us_yr$trade_deficit_perc
    )
    
    ggplot(data = us_yr, aes(x = obs_date, y = trendCol)) +
      geom_line()
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
      labs(fill = metricSelected) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
  })
}