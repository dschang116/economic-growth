# Read in datasets created from 'data_cleaning' script.

ten_econ <- readRDS("ten_econ.RDS")
state_metrics <- readRDS("state_metrics.RDS")
us_by_year <- readRDS("us_by_year.RDS")
weekly <- readRDS("weekly.RDS")

d_state_metrics <- as_tibble(state_metrics) %>%
  filter(dem_gov == "1") %>%
  mutate(state = tools::toTitleCase(state))

r_state_metrics <- as_tibble(state_metrics) %>%
  filter(dem_gov == "0")%>%
  mutate(state = tools::toTitleCase(state))

# Define server side logic 

server <- function(input, output) {
  
  ############## FIRST TAB ##############

  output$us_trend_plot = renderPlot({
    measureSelected <- input$measure
    
    measureCol <- case_when(
      measureSelected == "Real GDP Growth" ~ us_by_year$rgdp_cap_gr,
      measureSelected == "Real GDP per Capita" ~ us_by_year$rgdp_cap,
      measureSelected == "Trade Balance" ~ us_by_year$trade_balance,
      measureSelected == "Corporate Profits After Tax" ~ us_by_year$profits,
      measureSelected == "S&P 500 Index" ~ us_by_year$close,
      measureSelected == "Patents Granted" ~ us_by_year$tot_patents,
      measureSelected == "Mergers and Acquistions" ~ us_by_year$value_m_a
    )
    
    y_lab_suffix <- case_when(
      measureSelected == "Real GDP Growth" ~ "(%)",
      measureSelected == "Real GDP per Capita" ~ "($)",
      measureSelected == "Trade Balance" ~ "(Billions of $)",
      measureSelected == "Corporate Profits After Tax" ~ "(Billions of $)",
      measureSelected == "S&P 500 Index" ~ "",
      measureSelected == "Patents Granted" ~ "",
      measureSelected == "Mergers and Acquistions" ~ ""
    )
    
    us_by_year %>%
      ggplot(aes(x = obs_date, y = measureCol)) +
        geom_line(size = 1) +
        labs(x = "Year",
             y = paste(measureSelected, y_lab_suffix),
             title = paste(measureSelected, "From 1988 to 2018")) +
        scale_x_continuous(breaks = seq(from = 1990, to = 2018, by = 5))+
        scale_y_continuous(labels = scales::comma_format()) +
        theme_bw() +
        theme(plot.title = element_text(size = 20, face = "bold", color = "darkgreen"))
  })
  
  #######################################
    
  output$ten_econ_plot = renderPlot({
    countrySelected <- input$country
    
    ten_econ %>% filter(country %in% countrySelected) %>%
    pivot_longer(cols = -country,
                 names_to = "year",
                 values_to = "gdp") %>%
    ggplot(aes(x = year, y = gdp, group = country, color = country)) +
      geom_line() +
      labs (
        x = "Year",
        y = "GDP Growth (%)",
        color = "Country",
        title = paste(
          "GDP Growth for",
          paste(countrySelected, collapse = ', '),
          "in Recent Decades"
        )
      ) +
      scale_x_discrete(breaks = seq(1980, 2020, 5)) +
      theme_bw() +
      theme(plot.title = 
              element_text(size = 12, face = "bold", color = "darkgreen"))
  })
  
  ############## SECOND TAB ##############
  
  output$econ_regression_plot = renderPlot({
    methodSelected <- input$method
    xSelected <- input$x_var
    ySelected <- input$y_var

    x_var <- case_when(
      xSelected == "Real GDP Growth" ~ "rgdp_cap_gr",
      xSelected == "Real GDP per Capita" ~ "rgdp_cap",
      xSelected == "Trade Balance" ~ "trade_balance",
      xSelected == "Corporate Profits After Tax" ~ "profits",
      xSelected == "S&P 500 Index" ~ "close",
      xSelected == "Patents Granted" ~ "tot_patents",
      xSelected == "Mergers and Acquistions" ~ "value_m_a"
    )
    
    y_var <- case_when(
      ySelected == "Real GDP Growth" ~ "rgdp_cap_gr",
      ySelected == "Real GDP per Capita" ~ "rgdp_cap",
      ySelected == "Trade Balance" ~ "trade_balance",
      ySelected == "Corporate Profits After Tax" ~ "profits",
      ySelected == "S&P 500 Index" ~ "close",
      ySelected == "Patents Granted" ~ "tot_patents",
      ySelected == "Mergers and Acquistions" ~ "value_m_a"
    )
    
    x_lab_suffix <- case_when(
      xSelected == "Real GDP Growth" ~ "(%)",
      xSelected == "Real GDP per Capita" ~ "($)",
      xSelected == "Trade Balance" ~ "(Billions of $)",
      xSelected == "Corporate Profits After Tax" ~ "(Billions of $)",
      xSelected == "S&P 500 Index" ~ "",
      xSelected == "Patents Granted" ~ "",
      xSelected == "Mergers and Acquistions" ~ ""
    )
    
    y_lab_suffix <- case_when(
      ySelected == "Real GDP Growth" ~ "(%)",
      ySelected == "Real GDP per Capita" ~ "($)",
      ySelected == "Trade Balance" ~ "(Billions of $)",
      ySelected == "Corporate Profits After Tax" ~ "(Billions of $)",
      ySelected == "S&P 500 Index" ~ "",
      ySelected == "Patents Granted" ~ "",
      ySelected == "Mergers and Acquistions" ~ ""
    )
    
    model_method <- case_when(
      methodSelected == "Linear Model" ~ "lm",
      methodSelected == "Loess Model" ~ "loess"
    )
    
    us_by_year %>%
      ggplot(aes_string(x = x_var, y = y_var)) +
      geom_point() +
      geom_jitter() +
      geom_smooth(method = model_method,
                  level = 0.95,
                  formula = y ~ x) +
      labs(title = "U.S Economic and Policy Metrics",
           subtitle = "Summarized yearly from 1988-2018",
           x = paste(xSelected, x_lab_suffix),
           y = paste(ySelected, y_lab_suffix),
           caption = "Sources: FRED, IMAA, USPTO, Yahoo! Finance, Macrotrends") +
      theme_classic() +
      theme(plot.title = 
              element_text(size = 20, face = "bold", color = "darkgreen"),
            plot.subtitle = 
              element_text(size = 15, face = "bold", color = "darkgreen"),
            plot.caption = element_text(face = "italic"))
  })
  
  #######################################
  
  output$econ_regression_summary <- renderPrint({
    methodSelected <- input$method
    xSelected <- input$x_var
    ySelected <- input$y_var
    
    x_var <- case_when(
      xSelected == "Real GDP Growth" ~ us_by_year$rgdp_cap_gr,
      xSelected == "Real GDP per Capita" ~ us_by_year$rgdp_cap,
      xSelected == "Trade Balance" ~ us_by_year$trade_balance,
      xSelected == "Corporate Profits After Tax" ~ us_by_year$profits,
      xSelected == "S&P 500 Index" ~ us_by_year$close,
      xSelected == "Patents Granted" ~ us_by_year$tot_patents,
      xSelected == "Mergers and Acquistions" ~ us_by_year$value_m_a
    )
    
    y_var <- case_when(
      ySelected == "Real GDP Growth" ~ us_by_year$rgdp_cap_gr,
      ySelected == "Real GDP per Capita" ~ us_by_year$rgdp_cap,
      ySelected == "Trade Balance" ~ us_by_year$trade_balance,
      ySelected == "Corporate Profits After Tax" ~ us_by_year$profits,
      ySelected == "S&P 500 Index" ~ us_by_year$close,
      ySelected == "Patents Granted" ~ us_by_year$tot_patents,
      ySelected == "Mergers and Acquistions" ~ us_by_year$value_m_a
    )
    
    if (methodSelected == "Linear Model")
      fit_model <- lm(y_var ~ x_var, us_by_year)
    
    if (methodSelected == "Loess Model")
      fit_model <- loess(y_var ~ x_var, us_by_year)
    
    # Print a summary of the model.
    print(summary(fit_model))
  })
  
  ############## THIRD TAB ##############
  
  output$covid_us_map = renderPlot({
    metricSelected <- input$metric
    
    d_metricCol <- case_when(
      metricSelected == "Weekly Initial Claims" ~ d_state_metrics$tot_claims,
      metricSelected == "Unemployment Rate" ~ d_state_metrics$unemp_rate,
      metricSelected == "Total Positive Cases" ~ d_state_metrics$tot_pos,
      metricSelected == "Positive Rate" ~ d_state_metrics$pos_rate,
      metricSelected == "Tests Per Thousand" ~ d_state_metrics$test_per_1000,
      metricSelected == "Deaths Per Thousand" ~ d_state_metrics$death_per_1000
    )
    
    r_metricCol <- case_when(
      metricSelected == "Weekly Initial Claims" ~ r_state_metrics$tot_claims,
      metricSelected == "Unemployment Rate" ~ r_state_metrics$unemp_rate,
      metricSelected == "Total Positive Cases" ~ r_state_metrics$tot_pos,
      metricSelected == "Positive Rate" ~ r_state_metrics$pos_rate,
      metricSelected == "Tests Per Thousand" ~ r_state_metrics$test_per_1000,
      metricSelected == "Deaths Per Thousand" ~ r_state_metrics$death_per_1000
    )
    
    plot_title <- case_when(
      metricSelected == "Weekly Initial Claims" ~ 
        "Total Weekly Initial Claims (1/4/2020 to 10/31/2020)",
      metricSelected == "Unemployment Rate" ~ 
        "Average Unemployment Rate (July 2020 to September 2020)",
      metricSelected == "Total Positive Cases" ~ 
        "Total Positive Cases (3/6/2020 to 11/11/2020)",
      metricSelected == "Positive Rate" ~ 
        "Positive Rate from Total Tests (3/6/2020 to 11/11/2020)",
      metricSelected == "Tests Per Thousand" ~ 
        "Tests Per Thousand Based on State Population (3/6/2020 to 11/11/2020)",
      metricSelected == "Deaths Per Thousand" ~ 
        "Deaths Per Thousand Based on State Population (3/6/2020 to 11/11/2020)"
    )
    
    ggplot() + 
      geom_polygon(data = d_state_metrics, aes(x = long, y = lat, 
                                            fill = d_metricCol, group = group), 
                   color = "blue") + 
      geom_polygon(data = r_state_metrics, aes(x = long, y = lat, 
                                            fill = r_metricCol, group = group), 
                   color = "red") + 
      coord_fixed(1.5) +
      scale_fill_gradient(low = "white", high = "black", 
                          label = scales::comma) +
      theme_void() +
      labs(title = plot_title) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(size = 20, face = "bold", color = "darkgreen"))
  })
  
  #######################################
  
  output$d_state_metrics_table <- renderDataTable({
    datatable(
      d_state_metrics %>%
        select(
          "State" = state,
          "Total Initial Claims" = tot_claims,
          "Unemp Rate %" = unemp_rate,
          "Total Positive" = tot_pos,
          "Positive Rate %" = pos_rate,
          "Test per 1000" = test_per_1000,
          "Death per 1000" = death_per_1000
        ) %>%
        distinct() %>%
        mutate(across(is.numeric, ~ round(., 2))),
      rownames = FALSE,
      options = list(lengthChange = FALSE)
    ) %>%
      formatCurrency(c(2,4), currency = "", mark = ",", digits = 0)
  })
  
  #######################################
  
  output$r_state_metrics_table <- renderDataTable({
    datatable(
      r_state_metrics %>%
        select(
          "State" = state,
          "Total Initial Claims" = tot_claims,
          "Unemp Rate %" = unemp_rate,
          "Total Positive" = tot_pos,
          "Positive Rate %" = pos_rate,
          "Test per 1000" = test_per_1000,
          "Death per 1000" = death_per_1000
        ) %>%
        distinct() %>%
        mutate(across(is.numeric, ~ round(., 2))),
      rownames = FALSE,
      options = list(lengthChange = FALSE)
    ) %>%
    formatCurrency(c(2,4), currency = "", mark = ",", digits = 0)
  })
  
  ############## FOURTH TAB ##############
  
  output$wei_posterior = renderPlot({
    weekSelected <- input$week
    deathsSelected <- input$deaths
    claimsSelected <- input$claims
    
    fit_1 <- stan_glm(formula = index ~ week + new_deaths + claims,
                      data = weekly,
                      refresh = 0)
    
    new_obs <- tibble(week = weekSelected,
                      new_deaths = deathsSelected,
                      claims = claimsSelected)
    
    posterior_predict(fit_1, newdata = new_obs) %>%
      as_tibble() %>% 
      mutate_all(as.numeric) %>%
      rename(wei = `1`) %>% 
      ggplot(aes(x = wei, y = after_stat(count/sum(count)))) +
      geom_histogram(bins = 100) +
      labs(title = "Posterior Predictive Distribution",
            subtitle = paste("For week", weekSelected,
                             "new deaths", deathsSelected, 
                             "weekly claims", claimsSelected),
            x = "Weekly Economic Index (WEI)",
            y = "Probability") + 
      scale_x_continuous(labels = scales::number_format()) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_classic() +
      theme(plot.title = 
              element_text(size = 20, face = "bold", color = "darkgreen"),
            plot.subtitle = 
              element_text(size = 15, face = "bold", color = "darkgreen"))
  })
  
  #######################################
}

















