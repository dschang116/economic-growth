############## PREP ##############

library(tidyverse)
library(readxl)
require(ggplot2)
library(lubridate)

############## Cleaning Events ##############

# Read in IMF rGDP data. Rename first column to country 
# Drop NA values.

imf_rgdp <- read_excel("raw_data/imf-rgdp.xls") %>%
  rename(country = "Real GDP growth (Annual percent change)") %>%
  drop_na()

# Replace values of no data with NA.

imf_rgdp[imf_rgdp == "no data"] <- NA

# Rename China.

imf_rgdp$country[imf_rgdp$country == 
                       "China, People's Republic of"] <- "China"

# Ten biggest economies.
countries <- c("Brazil", "Canada", "China", "France", 
               "Germany", "India", "Italy", "Japan", 
               "United Kingdom", "United States") 

# Filter and change first column country as factor and rest as numeric column
# type.

ten_econ <- imf_rgdp %>% 
  filter(country %in% countries) %>% 
  mutate_at(1, as.factor) %>%
  mutate_at(2:47, as.numeric) %>% 
  select(c(1:42))

# Save the resulting data for use in Shiny App.

saveRDS(ten_econ, "ten_econ.RDS")

# Read in state population data.

state_pop <- read_csv("raw_data/state_pop.csv", col_types = cols(
  .default = col_double(),
  SUMLEV = col_character(),
  REGION = col_character(),
  DIVISION = col_character(),
  STATE = col_character(),
  NAME = col_character()
)) %>% 
  select(NAME, POPESTIMATE2019) %>% 
  rename(region = NAME,
         pop = POPESTIMATE2019) %>% 
  slice(-(1:5)) %>% 
  filter(! region %in% c("District of Columbia", "Puerto Rico")) %>% 
  mutate(region = tolower(region))
  

# Read in state first time unemployment claims data.

unemploy_state <- read_excel("raw_data/unemploy-state.xlsx") %>% 
  rename(region = "State",
         wk_ended = `Filed week ended`,
         in_claims = `Initial Claims`) %>% 
  select(region, wk_ended, in_claims) %>% 
  group_by(region) %>% 
  summarize(tot_claims = sum(in_claims), 
            .groups = "drop") %>%
  mutate(region = tolower(region))

# Read in state unemployment rate data.

unemploy_perc_state <- read_excel("raw_data/unemploy-perc-state.xlsx") %>% 
  mutate(avg_perc = rowMeans(.[2:4])) %>% 
  rename(region = "State")%>%
  mutate(region = tolower(region))

# Read in U.S. State and governor

us_states <- read_csv("raw_data/us-state-govs.csv",
                      col_types = cols(StateName = col_character(),
                                       StateAbbr = col_character(),
                                       DemGov = col_number())) %>%
  mutate(StateName = tolower(StateName))

# Read in states COVID-19 data.

covid <- read_csv("raw_data/states-covid.csv", 
                  col_types = cols(
                    .default = col_double(),
                    date = col_date(format = ""),
                    state = col_character(),
                    dataQualityGrade = col_character()
                  )) %>% 
  filter(! state %in% c("AS", "DC", "GU", "MP", "PR", "VI")) %>% 
  select(state, death, positive, totalTestResults)

# Replace NA value with 0.

covid[is.na(covid)] <- 0

covid <- us_states %>% 
  inner_join(covid, by = c("StateAbbr" = "state"))

# Summarize 

covid_summ <- covid %>%
  group_by(StateName) %>% 
  summarize(tot_death = max(death),
            tot_pos = max(positive),
            tot_test = max(totalTestResults),
            pos_rate = tot_pos / tot_test,
            .groups = "drop") %>%
  mutate(StateName = tolower(StateName)) %>%
  rename(region = StateName) %>% 
  inner_join(state_pop, by = "region") %>% 
  mutate(deaths_thous = tot_death * 1000 / pop,
         tests_thous = tot_test * 1000 / pop) %>% 
  select(region, pos_rate, deaths_thous, tot_pos, tests_thous)

# Get U.S. states from ggmap map_data

states <- map_data("state")

# Perform join.

state_metrics <- states %>% 
  inner_join(unemploy_state, by = "region") %>% 
  inner_join(unemploy_perc_state, by = "region") %>% 
  inner_join(covid_summ, by = "region") %>% 
  inner_join(us_states, by = c("region" = "StateName")) %>% 
  filter(region != "District of Columbia") %>% 
  select(region, long, lat, group, tot_claims, avg_perc,
         deaths_thous, pos_rate, tot_pos, tests_thous, DemGov)

# Save the resulting data for use in Shiny App.

saveRDS(state_metrics, "state_metrics.RDS")

# Read in rGDP per capita for the U.S.

us_rgdp_cap <- read_excel("raw_data/rgdp-cap.xls") %>% 
  slice(-c(1:10)) %>%
  rename(obs_date = `FRED Graph Observations`,
         rgdp_cap = ...2) %>% 
  mutate(obs_date = 1947:2020) %>% 
  slice(42:72)

# Read in rGDP per capita growth for the U.S.

us_rgdp_cap_growth <- read_excel("raw_data/rgdp-cap-growth.xls") %>%
  slice(-c(1:10)) %>%
  rename(obs_date = `FRED Graph Observations`,
         rgdp_cap_gr = ...2) %>% 
  mutate(obs_date = 1948:2020) %>% 
  slice(41:71)

# Read in purchasing power parity converted GDP per capita for the U.S.

corp_profits <- read_excel("raw_data/corp-profits.xls") %>% 
  slice(-c(1:10)) %>%
  rename(obs_date = `FRED Graph Observations`,
         profits = ...2) %>% 
  mutate(obs_date = 1947:2020) %>% 
  slice(42:72)

# Read in S&P 500 Index historical data.

sp500 <- read_csv("raw_data/sp500.csv", col_types = cols(
  Date = col_date(format = ""),
  Open = col_double(),
  High = col_double(),
  Low = col_double(),
  Close = col_double(),
  `Adj Close` = col_double(),
  Volume = col_double()
)) %>% 
  mutate(month = month(Date), 
         obs_date = year(Date),
         close = Close) %>% 
  group_by(obs_date) %>% 
  filter(row_number() == n()) %>% 
  select(obs_date, close) %>% 
  ungroup() %>% 
  slice(62:92)


# Read in patents data.

patents_granted <- read_excel("raw_data/patents.xlsx") %>% 
  slice(5:35) %>% 
  select(Utility...5, Design...7, Plant...8) %>% 
  rename(utility = Utility...5,
         design = Design...7,
         plant = Plant...8) %>% 
  mutate(utility = as.numeric(utility),
         design = as.numeric(design),
         plant = as.numeric(plant)) %>% 
  cbind(., tot_patents = rowSums(.)) %>% 
  mutate(obs_date = 1988:2018,
         tot_patents = rev(tot_patents)) %>% 
  select(obs_date, tot_patents)

# Read in mergers and acquistions data.

m_a <- read_excel("raw_data/m-a.xlsx") %>% 
  slice(-1) %>% 
  select(Value) %>% 
  rename(value_m_a = Value) %>% 
  mutate(obs_date = 1985:2020) %>% 
  slice(4:34) %>% 
  select(obs_date, value_m_a)

# Read in trade balance data.

trade_balance <- read_excel("raw_data/trade-balance.xlsx") %>% 
  mutate(month = month(date), 
         obs_date = year(date)) %>% 
  rename(trade_deficit_perc = "Percent") %>% 
  select(obs_date, trade_deficit_perc) %>% 
  slice(19:49)

# Perform join.

us_yr <- us_rgdp_cap %>% 
  inner_join(us_rgdp_cap_growth, by = "obs_date") %>%
  inner_join(corp_profits, by = "obs_date") %>% 
  inner_join(sp500, by = "obs_date") %>% 
  inner_join(patents_granted, by = "obs_date") %>% 
  inner_join(m_a, by = "obs_date") %>% 
  inner_join(trade_balance, by = "obs_date") %>% 
  mutate(rgdp_cap = as.numeric(rgdp_cap),
         rgdp_cap_gr = as.numeric(rgdp_cap_gr),
         profits = as.numeric(profits),
         value_m_a = as.numeric(value_m_a))

saveRDS(us_yr, "us_yr.RDS")



