library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)

##########################################################

# Read in rGDP per capita for the U.S.

us_rgdp_cap <- read_excel("raw_data/rgdp-cap.xls", skip = 10) %>%
  rename(obs_date = "observation_date",
         rgdp_cap = "A939RX0Q048SBEA") %>%
  mutate(obs_date = 1947:2020) %>%
  slice(42:72)

# Read in rGDP per capita growth for the U.S.

us_rgdp_cap_growth <- read_excel("raw_data/rgdp-cap-growth.xls", skip = 10) %>%
  rename(obs_date = "observation_date",
         rgdp_cap_gr = "A939RX0Q048SBEA_PCH") %>%
  mutate(obs_date = 1948:2020) %>%
  slice(41:71)

# Read in purchasing power parity converted GDP per capita for the U.S.

corp_profits <- read_excel("raw_data/corp-profits.xls", skip = 10) %>%
  rename(obs_date = "observation_date",
         profits = "CP") %>%
  mutate(obs_date = 1947:2020) %>%
  slice(42:72)

# Read in S&P 500 Index historical data

sp500 <- read_csv(
  "raw_data/sp500.csv",
  col_types = cols(
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

# Read in patents data

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

# Read in mergers and acquisitions data

m_a <- read_excel("raw_data/m-a.xlsx") %>%
  slice(-1) %>%
  select(Value) %>%
  rename(value_m_a = Value) %>%
  mutate(obs_date = 1985:2020) %>%
  slice(4:34) %>%
  select(obs_date, value_m_a)

# Read in trade balance data

trade <- read_excel("raw_data/trade-balance.xlsx") %>%
  mutate(month = month(date),
         obs_date = year(date)) %>%
  rename(trade_balance = 2) %>%
  select(obs_date, trade_balance) %>%
  slice(19:49)

# Perform join

us_by_year <- us_rgdp_cap %>%
  inner_join(us_rgdp_cap_growth, by = "obs_date") %>%
  inner_join(corp_profits, by = "obs_date") %>%
  inner_join(sp500, by = "obs_date") %>%
  inner_join(patents_granted, by = "obs_date") %>%
  inner_join(m_a, by = "obs_date") %>%
  inner_join(trade, by = "obs_date") %>%
  mutate(
    rgdp_cap = as.numeric(rgdp_cap),
    rgdp_cap_gr = as.numeric(rgdp_cap_gr),
    profits = as.numeric(profits),
    value_m_a = as.numeric(value_m_a)
  )

# Save the resulting data

saveRDS(us_by_year, "us_by_year.RDS")

##########################################################

# Read in IMF real GDP data.

# Rename first column to country.
# Drop no data and NA values.
# Rename China.

imf_rgdp <- read_excel("raw_data/imf-rgdp.xls") %>%
  rename(country = "Real GDP growth (Annual percent change)") %>%
  mutate_all( ~ replace(., . == "no data", NA)) %>%
  drop_na() %>%
  mutate(country = replace(country, country == "China, People's Republic of", 
                           "China"))


# Ten biggest economy countries.
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
)

# Filter based on ten biggest economy countries.
# Change first column country as factor type and rest as numeric column type.

ten_econ <- imf_rgdp %>%
  filter(country %in% countries) %>%
  mutate_at(1, as.factor) %>%
  mutate_at(2:47, as.numeric) %>%
  select(c(1:42))

# Save the resulting data

saveRDS(ten_econ, "ten_econ.RDS")

##########################################################
# Read in state first time unemployment claims data.
# Total initial claims for 44 weeks of 2020 for each state
# From week of 1/4/2020 to 10/31/2020

state_unemployment <- read_excel("raw_data/unemploy-state.xlsx") %>%
  rename(state = "State",
         initial_claims = `Initial Claims`) %>%
  select(state, initial_claims) %>%
  group_by(state) %>%
  summarize(tot_claims = sum(initial_claims),
            .groups = "drop") %>%
  mutate(state = tolower(state))

# Read in state unemployment rate data
# Average unemployment rate from July to September of 2020 for each state

state_unemployment_rate <-
  read_excel("raw_data/unemploy-perc-state.xlsx") %>%
  mutate(unemp_rate = rowMeans(select(., "July", "August", "September"))) %>%
  rename(state = "State") %>%
  select(state, unemp_rate) %>%
  mutate(state = tolower(state))

# Read in U.S. States and if governor is democratic

us_states <- read_csv(
  "raw_data/us-state-govs.csv",
  col_types = cols(
    StateName = col_character(),
    StateAbbr = col_character(),
    DemGov = col_number()
  )
) %>%
  rename(state = "StateName") %>%
  rename(dem_gov = "DemGov") %>%
  mutate(state = tolower(state))

# Read in state population data.

state_pop <- read_csv(
  "raw_data/state-pop.csv",
  col_types = cols(
    .default = col_double(),
    SUMLEV = col_character(),
    REGION = col_character(),
    DIVISION = col_character(),
    STATE = col_character(),
    NAME = col_character()
  )
) %>%
  select(NAME, POPESTIMATE2019) %>%
  rename(state = NAME,
         pop = POPESTIMATE2019) %>%
  slice(-(1:5)) %>%
  filter(!state %in% c("District of Columbia", "Puerto Rico")) %>%
  mutate(state = tolower(state))

# Read in states COVID-19 data.

# Data are collected from 3/6/2020 to 11/11/2020 and accumulative

state_covid <- read_csv(
  "raw_data/states-covid.csv",
  col_types = cols(
    .default = col_double(),
    date = col_date(format = ""),
    state = col_character(),
    dataQualityGrade = col_character()
  )) %>%
  filter(date == as.Date("2020-11-11")) %>%
  select(state, death, positive, totalTestResults) %>%
  rename(StateAbbr = state,
         tot_death = death, 
         tot_pos = positive, 
         tot_test = totalTestResults) %>%
  inner_join(us_states, by = "StateAbbr") %>%
  inner_join(state_pop, by = "state") %>%
  mutate(pos_rate = tot_pos / tot_test,
         death_per_1000 = tot_death * 1000 / pop,
         test_per_1000 = tot_test * 1000 / pop)

# Get U.S. states from ggmap map_data then perform join

state_metrics <- map_data("state") %>%
  rename(state = region) %>%
  inner_join(state_unemployment, by = "state") %>%
  inner_join(state_unemployment_rate, by = "state") %>%
  inner_join(state_covid, by = "state") %>%
  select(
    state,
    long,
    lat,
    group,
    tot_claims,
    unemp_rate,
    pos_rate,
    tot_pos,
    death_per_1000,
    test_per_1000,
    dem_gov
  )

# Save the resulting data

saveRDS(state_metrics, "state_metrics.RDS")

##########################################################

# Read in weekly economic index data

wei <- read_excel("raw_data/weekly-economic-index.xlsx", skip = 639) %>% 
  mutate(index = `1.55`,
         week = 1:38) %>% 
  select(week, index)

# Read in weekly case counts data

weekly_trends <- read_excel("raw_data/covid-weekly-trends.xlsx") %>% 
  mutate(new_cases = `Weekly New Cases`,
         new_deaths = `Weekly New Deaths`,
         week = 1:40) %>% 
  slice(1:38) %>% 
  select(week, new_cases, new_deaths)

# Read in initial claims data

initial_claims <- read_excel("raw_data/initial-claims-weekly.xls", skip = 52) %>% 
  mutate(claims = `217000`,
         week = 1:38) %>% 
  select(week, claims)

# Perform join to create tibble weekly

weekly <- inner_join(wei, weekly_trends, by = "week") %>% 
  inner_join(initial_claims, by = "week")

# Save the resulting data

saveRDS(weekly, "weekly.RDS")
