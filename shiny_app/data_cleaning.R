library(shiny)
library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)
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

fips_unemploy_state <- inner_join(unemploy_state, statepop, by = "full") %>% 
  filter(full != "District of Columbia") %>% 
  select(fips, tot_claims)

fips_unemploy_perc_state <- inner_join(unemploy_perc_state, statepop, by = "full") %>% 
  filter(full != "District of Columbia") %>% 
  select(fips, avg_perc)

fips_covid <- inner_join(covid_summ, statepop, by = "abbr") %>% 
  select(fips, tot_death, tot_pos, tot_test, pos_rate)

us_gdp_cap <- read_excel("shiny_app/raw_data/rgdp_cap.xls") %>% 
  slice(-c(1:10)) %>% 
  rename("gdp_cap" = ...2) %>% 
  mutate(obs_date = 1947:2020) %>% 
  select(obs_date, gdp_cap) %>% 
  mutate(gdp_cap = as.numeric(gdp_cap))

us_gdp_cap_growth <- read_excel("shiny_app/raw_data/rgdp-cap_growth.xls") %>% 
  slice(-c(1:10)) %>% 
  rename("rGDPpercapgrowth" = ...2) %>% 
  mutate(obs_date = 1948:2020) %>% 
  select(obs_date, rGDPpercapgrowth) %>% 
  mutate(rGDPpercapgrowth = as.numeric(rGDPpercapgrowth))

hhi <- read_excel("shiny_app/raw_data/hhi.xlsx", 
                   sheet = "Country-Timeseries") %>% 
  filter(`Country Name` == "United States") %>% 
  select('1991':'2018') %>% 
  pivot_longer(cols = '1991':'2018',
               names_to = "year",
               values_to = "concentration") %>% 
  mutate_all(as.numeric)

rGDPcap %>%
  slice(213:295) %>% 
  ggplot(aes(x = obs_date, y = rGDPpercap)) +
  geom_point()

us_gdp_cap_growth %>% 
  slice(212:294) %>% 
  ggplot(aes(x = obs_date, y = rGDPpercapgrowth)) +
  geom_line() 

HHI %>% 
  ggplot(aes(x = year, y = concentration)) +
    geom_point()

