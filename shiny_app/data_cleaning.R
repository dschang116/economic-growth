library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)

# as.data.frame, write_rds

rGDPcap <- read_excel("raw_data/fred_rgdp_cap.xls") %>% 
  slice(-c(1:10)) %>% 
  rename("rGDPpercap" = ...2) %>% 
  mutate(obs_date = seq(as.Date("1947-01-01"), by = "quarter", len = 295)) %>% 
  select(obs_date, rGDPpercap) %>% 
  mutate(rGDPpercap = as.numeric(rGDPpercap))

rGDPcapgrowth <- read_excel("raw_data/fred_rgdp_capgrowth.xls") %>% 
  slice(-c(1:10)) %>% 
  rename("rGDPpercapgrowth" = ...2) %>% 
  mutate(obs_date = seq(as.Date("1947-04-01"), by = "quarter", len = 294)) %>% 
  select(obs_date, rGDPpercapgrowth) %>% 
  mutate(rGDPpercapgrowth = as.numeric(rGDPpercapgrowth))

HHI <- read_excel("raw_data/WITS_HHI.xlsx", 
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

rGDPcapgrowth %>% 
  slice(212:294) %>% 
  ggplot(aes(x = obs_date, y = rGDPpercapgrowth)) +
  geom_line() 

HHI %>% 
  ggplot(aes(x = year, y = concentration)) +
    geom_point()

