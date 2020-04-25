
####################################
### Coronavirus Time Series Data ###
####################################

## 0. Load libraries

# - Data Wrangling
# library(tidyverse)
library(dplyr)   
library(tidyr)     
library(stringr)   # string
library(lubridate) # dates

# - Tables 
library(data.table) 
library(readr)
library(tibble)

# - Plots
library(ggplot2)
library(scales)

# - URL 
library(RCurl)

####################################
####################################

 
## 1. Download Files
url.path <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/',
                   'master/csse_covid_19_data/csse_covid_19_time_series')

filenames <- c('time_series_covid19_confirmed_global.csv',
               'time_series_covid19_deaths_global.csv',
               'time_series_covid19_recovered_global.csv',
               
               'time_series_covid19_confirmed_US.csv',
               'time_series_covid19_deaths_US.csv')

dest.path <- paste0(getwd(),"/covid_data")

## Create data source folder
# - make sure you're in the correct working directory
dir.create("covid_data")

## Download files to local wd
download <- function(filename) {
  url = file.path(url.path, filename)
  dest = file.path(dest.path, filename)
  download.file(url, dest)
}
bin <- lapply(filenames, download)

## Load data into R
raw_confirmed <- read_csv("./covid_data/time_series_covid19_confirmed_global.csv")
raw_deaths <- read_csv("./covid_data/time_series_covid19_deaths_global.csv")
raw_recovered <- read_csv("./covid_data/time_series_covid19_recovered_global.csv")

US_confirmed <- read_csv("./covid_data/time_series_covid19_confirmed_US.csv")
US_deaths <- read_csv("./covid_data/time_series_covid19_deaths_US.csv")

####################################
####################################

## 2. Data Cleaning

# function to avoid copy-paste
cleanData <- function(data) {
  data %>% 
    # not interested in geographic plotting yet
    select(-c(Lat, Long)) %>% 
    rename(country = `Country/Region`, state = `Province/State`) %>% 
    gather(-c(country, state), key=date, value=count) %>% 
    mutate(date = mdy(date))
}

### GLOBAL DATA ###
data_confirmed <- raw_confirmed %>% 
  cleanData() %>% 
  rename(confirmed = count)

data_deaths <- raw_deaths %>% 
  cleanData() %>% 
  rename(deaths = count)

data_recovered <- raw_recovered %>% 
  cleanData() %>% 
  rename(recovered = count)


# join confirmed, deaths, recovered tables into one table
data_global <- data_confirmed %>% 
  # confirmed cases 
  select(-state) %>% 
  group_by(country, date) %>% 
  summarise(confirmed = sum(confirmed, na.rm=T)) %>% 
  ungroup() %>% 
  
  # join with deaths table
  inner_join(data_deaths %>% 
               select(-state) %>% 
               group_by(country, date) %>% 
               summarise(deaths = sum(deaths, na.rm=T)) %>% 
               ungroup(),
             by = c("date", "country")) %>% 
  
  # join with recovered table
  inner_join(data_recovered %>% 
               select(-state) %>% 
               group_by(country, date) %>% 
               summarise(recovered = sum(recovered, na.rm=T)) %>% 
               ungroup(), 
             by = c("date", "country"))


### US DATA ###
US_confirmed <- US_confirmed %>% 
  select(-c(UID, iso2, iso3, code3, FIPS, Country_Region, Lat, Long_, Combined_Key)) %>% 
  rename(city = Admin2, state = Province_State)

US_deaths <- US_deaths %>% 
  select(-c(UID, iso2, iso3, code3, FIPS, Country_Region, Lat, Long_, Combined_Key)) %>% 
  rename(city = Admin2, state = Province_State, population = Population)

# join confirmed and deaths (no recovered data from source JHU CSSE)
data_US <- US_confirmed %>% 
  gather(-c(city, state), key=date, value=count) %>% rename(confirmed = count) %>% 
  inner_join(US_deaths %>% 
               gather(-c(city, state, population), key=date, value=count) %>% 
               rename(deaths = count), 
             by=c("city", "state", "date")) %>% 
  select(city, state, population, date, confirmed, deaths)


####################################

## Last updated date
as_of_date_global <- names(raw_confirmed)[max(length(raw_confirmed))]
as_of_date_US <- names(US_confirmed)[max(length(US_confirmed))]
