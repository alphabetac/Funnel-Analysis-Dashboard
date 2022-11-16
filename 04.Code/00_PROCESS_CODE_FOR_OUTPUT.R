## Load libraries ---------------------

library(tidyverse)
library(here)
library(zoo)
library(lubridate)

## Set options ------------------------

options(scipen = 999)

## Load data --------------------------

brand_awareness <- read_csv(here("03.Data", "Processed Data",
                                 "Prompted Brand Awareness.csv"))
uk_consumer_search <- read_csv(here("03.Data", "Processed Data",
                                    "UK Consumer Site Search Performance.csv"))
it_consumer_search <- read_csv(here("03.Data", "Processed Data",
                                    "IT Consumer Site Search Performance.csv"))
uk_business_metrics <- read_csv(here("03.Data", "Processed Data",
                                     "UK Business Metrics.csv"))
it_business_metrics <- read_csv(here("03.Data", "Processed Data",
                                     "IT Business Metrics.csv"))
google_trends <- read_csv(here("03.Data", "Processed Data",
                               "Google Trends - Share of Search.csv"))
business_traffic <- read_csv(here("03.Data", "Processed Data",
                                  "Weekly Business Page Loads.csv"))



brand_awareness_uk <- brand_awareness %>%
  filter(country == "UK") %>%
  select(quarter_start, prompted_awareness) %>%
  read.zoo(FUN = function(x) as.yearqtr(x, "%Y-Q%q")) %>%
  cbind(NA, NA) %>%
  t() %>%
  c()

brand_awareness_uk[70] <- NA
brand_awareness_uk[72] <- 0.631

brand_awareness_uk <- brand_awareness_uk %>%
  na.approx() %>%
  zooreg(start = as.yearmon("2016-07-01"), freq = 12) %>%
  fortify.zoo() %>%
  tibble() %>%
  mutate(country = "UK")

brand_awareness_it <- brand_awareness %>%
  filter(country == "IT") %>%
  select(quarter_start, prompted_awareness) %>%
  read.zoo(FUN = function(x) as.yearqtr(x, "%Y-Q%q")) %>%
  cbind(NA, NA) %>%
  t() %>%
  c()

brand_awareness_it[70] <- NA
brand_awareness_it[72] <- 0.384

brand_awareness_it <- brand_awareness_it %>%
  na.approx() %>%
  zooreg(start = as.yearmon("2018-10-01"), freq = 12) %>%
  fortify.zoo() %>%
  tibble() %>%
  mutate(country = "IT")

brand_awareness_interpolated <- brand_awareness_uk %>%
  add_row(brand_awareness_it) %>%
  rename(prompted_awareness = ".") %>%
  mutate(month_start = as_date(Index)) %>%
  select(-Index)

## Create comprehensive dataset
full_data <- google_trends %>%
  left_join(uk_business_metrics %>%
              mutate(country = "UK") %>%
              add_row(it_business_metrics %>%
                        mutate(country = "IT")),
            by = c("month_start", "month", "year")) %>%
  left_join(brand_awareness_interpolated,
            by = c("month_start", "country")) %>%
  left_join(business_traffic %>%
              pivot_longer(cols = 4:5,
                           names_to = "country",
                           names_pattern = "(uk|it)_consumer_traffic",
                           values_to = "biz_traffic_consumer") %>%
              mutate(month = month(week_start),
                     country = str_to_upper(country)) %>%
              group_by(month, year, country) %>%
              summarise(biz_traffic_consumer = sum(biz_traffic_consumer)),
            by = c("month", "year", "country")) %>%
  filter(
    !(country == "UK" & consumer_sessions <= 708508),
    !(country == "UK" & biz_traffic_consumer <= 2274),
    !(country == "UK" & biz_traffic_consumer >= 42577)
  ) %>%
  rename(search = share_of_search)

## Save data
full_data %>%
  write_csv(here("03.Data", "trustpilot_funnel_data.csv"))
