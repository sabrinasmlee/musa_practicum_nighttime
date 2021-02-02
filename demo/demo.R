##################################
## demonstration of safegraph data
##################################

library(tidyverse)
library(lubridate)
library(vroom)
library(sf)

## read it in with vroom to save time
moves <- vroom("moves_monthly.csv")
phila <- st_read("phila.geojson")

## how to unnest the temporal data
library(furrr)
plan(multiprocess, workers = 6)

## time is (88 seconds, so not fast)
tictoc::tic()

timeseries <- 
  moves %>% 
  select(safegraph_place_id, date_range_start, date_range_end, visits_by_day) %>%
  mutate(date_range_start = as_date(date_range_start),
         date_range_end = as_date(date_range_end)) %>%
  mutate(visits_by_day = str_remove_all(visits_by_day, pattern = "\\[|\\]")) %>% 
  mutate(visits_by_day = str_split(visits_by_day, pattern = ",")) %>%
  mutate(visits_by_day = future_map(visits_by_day, function(x){
    unlist(x) %>%
      as_tibble() %>%
      mutate(day = 1:n(),
             visits = value)
  })) %>%
  unnest(cols = c(visits_by_day))

tictoc::toc()

## how to unnest the origin-destination data

## note that we can speed these up by joining venue data and filtering it down
tictoc::tic()

odmatrix <- 
  moves %>% 
  select(safegraph_place_id, visitor_home_cbgs) %>%
  mutate(visitor_home_cbgs = future_map(visitor_home_cbgs, function(x){
    jsonlite::fromJSON(x) %>% 
      as_tibble()
  })) %>% 
  unnest(visitor_home_cbgs) %>%
  pivot_longer(!safegraph_place_id, names_to = "cbg", values_to = "visits") %>%
  drop_na(visits)

tictoc::tic()
