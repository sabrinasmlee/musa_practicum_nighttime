# Template to unnest popularity by hour column

#Load packages
library(tidyverse)
library(sf)

#setwd("~/GitHub/musa_practicum_nighttime")

#Load Safegraph data
dat <- read.csv("./moves_2018.csv")
phila <- st_read("./phila.geojson")

#Join Safegraph datasets
dat2 <- dat %>%
  dplyr::select(safegraph_place_id, date_range_start, date_range_end, raw_visit_counts,
                raw_visitor_counts, visits_by_day, poi_cbg, visitor_home_cbgs, 
                visitor_daytime_cbgs, visitor_work_cbgs, visitor_country_of_origin,
                distance_from_home, median_dwell, bucketed_dwell_times, related_same_day_brand, 
                related_same_month_brand, popularity_by_hour, popularity_by_day, device_type) %>%
  left_join(., phila, by = "safegraph_place_id") %>% 
  st_as_sf() 

#Unnest the popularity by hour column
dat_hour <- 
  dat2 %>% 
  select(safegraph_place_id, top_category, sub_category, popularity_by_hour) %>% #select variables
  mutate(popularity_by_hour = str_remove_all(popularity_by_hour, pattern = "\\[|\\]")) %>% #remove brackets
  unnest(popularity_by_hour) %>% #unnest values
  separate(.,
           popularity_by_hour,
           c("0", "1", "2", "3", "4", "5", "6", 
             "7", "8", "9", "10", "11", "12", 
             "13", "14", "15", "16", "17", "18",
             "19", "20", "21", "22", "23"),
           sep = ",") %>% #separate into columns by hour
  pivot_longer(cols = 4:27,
               names_to = "Hour",
               values_to = "Visitors") %>% #pivot into long format
  mutate(Hour = as.numeric(Hour),
         Visitors = as.numeric(Visitors)) #convert unnested values to numeric format


#Plot showing popularity_by_hour for bars
dat_hour %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)") %>%
  group_by(Hour) %>%
  summarize(Visitors = mean(Visitors)) %>%
  ggplot(., aes(x = Hour, y = Visitors)) + 
  geom_col() +
  labs(title = "Popularity by Hour, Bars in Philadelphia")

#Plot showing popularity_by_hour for restaurants
dat_hour %>%
  filter(top_category == "Restaurants and Other Eating Places") %>%
  group_by(Hour) %>%
  summarize(Visitors = mean(Visitors)) %>%
  ggplot(., aes(x = Hour, y = Visitors)) + 
  geom_col() +
  labs(title = "Popularity by Hour, Restaurants and Other Eating Places in Philadelphia")

#for grocery stores 
dat_hour %>%
  filter(top_category == "Grocery Stores") %>%
  group_by(Hour) %>%
  summarize(Visitors = mean(Visitors)) %>%
  ggplot(., aes(x = Hour, y = Visitors)) + 
  geom_col() +
  labs(title = "Popularity by Hour, Grocery Stores in Philadelphia")

#for performing arts companies
dat_hour %>%
  filter(top_category == "Performing Arts Companies") %>%
  group_by(Hour) %>%
  summarize(Visitors = mean(Visitors)) %>%
  ggplot(., aes(x = Hour, y = Visitors)) + 
  geom_col() +
  labs(title = "Popularity by Hour, Performing Arts Companies in Philadelphia")

