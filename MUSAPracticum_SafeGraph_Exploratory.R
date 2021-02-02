#SAFEGRAPH EXPLORATORY ANALYSIS
#MUSA Practicum

########
# SETUP
########

library(tidyverse)
library(sf)
library(lubridate)
library(datetime)

setwd("~/GitHub/musa_practicum_nighttime")

###########
# LOAD DATA
############
dat <- read.csv("./data/moves_2018.csv")
phl_cbg <- st_read("http://data.phl.opendata.arcgis.com/datasets/2f982bada233478ea0100528227febce_0.geojson")

#MADDY's INITIAL WORK 
#Clean up date column
dat <- dat %>% 
  mutate(date_range_start = as.date(dat$date_range_start),
         date_range_end = as.date(dat$date_range_end))

dat <- head(dat, 300) #For test purposes just looking at the first N observations

colnames(dat)
head(dat)

#Business traffic by day
dat_day <- 
  dat %>% 
  select(safegraph_place_id, popularity_by_day) %>%
  mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = "\\[|\\]")) %>%
  mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = "\\{|\\}")) %>%
  mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = '\\"|\\"')) %>% 
  #mutate(popularity_by_day = str_replace_all(popularity_by_day, "[:]", " ")) %>% 
  mutate(popularity_by_day = str_split(popularity_by_day, pattern = ",")) %>%
  unnest(popularity_by_day) %>%
  separate(.,
           popularity_by_day,
           c("Day", "Visits"),
           sep = ":") %>%
  mutate(Visits = as.numeric(Visits))

week_order = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

dat_day %>%
  group_by(Day) %>%
  summarize(Avg_Visits = mean(Visits)) %>%
  mutate(Day = factor(Day, levels = week_order)) %>% arrange(Day) %>% #order by day of the week
  ggplot(., aes(x = Day, y = Avg_Visits)) + 
  geom_col()

#Popularity by Hour
dat_hour <-
  dat %>%
  select(safegraph_place_id, popularity_by_hour) %>%
  mutate(popularity_by_hour = str_remove_all(popularity_by_hour, pattern = "\\[|\\]")) %>%
  mutate(popularity_by_hour = str_split(popularity_by_hour, pattern = ",")) %>%
  unnest(popularity_by_hour) # Problem - unlike pop_by_day column, the hours aren't tagged.

colnames(dat)

#CBGs
dat_cbg <-
  dat %>%
  select(safegraph_place_id, poi_cbg, visitor_home_cbgs) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>%
  separate(.,
           visitor_home_cbgs,
           c("CBG", "Visitors"),
           sep = ":") %>%
  mutate(Visitors = as.numeric(Visitors))

dat_cbg %>%
  group_by(poi_cbg) %>%
  summarize(Count = n())

# ^^Figure out how to join this to the phl_cbg map.  
#See which cbgs hve the highest number of people coming from ouside.
