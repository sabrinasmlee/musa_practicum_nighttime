#SAFEGRAPH EXPLORATORY ANALYSIS
#MUSA Practicum

########
# SETUP
########

library(tidyverse)
library(sf)
library(lubridate)
library(datetime)
library(viridis)

setwd("~/GitHub/musa_practicum_nighttime")

###########
# LOAD DATA
############
dat <- read.csv("./data/moves_2018.csv")
phl_cbg <- st_read("http://data.phl.opendata.arcgis.com/datasets/2f982bada233478ea0100528227febce_0.geojson") %>%
  mutate(GEOID10 = as.numeric(GEOID10))
phl_zip <- st_read("http://data.phl.opendata.arcgis.com/datasets/b54ec5210cee41c3a884c9086f7af1be_0.geojson") %>%
  mutate(CODE = as.numeric(CODE))

#MADDY's INITIAL WORK 
#Clean up date column
dat <- dat %>% 
  mutate(date_range_start = as.date(dat$date_range_start),
         date_range_end = as.date(dat$date_range_end))

#dat <- head(dat, 300) #For test purposes just looking at the first N observations

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

#Zip code analysis
dat_zip <-
  dat %>%
  select(safegraph_place_id,
         postal_code, 
         raw_visit_counts, 
         raw_visitor_counts, 
         median_dwell, 
         distance_from_home)%>%
  rename(., CODE = postal_code) %>%
  group_by(CODE) %>%
  summarize(Avg_Visits = mean(raw_visit_counts),
            Avg_Visitors = mean(raw_visitor_counts),
            Avg_Dwell = mean(median_dwell),
            Avg_DistHome = mean(distance_from_home)) %>%
  # pivot_longer(cols = c("Avg_Visits", "Avg_Visitors", "Avg_Dwell", "Avg_DistHome"),
  #              names_to = "Variable", 
  #              values_to = "Avg_Count") %>%
  left_join(phl_zip) %>%
  st_as_sf()

#Average number of visits by zip code
dat_zip %>%
  ggplot() + 
  geom_sf(aes(fill = Avg_Visits)) + 
  scale_fill_viridis() +
  labs(title = "Average SafeGraph Visits per POI by Zip Code")

#Average number of visitors by zip code
dat_zip %>%
  ggplot() + 
  geom_sf(aes(fill = Avg_Visitors)) + 
  scale_fill_viridis() +
  labs(title = "Average SafeGraph Visitors per POI by Zip Code")

#Average dwell time by zip code
dat_zip %>%
  ggplot() + 
  geom_sf(aes(fill = Avg_Dwell)) + 
  scale_fill_viridis() +
  labs(title = "Average SafeGraph Dwell \nTime per POI by Zip Code")

#Average dist from home by zip code
dat_zip %>%
  ggplot() + 
  geom_sf(aes(fill = Avg_DistHome)) + 
  scale_fill_viridis() +
  labs(title = "Average SafeGraph Distance from \nHome per POI by Zip Code")

#Census Block Groups
dat_cbg <-
  dat %>%
  select(safegraph_place_id,
         poi_cbg, 
         raw_visit_counts, 
         raw_visitor_counts, 
         median_dwell, 
         distance_from_home)%>%
  rename(., GEOID10 = poi_cbg) %>%
  group_by(GEOID10) %>%
  summarize(Avg_Visits = mean(raw_visit_counts),
            Avg_Visitors = mean(raw_visitor_counts),
            Avg_Dwell = mean(median_dwell),
            Avg_DistHome = mean(distance_from_home)) %>%
  full_join(phl_cbg) %>% #lots of cbgs without SG POIs, so full join instead of left join.
  st_as_sf()

#Average number of visits by CBG
dat_cbg %>%
  subset(Avg_Visits < 1500) %>% #removing outliers to see patterns in the data
  ggplot() + 
  geom_sf(data = phl_cbg, fill = "grey40", color = "transparent") +
  geom_sf(aes(fill = Avg_Visits), color = "transparent") + 
  scale_fill_viridis() +
  labs(title = "Average SafeGraph Visits per POI by CBG")

#Average number of visitors by CBG
dat_cbg %>%
  subset(Avg_Visitors < 1000) %>% #removing outliers to see patterns in the data
  ggplot() + 
  geom_sf(data = phl_cbg, fill = "grey40", color = "transparent") +
  geom_sf(aes(fill = Avg_Visitors), color = "transparent") + 
  scale_fill_viridis() +
  labs(title = "Average SafeGraph Visitors per POI by CBG")

boxplot(dat_cbg$Avg_Visitors)

#Average dwell time by CBG
dat_cbg %>%
  ggplot() + 
  geom_sf(aes(fill = Avg_Dwell), color = "transparent") + 
  scale_fill_viridis() +
  labs(title = "Average SafeGraph Dwell \nTime per POI by CBG")

#Average dist from home by zip code
dat_cbg %>%
  subset(Avg_DistHome < 9000) %>% #removing outliers to see patterns in the data
  ggplot() + 
  geom_sf(data = phl_cbg, fill = "grey40", color = "transparent") +
  geom_sf(aes(fill = Avg_DistHome), color = "transparent") + 
  scale_fill_viridis() +
  labs(title = "Average SafeGraph Distance from \nHome per POI by CBG")


#WIP: Figuring out the POI CBGs with higest number of visits from outside CBGs.

# dat_cbg <-
#   dat %>%
#   select(safegraph_place_id, poi_cbg, visitor_home_cbgs) %>%
#   mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
#   mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
#   mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
#   mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
#   unnest(visitor_home_cbgs) %>%
#   separate(.,
#            visitor_home_cbgs,
#            c("CBG", "Visitors"),
#            sep = ":") %>%
#   mutate(Visitors = as.numeric(Visitors))
# 
# dat_cbg %>%
#   group_by(poi_cbg) %>%
#   summarize(Count = n())

# ^^Figure out how to join this to the phl_cbg map.  
#See which cbgs hve the highest number of people coming from ouside.
