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

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}
q5 <- function(variable) {as.factor(ntile(variable, 5))}
palette5 <- viridis_pal()(5)

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

setwd("~/GitHub/musa_practicum_nighttime")

###########
# LOAD DATA
############
dat <- read.csv("./data/moves_2018.csv")
phila <- st_read("./demo/phila.geojson")

phl_cbg <- st_read("http://data.phl.opendata.arcgis.com/datasets/2f982bada233478ea0100528227febce_0.geojson") %>%
  mutate(GEOID10 = as.numeric(GEOID10))
phl_zip <- st_read("http://data.phl.opendata.arcgis.com/datasets/b54ec5210cee41c3a884c9086f7af1be_0.geojson") %>%
  mutate(CODE = as.numeric(CODE))

#MADDY's INITIAL WORK 
#Join to geography data
dat <- dat %>%
  dplyr::select(safegraph_place_id, date_range_start, date_range_end, raw_visit_counts,
                       raw_visitor_counts, visits_by_day, poi_cbg, visitor_home_cbgs, 
                       visitor_daytime_cbgs, visitor_work_cbgs, visitor_country_of_origin,
                       distance_from_home, median_dwell, bucketed_dwell_times, related_same_day_brand, 
                       related_same_month_brand, popularity_by_hour, popularity_by_day, device_type) %>%
  # mutate(date_range_start = as.date(dat$date_range_start),
  #        date_range_end = as.date(dat$date_range_end)) %>%
  left_join(., phila, by = "safegraph_place_id") %>% 
  st_as_sf() 


#dat <- head(dat, 300) #For test purposes just looking at the first N observations

colnames(dat)
head(dat)

#Popularity by Hour
dat_hour <-
  dat %>%
  select(safegraph_place_id, popularity_by_hour) %>%
  mutate(popularity_by_hour = str_remove_all(popularity_by_hour, pattern = "\\[|\\]")) %>%
  mutate(popularity_by_hour = str_split(popularity_by_hour, pattern = ",")) %>%
  unnest(popularity_by_hour) # Problem - unlike pop_by_day column, the hours aren't tagged.

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
  left_join(phl_cbg) %>% 
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

#Average dist from home by CBG
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

################################################
# Question 1: Where are people travelling from?
################################################

dat_cbg_visitors <- 
  dat %>%
  select(safegraph_place_id, poi_cbg, visitor_home_cbgs) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>%
  separate(.,
           visitor_home_cbgs,
           c("Visitor_CBG", "Visitors"),
           sep = ":") %>%
  mutate(Visitor_CBG = as.numeric(Visitor_CBG),
         poi_cbg = as.numeric(poi_cbg),
         Visitors = as.numeric(Visitors))

dat_cbg_visitors %>%
  group_by(poi_cbg) %>%
  summarize(Count = n()) %>%
  rename(., GEOID10 = poi_cbg) %>%
  left_join(phl_cbg) %>%
  st_as_sf() %>%
  ggplot() + 
  geom_sf(aes(fill = q5(Count)), color = "transparent") + 
  scale_fill_manual(values = palette5,
                    name = "CBG Count\nQuintile Breaks") + 
  mapTheme() +
  labs(title = "How many different CBGs visit the POI CBG?")

dat_zip_visitors <- 
  dat %>%
  select(safegraph_place_id, postal_code, visitor_home_cbgs) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>%
  separate(.,
           visitor_home_cbgs,
           c("Visitor_CBG", "Visitors"),
           sep = ":") %>%
  mutate(Visitor_CBG = as.numeric(Visitor_CBG),
         postal_code = as.numeric(postal_code),
         Visitors = as.numeric(Visitors))

dat_zip_visitors %>%
  group_by(postal_code) %>%
  summarize(Count = n()) %>%
  rename(., CODE = postal_code) %>%
  left_join(phl_zip) %>%
  st_as_sf() %>%
  ggplot() + 
  geom_sf(aes(fill = q5(Count)), color = "transparent") + 
  scale_fill_manual(values = palette5,
                    name = "CBG Count\nQuintile Breaks") + 
  mapTheme() +
  labs(title = "How many different CBGs visit the POI CBG?")

########################################
# Question 2: When are People Travelling
########################################
#Business traffic by day
dat_day <- 
  dat %>% 
  select(safegraph_place_id, popularity_by_day) %>%
  mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = "\\[|\\]")) %>%
  mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = "\\{|\\}")) %>%
  mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = '\\"|\\"')) %>% 
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

# dat_day %>%
#   group_by(Day) %>%
#   summarize(Count = n()) %>%
#   mutate(Day = factor(Day, levels = week_order)) %>% arrange(Day) %>% #order by day of the week
#   ggplot(., aes(x = Day, y = Count)) + 
#   geom_col()

dat_hour <- 
  dat %>% 
  select(safegraph_place_id, popularity_by_hour) %>%
  mutate(popularity_by_hour = str_remove_all(popularity_by_hour, pattern = "\\[|\\]")) %>%
  unnest(popularity_by_hour) %>%
  separate(.,
           popularity_by_hour,
           c("0", "1", "2", "3", "4", "5", "6", 
             "7", "8", "9", "10", "11", "12", 
             "13", "14", "15", "16", "17", "18",
             "19", "20", "21", "22", "23"),
           sep = ",") %>%
  pivot_longer(cols = 2:25,
               names_to = "Hour",
               values_to = "Count") %>%
  mutate(Hour = as.numeric(Hour),
         Count = as.numeric(Count))
  
dat_hour %>%
  group_by(Hour) %>%
  summarize(Count = sum(Count)) %>%
  ggplot(., aes(x = Hour, y = Count)) + 
  geom_col()


################################################
# Question 1: Where are people travelling from? - Restaurants
################################################

#By Visitor CBG

dat_cbg_visitors <- 
  dat %>%
  select(safegraph_place_id, date_range_start, top_category, sub_category, poi_cbg, visitor_home_cbgs, geometry) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>%
  separate(.,
           visitor_home_cbgs,
           c("Visitor_CBG", "Visitors"),
           sep = ":") %>%
  mutate(Visitor_CBG = as.numeric(Visitor_CBG),
         poi_cbg = as.numeric(poi_cbg),
         Visitors = as.numeric(Visitors))

unique(dat_cbg_visitors$date_range_start)

#Restaurants
dat_cbg_visitors %>%
  filter(top_category == "Restaurants and Other Eating Places", 
         date_range_start == "2018-01-01T05:00:00Z") %>%
  group_by(safegraph_place_id) %>%
  summarize(Count = n()) %>%
  # rename(., GEOID10 = poi_cbg) %>%
  # left_join(phl_cbg) %>%
  # st_as_sf() %>%
  ggplot() + 
  geom_sf(data = phl_cbg, fill = "grey40", color = "transparent") +
  geom_sf(aes(color = q5(Count)), size = .5) + 
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "CBG Count\nQuintile Breaks") +
  mapTheme() +
  labs(title = "How many different CBGs visit each restaurant (January)?")

dat_cbg_visitors %>%
  filter(top_category == "Restaurants and Other Eating Places", 
         date_range_start == "2018-07-01T04:00:00Z") %>%
  group_by(safegraph_place_id) %>%
  summarize(Count = n()) %>%
  ggplot() + 
  geom_sf(data = phl_cbg, fill = "grey40", color = "transparent") +
  geom_sf(aes(color = q5(Count)), size = .5) + 
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "CBG Count\nQuintile Breaks") +
  mapTheme() +
  labs(title = "How many different CBGs visit each restaurant (July)?")

#Bars
dat_cbg_visitors %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)", 
         date_range_start == "2018-01-01T05:00:00Z") %>%
  group_by(safegraph_place_id) %>%
  summarize(Count = n()) %>%
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey40", color = "transparent") +
  geom_sf(aes(color = q5(Count)), size = 1) + 
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "CBG Count\nQuintile Breaks") +
  mapTheme() +
  labs(title = "How many different CBGs visit each bar (January)?")

dat_cbg_visitors %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)", 
         date_range_start == "2018-07-01T04:00:00Z") %>%
  group_by(safegraph_place_id) %>%
  summarize(Count = n()) %>%
  ggplot() + 
  geom_sf(data = phl_cbg, fill = "grey40", color = "transparent") +
  geom_sf(aes(color = q5(Count)), size = 1) + 
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "CBG Count\nQuintile Breaks") +
  mapTheme() +
  labs(title = "How many different CBGs visit each bar (July)?")

#Concert Venues
dat_cbg_visitors %>%
  filter(top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies", 
         date_range_start == "2018-01-01T05:00:00Z") %>%
  group_by(safegraph_place_id) %>%
  summarize(Count = n()) %>%
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey40", color = "transparent") +
  geom_sf(aes(color = q5(Count)), size = 1) + 
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "CBG Count\nQuintile Breaks") +
  mapTheme() +
  labs(title = "How many different CBGs visit each bar (January)?")

dat_cbg_visitors %>%
  filter(top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  group_by(safegraph_place_id) %>%
  summarize(Count = n()) %>%
  ggplot() + 
  geom_sf(data = phl_cbg, fill = "grey40", color = "transparent") +
  geom_sf(aes(color = q5(Count)), size = 1) + 
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "CBG Count\nQuintile Breaks") +
  mapTheme() +
  labs(title = "How many different CBGs visit each bar (July)?")

# dat_cbg_visitors %>%
#   filter(top_category == "Restaurants and Other Eating Places") %>%
#   group_by(safegraph_place_id, Visitor_CBG) %>%
#   summarize(Count = n()) %>%
#   # rename(., GEOID10 = poi_cbg) %>%
#   # left_join(phl_cbg) %>%
#   # st_as_sf() %>%
#   ggplot() + 
#   geom_sf(data = phl_cbg, fill = "grey40", color = "transparent") +
#   geom_sf(aes(color = q5(Count)), size = .5) + 
#   scale_fill_manual(values = palette5,
#                     aesthetics = c("colour", "fill"),
#                     name = "CBG Count\nQuintile Breaks") +
#   mapTheme() +
#   labs(title = "How many different CBGs visit each restaurant?")


########################################
# Question 2: When are People Travelling - Restaurants
########################################
#Business traffic by day
dat_day <- 
  dat %>% 
  select(safegraph_place_id, top_category, sub_category, popularity_by_day) %>%
  mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = "\\[|\\]")) %>%
  mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = "\\{|\\}")) %>%
  mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = '\\"|\\"')) %>% 
  mutate(popularity_by_day = str_split(popularity_by_day, pattern = ",")) %>%
  unnest(popularity_by_day) %>%
  separate(.,
           popularity_by_day,
           c("Day", "Visits"),
           sep = ":") %>%
  mutate(Visits = as.numeric(Visits))

week_order = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

dat_day %>%
  filter(top_category == "Restaurants and Other Eating Places") %>%
  group_by(sub_category, Day) %>%
  summarize(Avg_Visits = mean(Visits)) %>%
  mutate(Day = factor(Day, levels = week_order)) %>% arrange(Day) %>% #order by day of the week
  ggplot(., aes(x = Day, y = Avg_Visits)) + 
  geom_col() +
  facet_wrap(~sub_category)

dat_hour <- 
  dat %>% 
  select(safegraph_place_id, top_category, sub_category, popularity_by_hour) %>%
  mutate(popularity_by_hour = str_remove_all(popularity_by_hour, pattern = "\\[|\\]")) %>%
  unnest(popularity_by_hour) %>%
  separate(.,
           popularity_by_hour,
           c("0", "1", "2", "3", "4", "5", "6", 
             "7", "8", "9", "10", "11", "12", 
             "13", "14", "15", "16", "17", "18",
             "19", "20", "21", "22", "23"),
           sep = ",") %>%
  pivot_longer(cols = 4:27,
               names_to = "Hour",
               values_to = "Count") %>%
  mutate(Hour = as.numeric(Hour),
         Count = as.numeric(Count))

#Restaurants
dat_hour %>%
  filter(top_category == "Restaurants and Other Eating Places") %>%
  group_by(sub_category, Hour) %>%
  summarize(Avg_Visits = mean(Count)) %>%
  ggplot(., aes(x = Hour, y = Avg_Visits)) + 
  geom_col() +
  facet_wrap(~sub_category)

#Bars
dat_hour %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)") %>%
  group_by(sub_category, Hour) %>%
  summarize(Avg_Visits = mean(Count)) %>%
  ggplot(., aes(x = Hour, y = Avg_Visits)) + 
  geom_col()

#Concert Venues
dat_hour %>%
  filter(top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  group_by(top_category, Hour) %>%
  summarize(Avg_Visits = mean(Count)) %>%
  ggplot(., aes(x = Hour, y = Avg_Visits)) + 
  geom_col() +
  facet_wrap(~top_category)

unique(dat$top_category)

dat %>% filter(top_category == "Performing Arts Companies")
unique(other$sub_category)
