# K-Means Clustering


########
# SETUP
########
library(sf)
library(tidyverse)
library(factoextra)

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

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

setwd("~/GitHub/musa_practicum_nighttime")

###########
# LOAD DATA
###########
dat <- read.csv("./data/moves_2018.csv")
phila <- st_read("./demo/phila.geojson", quiet = TRUE) 

dat2 <- dat %>% 
  dplyr::select(safegraph_place_id, 
                date_range_start, 
                date_range_end, 
                raw_visit_counts,
                raw_visitor_counts, 
                visits_by_day, 
                poi_cbg, 
                visitor_home_cbgs, 
                visitor_daytime_cbgs, 
                visitor_work_cbgs, 
                visitor_country_of_origin,
                distance_from_home, 
                median_dwell, 
                bucketed_dwell_times, 
                related_same_day_brand, 
                related_same_month_brand, 
                popularity_by_hour, 
                popularity_by_day, 
                device_type) %>%
  left_join(., phila, by = "safegraph_place_id") %>% 
  st_as_sf() %>%
  st_transform('ESRI:102728')

PHL_boundary <- 
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE) %>%
  st_transform('ESRI:102728') 

#Corridor shapefile
phl_corridors <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728') %>%
  select('NAME',
         'COMM_COUNT', 
         'OCC_COUNT', 
         'VAC_COUNT',
         'CORRIDOR_TYPE', 
         'PHYS_CHAR',  
         'COND_PRIV',
         'AGE',
         'STAGE',
         'Shape__Area') %>%
  drop_na() %>%
  mutate(corr_type = ifelse(CORRIDOR_TYPE == 1, "1. Neighborhood Subcenter", 
                            ifelse(CORRIDOR_TYPE == 2, "2. Neighborhood Center", 
                                   ifelse(CORRIDOR_TYPE == 3, "3. Community Center", 
                                          ifelse(CORRIDOR_TYPE == 4, "4. Regional Center", 
                                                 ifelse(CORRIDOR_TYPE == 5, "5. Superregional Center", 
                                                        ifelse(CORRIDOR_TYPE == 6, "6. Speciality Center", "Other")))))))

set.seed(123)
km.res <- kmeans(phl_corr.K, 4, nstart = 25)

phl_corridors <- cbind(phl_corridors, cluster = km.res$cluster) 

#K-Means
phl_corridors %>% 
  mutate(cluster = as.factor(cluster)) %>%
  ggplot() +
  geom_sf(data = PHL_boundary, fill = "grey40") +
  geom_sf(aes(fill = cluster), color = 'transparent') +
  scale_fill_brewer(palette = "Accent") +
  labs(title = "K-Means Corridor Clusters (n=4)") +
  mapTheme()

#Physical characteristics
# 1. Pedestrian/Transit Corridor - sidewalk oriented, continuous street- wall, separate property ownership, on-street parking.
# 2. Auto- Oriented Strip - separate property ownership, setbacks, frequent curb cuts, free off-street parking
# 3. Free- Standing Center - coordinated development & ownership, auto-oriented, internal circulation, setbacks free off-street parking
# 4. Specialty Center - Self-contained complex or cluster, stands as its own destination or complements other primary destination (e.g. airport)
# 5. Mixed Character - strongly exhibits two or more physical and functional characters

phl_corridors %>%
  mutate(PHYS_CHAR = as.factor(PHYS_CHAR)) %>%
  ggplot() +
  geom_sf(data = PHL_boundary, fill = "grey40") +
  geom_sf(aes(fill = PHYS_CHAR), color = 'transparent') +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Corridor by Physical Characteristics") +
  mapTheme()

#Corridor type
# 1. Neighborhood Subcenter - 10,000 - 35,000 sq.ft. GLA convenience store grocery, pharmacy, dry cleaner, deli, etc.
# 2. Neighborhood Center - 30,000 - 120,000 sq.ft. GLA supermarket, variety store, bank, pharmacy, post office, etc.
# 3. Community Center - 100,000 - 500, 000 sq. ft. GLA discount dept store, home improvement, "big boxes" or equiv., "power center"
# 4. Regional Center - 300,000 - 900,000+ sq.ft. GLA one or two full-line department stores or equivalent
# 5. Superregional Center - 500,000 - 2,000,000+ sq.ft. GLA three or more full-line department stores or equivalent
# 6. Specialty Center - specialty goods or services, dining, bars, amusements, arts, etc.

phl_corridors %>%
  mutate(corr_type = as.factor(corr_type)) %>%
  ggplot() +
  geom_sf(data = PHL_boundary, fill = "grey50", color = "transparent") +
  geom_sf(aes(fill = corr_type), color = 'transparent') +
  scale_fill_viridis_d() +
  labs(title = "Corridor Type") +
  mapTheme()

dat_corr_type <- 
  dat2 %>% 
  st_join(phl_corridors, join = st_intersects) %>%
  drop_na(corr_type)

# Hours by corridor type
dat_hour <- 
  dat_corr_type %>% 
  select(safegraph_place_id, top_category, sub_category, popularity_by_hour, poi_cbg, median_dwell, corr_type) %>%
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
               values_to = "Popularity") %>%
  mutate(Hour = as.numeric(Hour),
         Popularity = as.numeric(Popularity))

dat_hour %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  group_by(Hour, corr_type) %>%
  summarize(Popularity = mean(Popularity)) %>%
  ggplot(., aes(x = Hour, y = Popularity)) + 
  geom_col() +
  labs(title = "Philadelphia Nightlife Establishments, Popularity by Hour",
       subtitle = "Figure X.X") +
  facet_wrap(~corr_type, scales = "free") +
  plotTheme()

#Bucketed dwell times
dat_dwell <-
  dat_corr_type %>% 
  select(safegraph_place_id, top_category, sub_category, bucketed_dwell_times, corr_type) %>%
  st_drop_geometry() %>%
  mutate(bucketed_dwell_times = str_remove_all(bucketed_dwell_times, pattern = "\\{|\\}")) %>%
  mutate(bucketed_dwell_times = str_remove_all(bucketed_dwell_times, pattern = '\\"|\\"')) %>%
  mutate(bucketed_dwell_times = str_remove_all(bucketed_dwell_times, pattern = '<5:|5-20:|21-60:|61-240:|>240:')) %>%
  unnest(bucketed_dwell_times) %>%
  separate(.,
           bucketed_dwell_times,
           c("<5","5-20","21-60","61-240",">240"),
           sep = ",") %>%
  pivot_longer(cols = 4:8,
               names_to = "Dwell_Time",
               values_to = "Popularity")%>%
  mutate(Popularity = as.numeric(Popularity),
         Dwell_Time = factor(dat_dwell$Dwell_Time, level = c("<5","5-20","21-60","61-240",">240")))

dat_dwell %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  group_by(Dwell_Time, corr_type) %>%
  summarize(Popularity = mean(Popularity)) %>%
  ggplot(., aes(x = Dwell_Time, y = Popularity)) + 
  geom_col() +
  labs(title = "Philadelphia Nightlife Establishments, Dwell Time",
       subtitle = "Figure X.X") +
  facet_wrap(~corr_type, scales = "free") +
  plotTheme()

dat_dwell %>%
  # filter(top_category == "Drinking Places (Alcoholic Beverages)" |
  #          top_category == "Restaurants and Other Eating Places" |
  #          top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
  #          top_category == "Performing Arts Companies") %>%
  group_by(Dwell_Time, corr_type) %>%
  summarize(Popularity = sum(Popularity)) %>%
  ggplot(aes(fill=Dwell_Time, y=Popularity, x=corr_type)) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() + 
  scale_fill_viridis_d() +
  labs(title = "Bucketed Dwell Time by Corridor Type",
       subtitle = "Figure X.X") +
  plotTheme()
  
# Popoularity by Day
dat_day <-
  dat_corr_type %>% 
  select(safegraph_place_id, top_category, sub_category, popularity_by_day, corr_type) %>%
  st_drop_geometry() %>%
  mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = "\\{|\\}")) %>%
  mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = '\\"|\\"')) %>%
  mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = 'Monday:|Tuesday:|Wednesday:|Thursday:|Friday:|Saturday:|Sunday:')) %>%
  unnest(popularity_by_day) %>%
  separate(.,
           popularity_by_day,
           c("Monday","Tuesday","Wednesday","Thursday","Friday", "Saturday", "Sunday"),
           sep = ",") %>%
  pivot_longer(cols = 4:10,
               names_to = "Day",
               values_to = "Popularity") %>%
  mutate(Popularity = as.numeric(Popularity),
         Day = factor(dat_day$Day, level = c("Monday","Tuesday","Wednesday","Thursday","Friday", "Saturday", "Sunday")))

dat_day %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  group_by(Day, corr_type) %>%
  summarize(Popularity = mean(Popularity)) %>%
  ggplot(., aes(x = Day, y = Popularity)) + 
  geom_col() +
  labs(title = "Philadelphia Nightlife Establishments, Popularity by Day",
       subtitle = "Figure X.X") +
  facet_wrap(~corr_type, scales = "free") +
  plotTheme()

dat_day %>%
  # filter(top_category == "Drinking Places (Alcoholic Beverages)" |
  #          top_category == "Restaurants and Other Eating Places" |
  #          top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
  #          top_category == "Performing Arts Companies") %>%
  group_by(Day, corr_type) %>%
  summarize(Popularity = sum(Popularity)) %>%
  ggplot(aes(fill=corr_type, y=Popularity, x=Day)) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() + 
  scale_fill_viridis_d() +
  labs(title = "Bucketed Dwell Time by Corridor Type",
       subtitle = "Figure X.X") +
  plotTheme()
