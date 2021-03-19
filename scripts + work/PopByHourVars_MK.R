# Popularity_By_Hours Variables

########
# SETUP
########
library(sf)
library(tidyverse)
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

################
# DATA WRANGLING
################
dat_hour_unnest <- 
  dat2 %>% 
  select(safegraph_place_id, date_range_start, popularity_by_hour) %>%
  mutate(popularity_by_hour = str_remove_all(popularity_by_hour, pattern = "\\[|\\]")) %>%
  unnest(popularity_by_hour) %>%
  separate(.,
           popularity_by_hour,
           c("0", "1", "2", "3", "4", "5", "6", 
             "7", "8", "9", "10", "11", "12", 
             "13", "14", "15", "16", "17", "18",
             "19", "20", "21", "22", "23"),
           sep = ",") %>%
  pivot_longer(cols = 3:26,
               names_to = "Hour",
               values_to = "Count") %>%
  mutate(Hour = as.numeric(Hour),
         Count = as.numeric(Count))

dat_1_6 <- dat_hour_unnest %>%
  filter(Hour == "1" | 
           Hour == "2" | 
           Hour == "3" | 
           Hour == "4" | 
           Hour == "5" |
           Hour == "6") %>%
  group_by(safegraph_place_id, date_range_start) %>%
  summarize(Hrs1_6 = sum(Count)) 

dat_7_12 <- dat_hour_unnest %>%
  filter(Hour == "7" | 
           Hour == "8" | 
           Hour == "9" | 
           Hour == "10" | 
           Hour == "11" |
           Hour == "12") %>%
  group_by(safegraph_place_id, date_range_start) %>%
  summarize(Hrs7_12 = sum(Count)) 


dat_13_18 <- dat_hour_unnest %>%
  filter(Hour == "13" | 
           Hour == "14" | 
           Hour == "15" | 
           Hour == "16" | 
           Hour == "17" |
           Hour == "18") %>%
  group_by(safegraph_place_id, date_range_start) %>%
  summarize(Hrs13_18 = sum(Count)) 

dat_19_0 <- dat_hour_unnest %>%
  filter(Hour == "19" | 
           Hour == "20" | 
           Hour == "21" | 
           Hour == "22" | 
           Hour == "23" | 
           Hour == "0") %>%
  group_by(safegraph_place_id, date_range_start) %>%
  summarize(Hrs19_0 = sum(Count)) 

dat_workday <- dat_hour_unnest %>%
  filter(Hour == "9" | 
           Hour == "10" | 
           Hour == "11" | 
           Hour == "12" | 
           Hour == "13" | 
           Hour == "14" |
           Hour == "15" |
           Hour == "16" |
           Hour == "17" |
           Hour == "18") %>%
  group_by(safegraph_place_id, date_range_start) %>%
  summarize(Hrs_workday = sum(Count))

dat3 <- 
  dat2 %>% 
  left_join(dat_1_6, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_7_12, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_13_18, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_19_0, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_workday, by = c('safegraph_place_id', 'date_range_start')) %>%
  st_join(phl_corridors, join = st_intersects) %>%
  mutate(WorkDay_Evening_Ratio =  Hrs_workday / Hrs19_0)

#####
# VIZ
#####
#Average Daily Traffic Volume by Corridor Type
dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  group_by(corr_type) %>%
  summarise(Hrs1_6 = mean(Hrs1_6),
            Hrs7_12 = mean(Hrs7_12),
            Hrs13_18 = mean(Hrs13_18),
            Hrs19_0 = mean(Hrs19_0)) %>%
  pivot_longer(cols = 2:5,
               names_to = "Time",
               values_to = "Traffic") %>%
  ggplot(aes(fill=factor(Time, levels=c("Hrs19_0", "Hrs13_18", "Hrs7_12", "Hrs1_6")), 
             y=Traffic, 
             x=corr_type)) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Average Daily Traffic Volume by Corridor Type",
       subtitle = "Figure X.X") +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme()

#Median Daily Traffic Volume by Corridor Type
dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  group_by(corr_type) %>%
  summarise(Hrs1_6 = median(Hrs1_6),
            Hrs7_12 = median(Hrs7_12),
            Hrs13_18 = median(Hrs13_18),
            Hrs19_0 = median(Hrs19_0)) %>%
  pivot_longer(cols = 2:5,
               names_to = "Time",
               values_to = "Traffic") %>%
  ggplot(aes(fill=factor(Time, levels=c("Hrs19_0", "Hrs13_18", "Hrs7_12", "Hrs1_6")), 
             y=Traffic, 
             x=corr_type)) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Median Daily Traffic Volume by Corridor Type",
       subtitle = "Figure X.X") +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme()

#Workday to Evening Traffic Ratio by Corridor Type
dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  filter_at(vars(WorkDay_Evening_Ratio), all_vars(!is.infinite(.))) %>%
  group_by(corr_type) %>%
  summarise(WorkDay_Evening_Ratio = mean(WorkDay_Evening_Ratio, na.rm = TRUE)) %>%
  ggplot(aes(y=WorkDay_Evening_Ratio, 
             x=corr_type)) + 
  geom_bar(stat="identity") +
  scale_fill_viridis_d() +
  labs(title = "Average Workday to Evening Traffic Ratio by Corridor Type",
       subtitle = "Figure X.X") +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme()

dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  filter_at(vars(WorkDay_Evening_Ratio), all_vars(!is.infinite(.))) %>%
  group_by(corr_type) %>%
  summarise(WorkDay_Evening_Ratio = median(WorkDay_Evening_Ratio, na.rm = TRUE)) %>%
  ggplot(aes(y=WorkDay_Evening_Ratio, 
             x=corr_type)) + 
  geom_bar(stat="identity") +
  scale_fill_viridis_d() +
  labs(title = "Median Workday to Evening Traffic Ratio by Corridor Type",
       subtitle = "Figure X.X") +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme()

#Daily Traffic for All Business Types
dat3 %>%
  # filter(top_category == "Drinking Places (Alcoholic Beverages)" |
  #          top_category == "Restaurants and Other Eating Places" |
  #          top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
  #          top_category == "Performing Arts Companies") %>%
  # filter(raw_visit_counts > 20) %>%
  # filter(., grepl('Late Night', category_tags)) %>%
  st_drop_geometry() %>%
  group_by(top_category) %>%
  summarise(Hrs1_6 = mean(Hrs1_6),
            Hrs7_12 = mean(Hrs7_12),
            Hrs13_18 = mean(Hrs13_18),
            Hrs19_0 = mean(Hrs19_0)) %>%
  pivot_longer(cols = 2:5,
               names_to = "Time",
               values_to = "Traffic") %>%
  ggplot(aes(fill=factor(Time, levels=c("Hrs19_0", "Hrs13_18", "Hrs7_12", "Hrs1_6")), y=Traffic, x=top_category)) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Average Traffic by Hour for All Business Categories",
       subtitle = "Figure X.X") +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme()

dat3 %>%
  # filter(top_category == "Drinking Places (Alcoholic Beverages)" |
  #          top_category == "Restaurants and Other Eating Places" |
  #          top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
  #          top_category == "Performing Arts Companies") %>%
  # filter(raw_visit_counts > 20) %>%
  # filter(., grepl('Late Night', category_tags)) %>%
  st_drop_geometry() %>%
  group_by(top_category) %>%
  summarise(Hrs1_6 = median(Hrs1_6),
            Hrs7_12 = median(Hrs7_12),
            Hrs13_18 = median(Hrs13_18),
            Hrs19_0 = median(Hrs19_0)) %>%
  pivot_longer(cols = 2:5,
               names_to = "Time",
               values_to = "Traffic") %>%
  ggplot(aes(fill=factor(Time, levels=c("Hrs19_0", "Hrs13_18", "Hrs7_12", "Hrs1_6")), y=Traffic, x=top_category)) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Median Traffic by Hour for All Business Categories",
       subtitle = "Figure X.X") +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme()