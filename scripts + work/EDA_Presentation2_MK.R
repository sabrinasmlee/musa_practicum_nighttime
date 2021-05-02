# EDA for Second Presentation

#######
# SETUP
#######
# Load pacakages
library(sf)
library(tidyverse)
library(viridis)

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
palette3 <- viridis_pal()(3)
palette5 <- viridis_pal()(5)
palette_con <- viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "D")

?viridis_pal
  
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

#Block group shapefiles (projected & unprojected)
phl_cbg <- 
  st_read("http://data.phl.opendata.arcgis.com/datasets/2f982bada233478ea0100528227febce_0.geojson", quiet = TRUE) %>%
  mutate(GEOID10 = as.numeric(GEOID10))%>%
  st_transform('ESRI:102728') 
phl_cbg_unproj <- 
  st_read("http://data.phl.opendata.arcgis.com/datasets/2f982bada233478ea0100528227febce_0.geojson", quiet = TRUE) %>%
  mutate(GEOID10 = as.numeric(GEOID10),
         Lat = as.numeric(INTPTLAT10),
         Lon = as.numeric(INTPTLON10))

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
         'P_DIST',
         'Shape__Area') %>%
  drop_na() %>%
  mutate(corr_type = ifelse(CORRIDOR_TYPE == 1, "1. Neighborhood Subcenter", 
                            ifelse(CORRIDOR_TYPE == 2, "2. Neighborhood Center", 
                                   ifelse(CORRIDOR_TYPE == 3, "3. Community Center", 
                                          ifelse(CORRIDOR_TYPE == 4, "4. Regional Center", 
                                                 ifelse(CORRIDOR_TYPE == 5, "5. Superregional Center", 
                                                        ifelse(CORRIDOR_TYPE == 6, "6. Speciality Center", "Other")))))))


#Boundaries shapefiles (projected & unprojected)
phl_boundary <- 
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728') 
phl_boundary_unproj <- 
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)

#Corridor shapefiles (projected & unprojected)
phl_corridors_unproj <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson", quiet = TRUE)

#Neighborhood shapefiles (projected & unprojected)
phl_nhoods <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson", quiet = TRUE) %>%
  select("mapname") %>%
  st_transform('ESRI:102728')
phl_nhoods_unproj <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson", quiet = TRUE)

#Planning district shapefiles (projected & unprojected)
phl_dist <-
  st_read("http://data.phl.opendata.arcgis.com/datasets/0960ea0f38f44146bb562f2b212075aa_0.geojson", quiet = TRUE) %>%
  st_transform('ESRI:102728')
phl_dist_unproj <-
  st_read("http://data.phl.opendata.arcgis.com/datasets/0960ea0f38f44146bb562f2b212075aa_0.geojson", quiet = TRUE)

#Creating indepednet variable from pop_by_hours column
#FYI - Takes some time!
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

#Creating variables for days of the week 
# MK NOTE:  This isn't working for me. Taking super long time. Not sure why
# dat_day_unnest <-
#   dat2 %>%
#   select(safegraph_place_id, date_range_start, popularity_by_day) %>%
#   mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = "\\{|\\}")) %>%
#   mutate(popularity_by_day = str_remove_all(popularity_by_day, pattern = '\\"|\\"')) %>%
#   mutate(popularity_by_day = str_split(popularity_by_day, pattern = ",")) %>%
#   unnest(popularity_by_day) %>%
#   separate(.,
#            popularity_by_day,
#            c("Day", "Count"),
#            sep = ":") %>%
#   mutate(Count = as.numeric(Count))
# 
# dat_week <- dat_day_unnest %>% #Takes a really long time!
#   filter(Day == "Monday" | Day == "Tuesday" | Day == "Wednesday" | Day == "Thurday" | Day == "Friday") %>%
#   group_by(safegraph_place_id, date_range_start) %>%
#   summarize(Weekday = sum(Count))
# 
# dat_weekend <- dat_day_unnest %>% #Takes a really long time!
#   filter(Day == "Saturday" | Day == "Sunday") %>%
#   group_by(safegraph_place_id, date_range_start) %>%
#   summarize(Weekend = sum(Count))

#Join new dataset
dat3 <- 
  dat2 %>% 
  left_join(dat_1_6, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_7_12, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_13_18, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_19_0, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_workday, by = c('safegraph_place_id', 'date_range_start')) %>%
  st_join(phl_corridors, join = st_intersects) %>%
  st_join(phl_nhoods, join = st_intersects) %>%
  mutate(WorkDay_Evening_Ratio =  Hrs_workday / Hrs19_0)

######
# VIZ
######

mapTheme_dark <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "white"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    plot.background = element_rect(fill = "black")
  )
}


##----1. Corridor typologies
# 1. Neighborhood Subcenter - 10,000 - 35,000 sq.ft. GLA convenience store grocery, pharmacy, dry cleaner, deli, etc.
# 2. Neighborhood Center - 30,000 - 120,000 sq.ft. GLA supermarket, variety store, bank, pharmacy, post office, etc.
# 3. Community Center - 100,000 - 500, 000 sq. ft. GLA discount dept store, home improvement, "big boxes" or equiv., "power center"
# 4. Regional Center - 300,000 - 900,000+ sq.ft. GLA one or two full-line department stores or equivalent
# 5. Superregional Center - 500,000 - 2,000,000+ sq.ft. GLA three or more full-line department stores or equivalent
# 6. Specialty Center - specialty goods or services, dining, bars, amusements, arts, etc.

#V1 - 1 map
phl_corridors %>%
  ggplot() +
  geom_sf(data = phl_boundary, fill = "grey80", color = "grey40") +
  geom_sf(aes(fill = corr_type), color = "transparent") +
  scale_fill_viridis_d() +
  labs(title = "Philadelphia Corridor Typologies",
       subtitle = "Figure X.X") +
  mapTheme() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

#v2 - faceted map
phl_corridors %>%
  ggplot() +
  geom_sf(data = phl_boundary, fill = "white", color = "grey60") +
  geom_sf(aes(fill = corr_type), color = "transparent") +
  scale_fill_viridis_d() +
  facet_wrap(~corr_type, ncol =6) +
  mapTheme() + theme(legend.position = "none")

#Piechart - count
phl_corridors %>%
  group_by(corr_type) %>%
  summarize(Count = n()) %>%
  ggplot(aes(x="", y=Count, fill=corr_type)) +
  geom_bar(stat = "identity", width = 1, color="white") +
  coord_polar("y", start = 0, direction = -1) +
  scale_fill_viridis_d() +
  # labs(title = "Count of Philadelphia Commercial Corridors") +
  theme_void() + theme(legend.position = "none")

factor(dfc$Make, levels = rev(as.character(dfc$Make)))


mutate(contamination = factor(x = contamination,
                              levels = contamination), # ideally you should specify the order explicitly
       label = factor(x = label,
                      levels = label)) %>% # same as above note
  
  #Piechart - Area
  phl_corridors %>%
  group_by(corr_type) %>%
  summarize(Area = sum(Shape__Area)) %>%
  ggplot(aes(x="", y=Area, fill=corr_type)) +
  geom_bar(stat = "identity", width = 1, color="white") +
  coord_polar("y", start = 0, direction = -1) +
  scale_fill_viridis_d() +
  # labs(title = "Total Area of Philadelphia Commercial Corridors") +
  theme_void() +
  theme(legend.position = "none")

#Piechart - vistis
dat3 %>%
  drop_na(corr_type) %>%
  group_by(corr_type) %>%
  summarize(Visits = sum(raw_visit_counts)) %>%
  mutate(lab.ypos = cumsum(Visits) - 0.5*Visits) %>%
  st_drop_geometry() %>%
  ggplot(aes(x="", y=Visits, fill=corr_type)) +
  geom_bar(stat = "identity", width = 1, color="white") +
  coord_polar("y", start = 0, direction = -1) +
  scale_fill_viridis_d() +
  # labs(title = "Visits to Philadelphia Commercial Corridors",
  #      subtitle = "Figure X.X") +
  theme_void() + theme(legend.position = "none")

##----2. Popularity throughout time
#Total Daily Traffic Volume by Corridor Type
dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  group_by(corr_type) %>%
  summarise(Early_AM = sum(Hrs1_6),
            Late_AM = sum(Hrs7_12),
            Early_PM = sum(Hrs13_18),
            Late_PM = sum(Hrs19_0)) %>%
  pivot_longer(cols = 2:5,
               names_to = "Time",
               values_to = "Traffic") %>%
  ggplot(aes(fill=factor(Time, levels=c("Late_PM", 
                                        "Early_PM", 
                                        "Late_AM", 
                                        "Early_AM")), 
             y=Traffic, 
             x=factor(corr_type, levels=c("6. Speciality Center",
                                          "5. Superregional Center",
                                          "4. Regional Center",
                                          "3. Community Center",
                                          "2. Neighborhood Center",
                                          "1. Neighborhood Subcenter")))) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  scale_fill_viridis_d() +
  # labs(title = "Total Daily Traffic Volume by Corridor Type",
  #      subtitle = "Figure X.X") +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))


#Total Daily Traffic Volume for Nightlife Establishments by Corridor Type
dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  mutate(category = ifelse(top_category == "Drinking Places (Alcoholic Beverages)", "Bars",
                           ifelse(top_category == "Restaurants and Other Eating Places", "Restaurants",
                                  ifelse(top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
                                           top_category == "Performing Arts Companies", "Arts", "Other")))) %>%
  group_by(corr_type, category) %>%
  summarise(Hrs1_6 = sum(Hrs1_6),
            Hrs7_12 = sum(Hrs7_12),
            Hrs13_18 = sum(Hrs13_18),
            Hrs19_0 = sum(Hrs19_0)) %>%
  pivot_longer(cols = 3:6,
               names_to = "Time",
               values_to = "Traffic") %>%
  ggplot(aes(fill=factor(Time, levels=c("Hrs19_0", "Hrs13_18", "Hrs7_12", "Hrs1_6")), 
             y=Traffic, 
             x=category)) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Total Daily Traffic Volume by Corridor Type",
       subtitle = "Figure X.X") +
  facet_wrap(~corr_type) +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme()

#Ratio of Workday to Evening
#Quintile Breakdown
dat3 %>%
  drop_na(NAME.y) %>%
  filter_at(vars(WorkDay_Evening_Ratio), all_vars(!is.infinite(.))) %>%
  group_by(NAME.y) %>%
  summarize(WorkDay_Evening_Ratio = mean(WorkDay_Evening_Ratio, na.rm = TRUE)) %>%
  st_drop_geometry() %>%
  rename("NAME" = "NAME.y") %>%
  left_join(., phl_corridors) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(data = phl_boundary, fill = "grey60", color = "transparent") +
  geom_sf(aes(fill = log(WorkDay_Evening_Ratio)), color = "transparent") +
  scale_fill_viridis() +
  # scale_fill_manual(values = palette5,
  #                   aesthetics = c("colour", "fill"),
  #                   name = "Workday to Evening Ratio \n(Quintile)") +
  # facet_wrap(~corr_type, ncol = 6) +
  mapTheme() +
  theme(legend.position = "bottom")

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
  # labs(title = "Average Workday to Evening Traffic Ratio by Corridor Type",
  #      subtitle = "Figure X.X") +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme()

##Evening to WorkdayRatio for Nightlife Establishments
dat3 %>%
  drop_na(NAME.y) %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  filter_at(vars(WorkDay_Evening_Ratio), all_vars(!is.infinite(.))) %>%
  group_by(NAME.y) %>%
  summarize(WorkDay_Evening_Ratio = mean(WorkDay_Evening_Ratio, na.rm = TRUE)) %>%
  st_drop_geometry() %>%
  rename("NAME" = "NAME.y") %>%
  left_join(., phl_corridors) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(data = phl_boundary, fill = "grey60", color = "transparent") +
  geom_sf(aes(fill = log(WorkDay_Evening_Ratio)), color = "transparent") +
  scale_fill_viridis() +
  # scale_fill_manual(values = palette5,
  #                   aesthetics = c("colour", "fill"),
  #                   name = "Workday to Evening Ratio \n(Quintile)") +
  # facet_wrap(~corr_type, ncol = 6) +
  mapTheme() +
  theme(legend.position = "bottom")

dat3 %>%
  st_drop_geometry() %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  drop_na(corr_type) %>%
  filter_at(vars(WorkDay_Evening_Ratio), all_vars(!is.infinite(.))) %>%
  group_by(corr_type) %>%
  summarise(WorkDay_Evening_Ratio = mean(WorkDay_Evening_Ratio, na.rm = TRUE)) %>%
  ggplot(aes(y=WorkDay_Evening_Ratio, 
             x=corr_type)) + 
  geom_bar(stat="identity") +
  scale_fill_viridis_d() +
  # labs(title = "Average Workday to Evening Traffic Ratio by Corridor Type",
  #      subtitle = "Figure X.X") +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme()

#---Alternative to workday ratio (confusing):  Evening Trip percentage by corridor type
# colnames(dat3)
dat3 %>%
  group_by(NAME.y) %>%
  summarize(Hrs1_6 = sum(Hrs1_6),
            Hrs7_12 = sum(Hrs7_12),
            Hrs13_18 = sum(Hrs13_18),
            Hrs19_0 = sum(Hrs19_0)) %>%
  mutate(TotalTrips = Hrs1_6 + Hrs7_12 + Hrs13_18 + Hrs19_0,
         Pct.EveningTrips = Hrs19_0 / TotalTrips *100) %>%
  st_drop_geometry() %>%
  dplyr::rename(NAME = NAME.y) %>%
  left_join(phl_corridors %>% select(NAME, geometry)) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(data = phl_boundary, fill = 'grey80', color = 'grey40') +
  geom_sf(aes(fill = q5(Pct.EveningTrips)), color = "transparent") +
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Share of Evening Trips \n(Quintile)") +
  # geom_sf(aes(fill = Pct.EveningTrips), color = "transparent") +
  # scale_fill_viridis() +
  mapTheme() +
  theme(legend.position = "bottom")

dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  group_by(corr_type) %>%
  summarize(Hrs1_6 = sum(Hrs1_6),
            Hrs7_12 = sum(Hrs7_12),
            Hrs13_18 = sum(Hrs13_18),
            Hrs19_0 = sum(Hrs19_0)) %>%
  mutate(TotalTrips = Hrs1_6 + Hrs7_12 + Hrs13_18 + Hrs19_0,
         Pct.EveningTrips = Hrs19_0 / TotalTrips *100) %>%
  ggplot(aes(y = Pct.EveningTrips, x = corr_type)) +
  geom_bar(stat = "identity") + 
  plotTheme() +
  scale_x_discrete(name ="Corridor Type")+  
  scale_y_continuous(name ="Percent of Trips during Evening")

#---Percent Workday traffic

dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  group_by(corr_type) %>%
  summarize(Hrs1_6 = sum(Hrs1_6),
            Hrs7_12 = sum(Hrs7_12),
            Hrs13_18 = sum(Hrs13_18),
            Hrs19_0 = sum(Hrs19_0),
            Hrs_workday = sum(Hrs_workday)) %>%
  mutate(TotalTrips = Hrs1_6 + Hrs7_12 + Hrs13_18 + Hrs19_0,
         Pct.WorkdayTrips = Hrs_workday / TotalTrips *100) %>%
  ggplot(aes(y = Pct.WorkdayTrips, x = corr_type)) +
  geom_bar(stat = "identity") + 
  scale_x_discrete(name ="Corridor Type")+  
  scale_y_continuous(name ="Percent of Trips during Workday") +
  plotTheme()


dat3 %>%
  group_by(NAME.y) %>%
  summarize(Hrs1_6 = sum(Hrs1_6),
            Hrs7_12 = sum(Hrs7_12),
            Hrs13_18 = sum(Hrs13_18),
            Hrs19_0 = sum(Hrs19_0),
            Hrs_workday = sum(Hrs_workday)) %>%
  mutate(TotalTrips = Hrs1_6 + Hrs7_12 + Hrs13_18 + Hrs19_0,
         Pct.WorkdayTrips = Hrs_workday / TotalTrips *100) %>%
  st_drop_geometry() %>%
  dplyr::rename(NAME = NAME.y) %>%
  left_join(phl_corridors %>% select(NAME, geometry)) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(data = phl_boundary, fill = 'grey80', color = 'grey40') +
  geom_sf(aes(fill = q5(Pct.WorkdayTrips)), color = "transparent") +
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Workday Trip Share \n(Quintile)") +
  # geom_sf(aes(fill = Pct.WorkdayTrips), color = "transparent") +
  # scale_fill_viridis() +
  mapTheme()+
  theme(legend.position = "bottom")
  

###POPULARITY BY NIGHTLIFE ESTABLISHMENT TYPE
dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  mutate(category = ifelse(top_category == "Drinking Places (Alcoholic Beverages)", "Bars",
                           ifelse(top_category == "Restaurants and Other Eating Places", "Restaurants",
                                  ifelse(top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
                                           top_category == "Performing Arts Companies", "Arts", "Other")))) %>%
  group_by(category, corr_type) %>%
  summarise(Visits = sum(raw_visit_counts),
            Visitors = sum(raw_visitor_counts),
            Ratio = Visits/Visitors) %>%
  pivot_longer(cols = 3:4,
               names_to = "metric",
               values_to = "value") %>%
  ggplot(aes(fill = metric,
             y=value, 
             x=category)) + 
  geom_bar(position="dodge", stat="identity") +
  # geom_text(aes(label = Ratio), hjust = -0,1, vjust = -1)+
  scale_fill_viridis_d() +
  # labs(title = "Total Traffic by Business & Corridor Type",
  #      subtitle = "Figure X.X") +
  facet_wrap(~corr_type) +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme()

##----3. Distance from Home
#Quintile transformation
dat3 %>%
  drop_na(NAME.y) %>%
  group_by(NAME.y) %>%
  summarize(distance_from_home = mean(distance_from_home, na.rm = TRUE)) %>%
  st_drop_geometry() %>%
  rename("NAME" = "NAME.y") %>%
  left_join(., phl_corridors) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(data = phl_boundary, fill = "grey80", color = "grey40") +
  geom_sf(aes(fill = q5(distance_from_home)), color = "transparent") +
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Distance from Home (Quintile)") +
  # labs(title = "Distance from Home",
  #      subtitle = "Figure X.X") +
  # facet_wrap(~corr_type) +
  mapTheme() +
  theme(legend.position = "bottom")

#Barplots
dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  # filter(top_category == "Drinking Places (Alcoholic Beverages)" |
  #          top_category == "Restaurants and Other Eating Places" |
  #          top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
  #          top_category == "Performing Arts Companies") %>%
  # mutate(category = ifelse(top_category == "Drinking Places (Alcoholic Beverages)", "Bars",
  #                          ifelse(top_category == "Restaurants and Other Eating Places", "Restaurants",
  #                                 ifelse(top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
  #                                          top_category == "Performing Arts Companies", "Arts", "Other")))) %>%
  group_by(corr_type) %>%
  summarise(Avg_Distance_m = mean(distance_from_home, na.rm = TRUE)) %>%
  ggplot(aes(y=Avg_Distance_m, x = corr_type)) + 
  geom_bar(stat="identity") +
  scale_fill_viridis_d() +
  # labs(title = "Distance from Home",
  #      subtitle = "Figure X.X") +
  # facet_wrap(~corr_type) +
  guides(fill=guide_legend(title=NULL)) +
  theme(axis.title.x = element_blank()) +
  plotTheme() 

##----4. Median Dwell Time
#Quintile transformation
dat3 %>%  drop_na(NAME.y, corr_type) %>%
  group_by(NAME.y) %>%
  summarize(median_dwell = median(median_dwell, na.rm = TRUE)) %>%
  st_drop_geometry() %>%
  rename("NAME" = "NAME.y") %>%
  left_join(., phl_corridors) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(data = phl_boundary, fill = "grey60", color = "transparent") +
  geom_sf(aes(fill = median_dwell), color = "transparent") +
  # scale_fill_manual(values = palette5,
  #                   aesthetics = c("colour", "fill"),
  #                   name = "Median Dwell \n(Quintile)") +
  mapTheme()

#Barplots
dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  mutate(category = ifelse(top_category == "Drinking Places (Alcoholic Beverages)", "Bars",
                           ifelse(top_category == "Restaurants and Other Eating Places", "Restaurants",
                                  ifelse(top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
                                           top_category == "Performing Arts Companies", "Arts", "Other")))) %>%
  group_by(corr_type, category) %>%
  summarise(Dwell = median(median_dwell, na.rm = TRUE)) %>%
  ggplot(aes(y=Dwell, 
             x=category)) + 
  geom_bar(stat="identity") +
  scale_fill_viridis_d() +
  labs(title = "Median Dwell by Establishment and Corridor Type",
       subtitle = "Figure X.X") +
  facet_wrap(~corr_type) +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme()

##----5. Origin & Destination
flows <- dat2 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>% #filter for business types
  mutate(category = ifelse(top_category == "Drinking Places (Alcoholic Beverages)", "Bars",
                           ifelse(top_category == "Restaurants and Other Eating Places", "Restaurants",
                                  ifelse(top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
                                           top_category == "Performing Arts Companies", "Arts", "Other")))) %>%
  st_join(phl_corridors) %>% #join destinations to phl corridors (apply corridor destination to each trip)
  st_as_sf() %>%
  st_drop_geometry() %>% 
  select(safegraph_place_id, 
         date_range_start,
         top_category,
         corr_type,
         poi_cbg,
         visitor_home_cbgs,
         NAME.y) %>% #select columns
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>% #unnest visitor cbg column
  separate(.,
           visitor_home_cbgs,
           c("visitor_cbg", "count"),
           sep = ":") %>% #separate count column from visitor cbg
  mutate(count = as.numeric(count),
         visitor_cbg = as.numeric(visitor_cbg)) %>%
  dplyr::rename(., corridor_dest = NAME.y) %>%
  drop_na(corridor_dest) #Remove trips with destinations outside of corridors

#prepare PHL boundary file to coordinates for plotting
PHL_boundary_unproj <- 
  phl_boundary_unproj %>% 
  st_coordinates() # split out coordinates 

PHL_boundary_unproj <-
  as.data.frame(PHL_boundary_unproj) # save as dataframe

#Generate CBG centroid
phl_cbg_cent <- 
  phl_cbg_unproj %>% 
  select(GEOID10, geometry) %>% 
  st_centroid()

#Generate neighborhood centroids
phl_nhood_cent <- 
  phl_nhoods_unproj %>% 
  select(name) %>%
  st_centroid()

#Generate corridor centroid
phl_corr_cent <- 
  phl_corridors_unproj %>% 
  select(NAME, geometry) %>% 
  st_centroid()

#Join CBG centroid to neighborhood shapefile
phl_cbg_nhood <- 
  st_join(phl_nhoods_unproj, phl_cbg_cent, join = st_intersects) %>%
  select(GEOID10, name) %>%
  st_centroid()

flows_nhood <- flows %>%
  left_join(phl_cbg_nhood, by=c("visitor_cbg"="GEOID10")) %>% #join visitor CBGs to nhoods
  dplyr::rename(., nhood_origin = name) %>% #cleanup columns for clarity
  drop_na(nhood_origin) %>% #dropping trips outside of Philadelphia
  st_as_sf() %>%
  st_drop_geometry() %>% 
  group_by(nhood_origin, top_category, corridor_dest, corr_type) %>% #grouping trip counts by origin neighborhood
  summarize(count = sum(count)) %>%
  left_join(phl_nhood_cent, by=c("nhood_origin"="name")) %>% #join origin nhood to nhood centroid
  left_join(., phl_corr_cent, by=c("corridor_dest"="NAME")) %>% #join destination corridor to corr centroid
  dplyr::rename(., origin.geom = geometry.x,
                dest.geom = geometry.y) #clean-up columns for clarity

#Convert from tibble to data frame for next step
flows_nhood <- as.data.frame(flows_nhood) 

#split point data into lat and long columns
flows_nhood <- flows_nhood %>% 
  mutate(lat.origin = unlist(map(flows_nhood$origin.geom,1)),
         long.origin = unlist(map(flows_nhood$origin.geom,2)),
         lat.dest = unlist(map(flows_nhood$dest.geom,1)),
         long.dest = unlist(map(flows_nhood$dest.geom,2)),
         id = as.character(c(1:nrow(.))))

#Faceted Map
flows_nhood %>% 
  filter(corridor_dest == "South Street/16th-21st" | #Neighborhood Subcenter
           # corridor_dest == "Fairmount/19th-25th" | #Neighborhood Center
           # corridor_dest == "Central Waterfront/Washington" | #Community Center
           corridor_dest == "Market West" #Regional Center
           # corridor_dest == "Market East" | #Superregional Center
           # corridor_dest == "Old City"
           ) %>% #Specialty Center
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y), fill = "grey20") + 
  geom_segment(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest,  
                   color=log(count)), 
               size = .5,
               lineend = "round") +
  scale_color_viridis() +
  coord_equal() +
  mapTheme() + 
  facet_wrap(~corr_type, ncol = 3)
  # + labs(title = "Trips to Sample Corridors in Central Philadelphia")


#CBG
#Preparing dataset to split out by individual cbgs
dat_cbg <- 
  dat2 %>%
  select(safegraph_place_id, 
         date_range_start, 
         top_category, 
         sub_category, 
         # poi_cbg, 
         visitor_home_cbgs, 
         geometry) %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>% #filter for nightlife establishments
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
         Visitors = as.numeric(Visitors))

phila_proj <- phila %>%
  st_transform('ESRI:102728')

cbg_origin <- #takes a really long time!
  dat_cbg %>%
  st_join(phl_corridors) %>%
  st_drop_geometry() %>% #drop geometry to join with cbg file
  dplyr::rename(., GEOID10 = Visitor_CBG) %>% #renaming to match cbg file
  left_join(phl_cbg, by = "GEOID10") %>% # join to cbg file
  select(safegraph_place_id, 
         GEOID10, 
         top_category,
         Visitors,
         NAME,
         corr_type,
         geometry) %>% #clean up dataset
  rename(., cbg_origin = GEOID10, #clean up column names
         geometry_origin = geometry)

cbg_origin2 <-
  cbg_origin %>%
  left_join(., phl_corridors, by = "NAME") %>% #join back to the SafeGraph locations
  select(safegraph_place_id,
         cbg_origin, 
         Visitors, 
         top_category,
         corr_type.x,
         NAME,
         geometry_origin, 
         geometry) %>%
  rename(., geometry_dest = geometry) %>%
  drop_na(geometry_origin, geometry_dest) #removing geometries that didn't match (ie outside philily)

cbg_origin2 <- cbg_origin2 %>%
  mutate(distance = mapply(st_distance, st_centroid(geometry_origin), geometry_dest)) #calculate distance

cbg_map <-
  cbg_origin2 %>%
  drop_na(distance) %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  filter(NAME == "South Street/16th-21st" | #Neighborhood Subcenter
           NAME == "Fairmount/19th-25th" | #Neighborhood Center
           NAME == "Central Waterfront/Washington" | #Community Center
           NAME == "Market West" | #Regional Center
           NAME == "Market East" | #Superregional Center
           NAME == "Old City") %>% #Specialty Center
  st_as_sf() %>%
  group_by(NAME, corr_type.x, cbg_origin) %>%
  summarize(avg_distance = weighted.mean(distance, Visitors))
  
cbg_map %>%
  ggplot() +
  geom_sf(data = phl_boundary, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = avg_distance), color = "transparent") + 
  scale_fill_viridis() +
  # scale_fill_manual(values = palette5,
  #                   aesthetics = c("colour", "fill"),
  #                   name = "Average Distance \n(Quintile)") +
  # labs(title = "How far do people travel to nightlife?",
  #      subtitle = "Figure X.X") +
  facet_wrap(~corr_type.x) +
  mapTheme()


#----6. Retail Mix
retail.mix <- dat3 %>%
  group_by(NAME.y) %>%
  summarize(retail.mix = n_distinct(top_category)) %>%
  dplyr::rename(NAME = NAME.y) %>%
  st_drop_geometry() %>%
  left_join(phl_corridors %>% select(NAME, geometry)) %>%
  st_as_sf()
  
retail.mix %>%
  ggplot() +
  geom_sf(data = phl_boundary, fill = "grey80", color = "grey40") +
  geom_sf(aes(fill = q5(retail.mix)), color = "transparent") +
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Retail Mix \n(Quintile)") +
  mapTheme()

