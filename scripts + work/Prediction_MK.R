# Prediction model

########
# SETUP
########
library(sf)
library(tidyverse)
library(FNN)

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

nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    dplyr::summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
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
  select(4:5,18, 'Shape__Area') %>%
  rename(., corridor = NAME,
         district = P_DIST,
         vacancy = VAC_RATE,
         area = Shape__Area) %>%
  mutate()

#Neighborhood shapefile
phl_nhoods <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson", quiet = TRUE) %>%
  st_transform('ESRI:102728') %>%
  select(mapname) %>%
  rename(., neighborhood = mapname)


#Features
bar.sf <- dat2 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)") %>%
  select(location_name) %>%
  st_as_sf(coords = "geometry", crs = 4326, agr = "constant")

restaurant.sf <- dat2 %>%
  filter(top_category == "Restaurants and Other Eating Places") %>%
  select(location_name) %>%
  st_as_sf()

arts.sf <- dat2 %>%
  filter(top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  select(location_name) %>%
  st_as_sf()

college.sf <- dat2 %>%
  filter(top_category == "Colleges, Universities, and Professional Schools") %>%
  select(location_name) %>%
  st_as_sf()

sports.sf <- dat2 %>%
  filter(top_category == "Spectator Sports") %>%
  select(location_name) %>%
  st_as_sf()

casinos.sf <- dat2 %>%
  filter(sub_category == "Casino Hotels") %>%
  select(location_name) %>%
  st_as_sf()

hotels.sf <- dat2 %>%
  filter(sub_category == "Hotels (except Casino Hotels) and Motels") %>%
  select(location_name) %>%
  st_as_sf()

st_c <- st_coordinates

################
# FEATURE ENGINEERING
################
dat_corr <-
  dat2 %>% 
  # filter(top_category == "Drinking Places (Alcoholic Beverages)" |
  #          top_category == "Restaurants and Other Eating Places" |
  #          top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
  #          top_category == "Performing Arts Companies") %>%
  st_join(phl_corridors, join = st_intersects) %>%
  st_join(phl_nhoods, join = st_intersects) %>%
  drop_na(corridor) 

# top_cat <-
#   dat_corr %>%
#   st_drop_geometry() %>%
#   group_by(top_category) %>%
#   summarise(count = sum(raw_visitor_counts))
# 
# cats <-
#   dat_corr %>%
#   select(top_category, sub_category) %>%
#   st_drop_geometry()
# 
cats %>%
  filter(top_category == 'Traveler Accomodation') %>%
  unique()

# same_day_brand <-
#   dat_corr %>%
#   select(corridor, related_same_day_brand) %>%
#   st_drop_geometry() %>%
#   mutate(related_same_day_brand = str_remove_all(related_same_day_brand, pattern = "\\{|\\}")) %>%
#   mutate(related_same_day_brand = str_remove_all(related_same_day_brand, pattern = "\\{|\\}")) %>%
#   mutate(related_same_day_brand = str_remove_all(related_same_day_brand, pattern = '\\"|\\"')) %>%
#   mutate(related_same_day_brand = str_replace_all(related_same_day_brand, pattern = '"', replacement = "")) %>%
#   mutate(related_same_day_brand = str_split(related_same_day_brand, pattern = ",")) %>%
#   unnest(related_same_day_brand) %>% #unnest visitor cbg column
#   separate(.,
#            related_same_day_brand,
#            c("brand", "count"),
#            sep = ":") %>% #separate count column from visitor cbg
#   mutate(count = as.numeric(count)) 

# same_day_brand %>%
#   drop_na() %>%
#   group_by(brand) %>%
#   summarize(count = sum()) 

# Physical characteristics
# Amenities
# Demographic & census data
# Clustering
  
dat_corr <-
  dat_corr %>%
  mutate(total = 1,
         bars = ifelse(top_category == 'Drinking Places (Alcoholic Beverages)', 1, 0),
         restaurant = ifelse(top_category == 'Restaurants and Other Eating Places', 1, 0),
         arts = ifelse(top_category == 'Promoters of Performing Arts, Sports, and Similar Events' |
                         top_category == 'Performing Arts Companies', 1, 0),
         restaurant = ifelse(top_category == 'Restaurants and Other Eating Places', 1, 0),
         grocery_all = ifelse(top_category == 'Grocery Stores', 1, 0),
         grocery = ifelse(sub_category == 'Supermarkets and Other Grocery (except Convenience) Stores', 1, 0),
         malls = ifelse(sub_category == 'Malls', 1, 0),
         amusement = ifelse(top_category == 'Other Amusement and Recreation Industries', 1, 0),
         jrcol = ifelse(top_category == 'Junior Colleges', 1, 0),
         college = ifelse(top_category == 'Colleges, Universities, and Professional Schools', 1, 0),
         sports = ifelse(top_category == 'Spectator Sports', 1, 0),
         museum = ifelse(top_category == 'Museums, Historical Sites, and Similar Institutions', 1, 0),
         hotels = ifelse(top_category == 'Traveler Accommodation', 1, 0),
         bars_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(bar.sf)), 4),
         rest_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(restaurant.sf)), 4),
         arts_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(arts.sf)), 4),
         college_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(college.sf)), 1),
         sports_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(sports.sf)), 1),
         hotels_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(hotels.sf)), 4),
         casinos_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(casinos.sf)), 1),
         # bars_nn3 = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(bar.sf)), 3),
         # rest_nn3 = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(restaurant.sf)), 3),
         # arts_nn3 = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(arts.sf)), 3),
         # bars_nn2 = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(bar.sf)), 2),
         # rest_nn2 = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(restaurant.sf)), 2),
         # arts_nn2 = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(arts.sf)), 2),
         # bars_nn1 = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(bar.sf)), 1),
         # rest_nn1 = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(restaurant.sf)), 1),
         # arts_nn1 = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(arts.sf)), 1),
         # wawa = if_else(grepl('Wawa', dat_corr$related_same_day_brand), 1, 0),
         # wendys = if_else(grepl("Wendy's", dat_corr$related_same_day_brand), 1, 0),
         late_night = if_else(grepl("Late Night", dat_corr$category_tags), 1, 0),
         bar_pub = if_else(grepl("Bar or Pub", dat_corr$category_tags), 1, 0))

corr_test <- 
  dat_corr %>%
  filter(date_range_start == '2018-06-01T04:00:00Z') %>%
  group_by(corridor, area) %>%
  summarize(visits = sum(raw_visit_counts),
            total = sum(total),
            area = mean(area),
            sg_visitors = sum(raw_visitor_counts),
            sg_dwell = mean(median_dwell, na.rm = TRUE),
            sg_distance_home = mean(distance_from_home, na.rm = TRUE),
            count_bars = sum(bars, na.rm = TRUE)/area,
            count_rest = sum(restaurant, na.rm = TRUE)/area,
            count_arts = sum(arts, na.rm = TRUE)/area,
            count_jrcol = sum(jrcol, na.rm = TRUE)/area,
            count_college = sum(college, na.rm = TRUE)/area,
            count_sports = sum(sports, na.rm = TRUE)/area,
            count_museums = sum(museum, na.rm = TRUE)/area,
            # corr_amuse = sum(amusement, na.rm = TRUE)/total,
            # corr_grocery_all = sum(grocery_all, na.rm = TRUE)/total,
            # corr_grocery = sum(grocery, na.rm = TRUE)/total,
            # corr_malls = sum(malls, na.rm = TRUE) / area,
            nn_bars = mean(bars_nn4),
            nn_rest = mean(rest_nn4),
            nn_arts = mean(arts_nn4),
            nn_college = mean(college_nn),
            nn_sports = mean(sports_nn),
            nn_casinos = mean(casinos_nn),
            nn_hotels = mean(hotels_nn),
            # bars_nn3 = mean(bars_nn3),
            # rest_nn3 = mean(rest_nn3),
            # arts_nn3 = mean(arts_nn3),
            # bars_nn2 = mean(bars_nn2),
            # rest_nn2 = mean(rest_nn2),
            # arts_nn2 = mean(arts_nn2),
            # bars_nn1 = mean(bars_nn1),
            # rest_nn1 = mean(rest_nn1),
            # arts_nn1 = mean(arts_nn1),\
            count_late_tag = sum(late_night, na.rm = TRUE)/area,
            count_barpub_tag = sum(bar_pub, na.rm = TRUE)/area
            )


#Safegraph summary variables
corr_test %>% 
  st_drop_geometry() %>%
  pivot_longer(., cols = starts_with("sg_"), names_to = "variable", values_to="value") %>%
  ggplot(aes(value, visits)) +
  geom_point(size = .5) + 
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~variable, ncol = 3, scales = "free") +
  labs(title = "Visits as a function of SafeGraph SUmmary Variables") +
  plotTheme()

#Count variables
corr_test %>% 
  st_drop_geometry() %>%
  pivot_longer(., cols = starts_with("count_"), names_to = "variable", values_to="value") %>%
  ggplot(aes(value, visits)) +
  geom_point(size = .5) + 
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~variable, ncol = 3, scales = "free") +
  labs(title = "Visits as a function of Continuous Count Variables") +
  plotTheme()

#Nearest neighbor variables
corr_test %>% 
  st_drop_geometry() %>%
  pivot_longer(., cols = starts_with("nn_"), names_to = "variable", values_to="value") %>%
  ggplot(aes(value, visits)) +
  geom_point(size = .5) + 
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~variable, ncol = 3, scales = "free") +
  labs(title = "Visits as a function of Nearest Neighbor Variables") +
  plotTheme()
