# Prediction model

########
# SETUP
########
library(sf)
library(tidyverse)
library(tidycensus)
library(FNN)
library(caret)
library(stargazer)
library(kableExtra)
library(viridis)
library(gridExtra)

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

#Boundaries shapefiles (projected & unprojected)
phl_boundary <- 
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728') 

#Districts
phl_dist <-
  st_read("http://data.phl.opendata.arcgis.com/datasets/0960ea0f38f44146bb562f2b212075aa_0.geojson", 
          quiet = TRUE) %>%
  st_transform('ESRI:102728')

#Neighborhood shapefile
phl_nhoods <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson", quiet = TRUE) %>%
  st_transform('ESRI:102728') %>%
  select(mapname) %>%
  rename(., neighborhood = mapname)

#SafeGraph Features
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

#Philadelphia Features
CenterCity.sf <- phl_dist %>% 
  select(DIST_NAME) %>%
  filter(DIST_NAME == "Central") %>%
  st_centroid()
  
septaStops <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/8c6e2575c8ad46eb887e6bb35825e1a6_0.geojson", quiet = TRUE) %>% 
      mutate(Line = "El") %>% 
      st_transform('ESRI:102728') %>%
      select(Station, Line),
    st_read("https://opendata.arcgis.com/datasets/2e9037fd5bef406488ffe5bb67d21312_0.geojson", quiet = TRUE) %>%
      mutate(Line ="Broad_St") %>%
      st_transform('ESRI:102728') %>%
      select(Station, Line)) 

parks <-
  st_read("https://opendata.arcgis.com/datasets/d52445160ab14380a673e5849203eb64_0.geojson", 
          quiet = TRUE) %>%
  st_transform('ESRI:102728') %>%
  select(PUBLIC_NAME)

#Building footprints shpfile 
phl_building_footprints <-
  st_read("https://opendata.arcgis.com/datasets/ab9e89e1273f445bb265846c90b38a96_0.geojson", 
          quiet = TRUE) %>%
  dplyr::rename(., 
                street_address = ADDRESS,
                Building_area = Shape__Area) %>%
  dplyr::select(street_address, Building_area) %>%
  st_transform('ESRI:102728')

st_c <- st_coordinates

#Demographic data
phl_blockgroups <- 
  get_acs(geography = "block group", 
          variables = c("B01003_001E", 
                        "B02001_002E", 
                        "B01002_001E",
                        "B19013_001E", 
                        "B25064_001E",
                        "B03002_012E",
                        "B02001_003E"),
          year=2018, 
          state=42, 
          county=101, 
          geometry=T, 
          output = "wide") %>%
  st_transform('ESRI:102728')%>%
  rename(TotalPop = B01003_001E,
         White = B02001_002E,
         MedAge = B01002_001E,
         MedHHInc = B19013_001E,
         MedRent = B25064_001E,
         Hisp = B03002_012E,
         Black = 	B02001_003E,
         GEOID10 = GEOID) %>%
  dplyr::select(-ends_with("M")) %>%
  mutate(pctWhite = ((White / TotalPop)*100),
         pctBlack = ((Black / TotalPop)*100),
         pctHisp = ((Hisp / TotalPop)*100),
         Context_Race = ifelse(pctWhite > .5, "Majority White", "Majority Non-White"),
         Context_Income = ifelse(MedHHInc > 46116, "High Income", "Low Income"),
         Context_Age = ifelse(MedAge > 34.5, "High Median Age", "Low Median Age"),
         Context_Rent = ifelse(MedRent > 1032, "High Median Rent", "Low Median Rent")) %>%
  st_as_sf()

# View(load_variables(2018, dataset = "acs5"))

dat2 <- st_join(dat2 %>% st_transform(crs=4326),
                phl_blockgroups %>%
                  st_transform(crs=4326),
                join=st_intersects, 
                left = TRUE) %>%
  st_transform('ESRI:102728')

################################
# FEATURE ENGINEERING - LATE HOURS
################################
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
  filter(Hour == "1" | Hour == "2" | Hour == "3" | 
           Hour == "4" | Hour == "5" |Hour == "6") %>%
  group_by(safegraph_place_id, date_range_start) %>%
  summarize(Hrs1_6 = sum(Count)) 

dat_7_12 <- dat_hour_unnest %>%
  filter(Hour == "7" | Hour == "8" | Hour == "9" | 
           Hour == "10" | Hour == "11" |Hour == "12") %>%
  group_by(safegraph_place_id, date_range_start) %>%
  summarize(Hrs7_12 = sum(Count)) 

dat_13_18 <- dat_hour_unnest %>%
  filter(Hour == "13" | Hour == "14" | Hour == "15" | 
           Hour == "16" | Hour == "17" | Hour == "18") %>%
  group_by(safegraph_place_id, date_range_start) %>%
  summarize(Hrs13_18 = sum(Count)) 

dat_19_0 <- dat_hour_unnest %>%
  filter(Hour == "19" | Hour == "20" | Hour == "21" | 
           Hour == "22" | Hour == "23" | Hour == "0") %>%
  group_by(safegraph_place_id, date_range_start) %>%
  summarize(Hrs19_0 = sum(Count)) 

dat_workday <- dat_hour_unnest %>%
  filter(Hour == "9" | Hour == "10" | Hour == "11" | 
           Hour == "12" | Hour == "13" | Hour == "14" |
           Hour == "15" | Hour == "16" | Hour == "17" |Hour == "18") %>%
  group_by(safegraph_place_id, date_range_start) %>%
  summarize(Hrs_workday = sum(Count))

#Join new dataset
dat3 <- 
  dat2 %>% 
  left_join(dat_1_6, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_7_12, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_13_18, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_19_0, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_workday, by = c('safegraph_place_id', 'date_range_start')) %>%
  st_join(phl_nhoods) %>%
  st_join(phl_corridors) %>%
  st_join(., phl_building_footprints) %>%
  drop_na(corridor)

# dat_geomcorr <- 
#   dat3 %>%
#   st_drop_geometry() %>%
#   left_join(phl_corridors %>% select(corridor)) %>%
#   drop_na(corridor) %>%
#   st_as_sf()

#Engineer SG variables
dat_pred <-
  dat3 %>%
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
         transit_nn = nn_function(st_c(st_centroid(dat3)), st_c(st_centroid(septaStops)), 2),
         bars_nn = nn_function(st_c(st_centroid(dat3)), st_c(st_centroid(bar.sf)), 4),
         rest_nn = nn_function(st_c(st_centroid(dat3)), st_c(st_centroid(restaurant.sf)), 4),
         arts_nn = nn_function(st_c(st_centroid(dat3)), st_c(st_centroid(arts.sf)), 4),
         college_nn = nn_function(st_c(st_centroid(dat3)), st_c(st_centroid(college.sf)), 1),
         sports_nn = nn_function(st_c(st_centroid(dat3)), st_c(st_centroid(sports.sf)), 1),
         hotels_nn = nn_function(st_c(st_centroid(dat3)), st_c(st_centroid(hotels.sf)), 4),
         casinos_nn = nn_function(st_c(st_centroid(dat3)), st_c(st_centroid(casinos.sf)), 1),
         parks_nn = nn_function(st_c(st_centroid(dat3)), st_c(st_centroid(parks)), 4),
         late_night = if_else(grepl("Late Night", dat3$category_tags), 1, 0),
         bar_pub = if_else(grepl("Bar or Pub", dat3$category_tags), 1, 0),
         drinks = if_else(grepl("Drinks", dat3$category_tags), 1, 0),
         cocktail = if_else(grepl("Cocktail Lounge", dat3$category_tags), 1, 0),
         SocHill = if_else(neighborhood == "Society Hill", 1, 0),
         UCity = if_else(neighborhood == "University City", 1, 0),
         NavyYard = if_else(neighborhood == "Navy Yard", 1, 0),
         PM_Workday = Hrs19_0 / Hrs_workday,
         AM_PM = (Hrs1_6 + Hrs7_12) / (Hrs13_18+Hrs19_0),
         Center_City = nn_function(st_c(st_centroid(dat3)), st_c(st_centroid(CenterCity.sf)), 1))%>% 
  mutate_if(is.numeric, list(~na_if(., Inf)))

#Aggregating by corridor
dat_pred_agg <- 
  dat_pred %>%
  group_by(corridor) %>%
  summarize(Night_visits = sum(Hrs19_0),
            Night_log = log(Night_visits),
            total = sum(total),
            phl_area = mean(area),
            phl_building_area = mean(Building_area, na.rm = TRUE),
            sg_visitors = sum(raw_visitor_counts),
            sg_visits = sum(raw_visit_counts),
            sg_dwell = mean(median_dwell, na.rm = TRUE),
            sg_distance_home = mean(distance_from_home, na.rm = TRUE),
            sg_workday = mean(PM_Workday, na.rm = TRUE),
            sg_AM_PM = mean(AM_PM, na.rm = TRUE),
            count_bars_a = sum(bars, na.rm = TRUE)/phl_area,
            count_rest_a = sum(restaurant, na.rm = TRUE)/phl_area,
            count_arts_a = sum(arts, na.rm = TRUE)/phl_area,
            # count_jrcol_a = sum(jrcol, na.rm = TRUE)/phl_area,
            count_college_a = sum(college, na.rm = TRUE)/phl_area,
            count_sports_a = sum(sports, na.rm = TRUE)/phl_area,
            count_museums_a = sum(museum, na.rm = TRUE)/phl_area,
            count_amuse_a = sum(amusement, na.rm = TRUE)/phl_area,
            count_hotels_a = sum(hotels, na.rm = TRUE)/phl_area,
            # count_bars_t = sum(bars, na.rm = TRUE)/total,
            # count_rest_t = sum(restaurant, na.rm = TRUE)/total,
            # count_arts_t = sum(arts, na.rm = TRUE)/total,
            # count_jrcol_t = sum(jrcol, na.rm = TRUE)/total,
            # count_college_t = sum(college, na.rm = TRUE)/total,
            # count_sports_t = sum(sports, na.rm = TRUE)/total,
            # count_museums_t = sum(museum, na.rm = TRUE)/total,
            # count_amuse_t = sum(amusement, na.rm = TRUE)/total,
            # count_hotels_t = sum(hotels, na.rm = TRUE)/total,
            # log_bars_a = log(sum(bars, na.rm = TRUE)/phl_area),
            # log_rest_a = log(sum(restaurant, na.rm = TRUE)/phl_area),
            # log_arts_a = log(sum(arts, na.rm = TRUE)/phl_area),
            # log_jrcol_a = log(sum(jrcol, na.rm = TRUE)/phl_area),
            # log_college_a = log(sum(college, na.rm = TRUE)/phl_area),
            # log_sports_a = log(sum(sports, na.rm = TRUE)/phl_area),
            # log_museums_a = log(sum(museum, na.rm = TRUE)/phl_area),
            # log_amuse_a = log(sum(amusement, na.rm = TRUE)/phl_area),
            # log_bars_t = log(sum(bars, na.rm = TRUE)/total),
            # log_rest_t = log(sum(restaurant, na.rm = TRUE)/total),
            # log_arts_t = log(sum(arts, na.rm = TRUE)/total),
            # log_jrcol_t = log(sum(jrcol, na.rm = TRUE)/total),
            # log_college_t = log(sum(college, na.rm = TRUE)/total),
            # log_sports_t = log(sum(sports, na.rm = TRUE)/total),
            # log_museums_t = log(sum(museum, na.rm = TRUE)/total),
            # log_amuse_t = log(sum(amusement, na.rm = TRUE)/total),
            nn_transit = mean(transit_nn),
            nn_CenterCity = mean(Center_City),
            nn_parks = mean(parks_nn),
            # nn_bars = mean(bars_nn),
            # nn_rest = mean(rest_nn),
            # nn_arts = mean(arts_nn),
            # nn_college = mean(college_nn),
            # nn_sports = mean(sports_nn),
            # nn_casinos = mean(casinos_nn),
            # nn_hotels = mean(hotels_nn),
            count_late_tag = sum(late_night, na.rm = TRUE)/phl_area,
            count_barpub_tag = sum(bar_pub, na.rm = TRUE)/phl_area,
            count_drinks_tag = sum(drinks, na.rm = TRUE)/phl_area,
            count_cocktail_tag = sum(cocktail, na.rm = TRUE)/phl_area,
            demo_pctWhite = mean(pctWhite, na.rm = TRUE),
            demo_pctBlack = mean(pctBlack, na.rm = TRUE),
            demo_pctHisp = mean(pctHisp, na.rm = TRUE),
            demo_medAge = mean(MedAge, na.rm = TRUE),
            demo_MHI = median(MedHHInc, na.rm = TRUE),
            demo_medrent = median(MedRent, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, list(~na_if(., Inf))) %>%
  st_drop_geometry() %>%
  left_join(phl_corridors %>% select(corridor)) %>% 
  drop_na(corridor, demo_MHI, demo_medrent) %>% #Some of the census variables aren't reporting for our block groups
  st_as_sf() 

#############
# CORR PLOTS
#############
#Philadelphia summary variables
dat_pred_agg %>%
  st_drop_geometry() %>%
  pivot_longer(., cols = starts_with("phl_"), names_to = "variable", values_to="value") %>%
  ggplot(aes(value, Night_visits)) +
  geom_point(size = .5) +
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~variable, ncol = 3, scales = "free") +
  labs(title = "Nighttime Visits as a function of Philadelphia Summary Variables",
       subtitle = "Figure X.X") +
  plotTheme()

#Safegraph summary variables
dat_pred_agg %>%
  st_drop_geometry() %>%
  pivot_longer(., cols = starts_with("sg_"), names_to = "variable", values_to="value") %>%
  ggplot(aes(value, Night_visits)) +
  geom_point(size = .5) +
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~variable, ncol = 3, scales = "free") +
  labs(title = "Nighttime Visits as a function of Gobal SafeGraph Summary Variables",
       subtitle = "Figure X.X") +
  plotTheme()

#Count variables
dat_pred_agg %>%
  st_drop_geometry() %>%
  pivot_longer(., cols = starts_with("count_"), names_to = "variable", values_to="value") %>%
  ggplot(aes(value, Night_visits)) +
  geom_point(size = .5) +
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~variable, ncol = 4, scales = "free") +
  labs(title = "Nighttime Visits as a function of Normalized Count Variables",
       subtitle = "Figure X.X") +
  plotTheme()

# #Log variables
# dat_pred_agg %>%
#   st_drop_geometry() %>%
#   pivot_longer(., cols = starts_with("log_"), names_to = "variable", values_to="value") %>%
#   ggplot(aes(value, Night_visits)) +
#   geom_point(size = .5) +
#   geom_smooth(method = "lm", se=F, colour = "#FA7800") +
#   facet_wrap(~variable, ncol = 3, scales = "free") +
#   labs(title = "Nighttime Visits as a function of Log Transformed Variables") +
#   plotTheme()

#NN variables
dat_pred_agg %>%
  st_drop_geometry() %>%
  pivot_longer(., cols = starts_with("nn_"), names_to = "variable", values_to="value") %>%
  ggplot(aes(value, Night_visits)) +
  geom_point(size = .5) +
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~variable, ncol = 3, scales = "free") +
  labs(title = "Nighttime Visits as a function of Nearest Neighbor Variables",
       subtitle = "Figure X.X") +
  plotTheme()

##Demographic Variables
dat_pred_agg %>%
  st_drop_geometry() %>%
  pivot_longer(., cols = starts_with("demo_"), names_to = "variable", values_to="value") %>%
  ggplot(aes(value, Night_visits)) +
  geom_point(size = .5) +
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~variable, ncol = 3, scales = "free") +
  labs(title = "Nighttime Visits as a function of Demographic Variables",
       subtitle = "Figure X.X") +
  plotTheme()

#################
# MODEL BUILDING
#################
# dat_pred_agg <- dat_pred_agg %>%
#   st_as_sf() %>%
#   select(-corridor) #Having issues including this variable below

#Setting up test and training datasets
inTrain <- createDataPartition(y=dat_pred_agg$Night_visits,
                               p = .60, list = FALSE)

phl.training <- dat_pred_agg[inTrain,] 
phl.test <- dat_pred_agg[-inTrain,]  

is.na(phl.training)

#Multivariate regression
colnames(dat_pred_agg)

reg1 <- lm(Night_visits ~ ., data = st_drop_geometry(phl.training) %>% 
             select(Night_visits, 
                    phl_area, 
                    phl_building_area,
                    sg_distance_home,
                    sg_dwell,
                    sg_workday,
                    sg_AM_PM,
                    count_bars_a,
                    count_rest_a,
                    count_arts_a,
                    count_college_a,
                    count_sports_a,
                    count_museums_a,
                    count_amuse_a,
                    count_hotels_a,
                    nn_transit,
                    nn_CenterCity,
                    nn_parks, 
                    count_late_tag,
                    count_barpub_tag,
                    count_drinks_tag, 
                    count_cocktail_tag,
                    demo_pctWhite,
                    demo_pctBlack, 
                    demo_pctHisp,
                    demo_medAge, 
                    demo_MHI,
                    demo_medrent))

stargazer(
  reg1,
  type = "text",
  title = "Linear Model Summary Table",
  dep.var.caption = " ",
  dep.var.labels = "Model Features")

phl.test <-
  phl.test %>%
  st_drop_geometry() %>%
  mutate(Regression = "Baseline Regression",
         Visits.Predict = predict(reg1, phl.test),
         Visits.Error = Visits.Predict - Night_visits,
         Visits.AbsError = abs(Visits.Predict - Night_visits),
         Visits.APE = (abs(Visits.Predict - Night_visits)) / Visits.Predict)

ErrorTable <- 
  phl.test %>% 
  dplyr::summarize(Regression = "Baseline Regression",
                   MAE = mean(Visits.AbsError, na.rm = T), 
                   MAPE = mean(Visits.AbsError, na.rm = T) / mean(Night_visits, na.rm = T)) 

ErrorTable %>% 
  group_by(Regression) %>%
  arrange(desc(MAE)) %>% 
  kable(caption = "MAE and MAPE for Test Set Data") %>% kable_styling()

#Cross Validation
reg1_predict <- predict(reg1, newdata = phl.test)

rmse.train <- caret::MAE(predict(reg1), phl.training$Night_visits)
rmse.test <- caret::MAE(reg1_predict, phl.test$Night_visits)

preds.train <- data.frame(pred   = predict(reg1),
                          actual = phl.training$Night_visits,
                          source = "training data")
preds.test  <- data.frame(pred   = reg1_predict,
                          actual = phl.test$Night_visits,
                          source = "testing data")

preds <- rbind(preds.train, preds.test)

ggplot(preds, aes(x = pred, y = actual, color = source)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") +
  geom_abline(color = "orange") +
  theme_bw() +
  facet_wrap(~source, ncol = 1) +
  labs(title = "Comparing predictions to actual values",
       x = "Predicted Value",
       y = "Actual Value",
       subtitle = "Figure X.X") +
  theme(
    legend.position = "none"
  )

#Generalizability
fitControl <- trainControl(method = "cv", 
                           number = 100,
                           savePredictions = TRUE)

set.seed(717)
reg1.cv <- 
  train(Night_visits ~ ., data = st_drop_geometry(dat_pred_agg) %>% 
          dplyr::select(Night_visits, 
                        phl_area, 
                        phl_building_area,
                        sg_distance_home,
                        sg_dwell,
                        sg_workday,
                        sg_AM_PM,
                        count_bars_a,
                        count_rest_a,
                        count_arts_a,
                        count_college_a,
                        count_sports_a,
                        count_museums_a,
                        count_amuse_a,
                        count_hotels_a,
                        nn_transit,
                        nn_CenterCity,
                        nn_parks, 
                        count_late_tag,
                        count_barpub_tag,
                        count_drinks_tag, 
                        count_cocktail_tag,
                        demo_pctWhite,
                        demo_pctBlack, 
                        demo_pctHisp,
                        demo_medAge, 
                        demo_MHI,
                        demo_medrent), 
        method = "lm", 
        trControl = fitControl, 
        na.action = na.pass)

reg1.cv 

#Standard Deviation and Histogram of MAE
reg1.cv.resample <- reg1.cv$resample

ggplot(reg1.cv.resample, aes(x=MAE)) + 
  geom_histogram(color = "grey40", fill = "#27fdf5", bins = 50) + 
  labs(title="Histogram of Mean Average Error Across 100 Folds",
       subtitle = "Figure 5.2") +
  plotTheme()

#Mapping Errors
cv_preds <- reg1.cv$pred

map_preds <- dat_pred_agg %>% 
  rowid_to_column(var = "rowIndex") %>% 
  left_join(cv_preds, by = "rowIndex") %>% 
  mutate(Visits.AbsError = abs(pred - Night_visits),
         PercentError = (Visits.AbsError / Night_visits)*100) 

ErrorPlot1 <- ggplot() +
  geom_sf(data = phl_boundary, fill = "grey40") +
  geom_sf(data = map_preds, aes(fill = q5(Visits.AbsError)), color = "transparent") +
  scale_fill_manual(values = palette5,
                      labels=qBr(map_preds,"Visits.AbsError"),
                      name="Quintile\nBreaks") +
  labs(title="Absolute Errors",
       subtitle = "Night Visit Predictions",
       caption = "Figure X.X") +
  mapTheme()

ErrorPlot2 <- ggplot() +
  geom_sf(data = phl_boundary, fill = "grey40") +
  geom_sf(data = map_preds, aes(fill = q5(PercentError)), color = "transparent") +
  scale_fill_manual(values = palette5,
                      labels=qBr(map_preds, "PercentError"),
                      name="Quintile\nBreaks") +
  labs(title="Percent Errors",
       subtitle = "Night Visit Predictions",
       caption = "Figure X.X") +
  mapTheme()

grid.arrange(ErrorPlot1, ErrorPlot2, ncol=2)