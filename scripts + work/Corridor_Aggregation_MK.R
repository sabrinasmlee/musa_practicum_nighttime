# Corridor Summary Metrics
# Safegraph Origins & Destinations

#######
# SETUP
#######
# Load pacakages
library(sf)
library(tidyverse)
library(tidycensus)
library(kableExtra)
library(viridis)

options(scipen=999)
options(tigris_class = "sf")

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "white"),
    plot.title = element_text(size = 14,colour = "white"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill="black"), #axis.title = element_blank(),
    panel.grid.major = element_line(color = "black"),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    legend.background = element_rect(fill = "black")
  )
}

setwd("~/GitHub/musa_practicum_nighttime")
#setwd("C:/Users/rawnb/Desktop/MUSA/Spring 2021/Practicum")

###########
#LOAD DATA
###########
dat2018 <- read.csv("./data/moves_2018.csv")
phila <- st_read("./demo/phila.geojson", quiet = TRUE) 
#dat2018 <- read.csv("./moves_2018.csv")
#phila <- st_read("./phila.geojson")

#Boundaries shapefiles (projected & unprojected)
PHL_boundary <- 
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728') 
PHL_boundary_unproj <- 
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)

#prepare PHL boundary file to coordinates for plotting
PHL_boundary_unproj <- 
  PHL_boundary_unproj %>% 
  st_coordinates() # split out coordinates 

PHL_boundary_unproj <-
  as.data.frame(PHL_boundary_unproj) # save as dataframe

#Corridor shapefiles (projected & unprojected)
phl_corridors <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728')
phl_corridors_unproj <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson", quiet = TRUE)

#Generate corridor centroid
phl_corr_cent <- 
  phl_corridors %>% 
  select(NAME, geometry) %>% 
  st_centroid()

phl_corridors_cent <- phl_corridors %>%
  dplyr::select(NAME, VAC_RATE) %>%
  st_drop_geometry() %>%
  left_join(., phl_corr_cent, by = "NAME") %>%
  mutate("centroid_corr" = geometry) 

#Plot corridors
phl_corridors %>%  
  ggplot() + 
  geom_sf( color = "white") + 
  scale_fill_distiller(palette="RdYlGn", direction = 1) +
  labs(title = "Restaurant Trip % Change April 2018-2020") +
  mapTheme()

#Census data
v18 <- load_variables(2018, "acs5", cache = TRUE)
# View(v18)

# census_api_key("c0e7f2bf1ed21adb8eca6c9652036dfd5c6e1040", overwrite = TRUE)
pa_blockgroups <- 
  get_acs(geography = "block group", 
          variables = c("B01003_001E", #select variables 
                        "B02001_002E", 
                        "B01002_001E",
                        "B19013_001E", 
                        "B25064_001E",
                        "B03002_012E",
                        "B02001_003E"),
          year=2018, #select year
          state=42, #PA FIPs code
          county=c(101, 017, 045, 091), #FIPS code for Philly, Bucks, Montgomery, Delaware
          geometry=T, 
          output = "wide") %>%
  st_transform('ESRI:102728') %>% #PA projection
  rename(TotalPop = B01003_001E, 
         White = B02001_002E,
         MedAge = B01002_001E,
         MedHHInc = B19013_001E,
         MedRent = B25064_001E,
         Hisp = B03002_012E,
         Black = 	B02001_003E,
         GEOID10 = GEOID) %>% #Clean up column names
  dplyr::select(-ends_with("M")) %>%
  mutate(pctWhite = ((White / TotalPop)*100),
         pctBlack = ((Black / TotalPop)*100),
         pctHisp = ((Hisp / TotalPop)*100),
         tract_area_mi = as.numeric(st_area(geometry)*0.000000035870),
         popdens_mi = TotalPop / tract_area_mi) %>% #Generate new columns
  st_as_sf()

nj_blockgroups <- 
  get_acs(geography = "block group", 
          variables = c("B01003_001E", 
                        "B02001_002E", 
                        "B01002_001E",
                        "B19013_001E", 
                        "B25064_001E",
                        "B03002_012E",
                        "B02001_003E"),
          year=2018, 
          state=c(34),
          county=c(007, 015, 005), #Camden, Gloucester and Burlington
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
         tract_area_mi = as.numeric(st_area(geometry)*0.000000035870),
         popdens_mi = TotalPop / tract_area_mi) %>%
  st_as_sf()

blockgroups2018 <- rbind(pa_blockgroups, nj_blockgroups)

blockgroups2018 <- blockgroups2018 %>%
  mutate(centroid_cbg = st_centroid(geometry),
         visitor_cbg = as.numeric(GEOID10)) %>%
  select(-GEOID10)

#county boundaries
pa_counties <- 
  get_acs(geography = "county", 
          variables = c("B01003_001E"),
          year=2018, #select year
          state=42, #PA FIPs code
          county=c(101, 017, 045, 091), #FIPS code for Philly, Bucks, Montgomery, Delaware
          geometry=T, 
          output = "wide") %>%
  st_transform('ESRI:102728') %>% #PA projection
  rename(TotalPop = B01003_001E) %>%
  dplyr::select(-ends_with("M")) %>%
  st_as_sf()

nj_counties <- 
  get_acs(geography = "county", 
          variables = c("B01003_001E"),
          year=2018, 
          state=c(34),
          county=c(007, 015, 005), #Camden, Gloucester and Burlington
          geometry=T, 
          output = "wide") %>%
  st_transform('ESRI:102728')%>%
  rename(TotalPop = B01003_001E) %>%
  dplyr::select(-ends_with("M")) %>%
  st_as_sf()

counties2018 <- rbind(pa_counties, nj_counties)

#Plot census blocks
blockgroups2018 %>%  
  ggplot() + 
  geom_sf(color = "white") + 
  geom_sf(data = counties2018, fill = "transparent", color = "red", lwd = 2) +
  scale_fill_distiller(palette="RdYlGn", direction = 1) +
  labs(title = "Philadelphia and Surrounding Counties") +
  mapTheme()

#Modify 2018 data
#Join to places dataset
dat_2018 <- dat2018 %>% 
  dplyr::select(safegraph_place_id, 
                raw_visit_counts,
                poi_cbg, 
                visitor_home_cbgs, 
                visitor_daytime_cbgs, 
                visitor_work_cbgs, 
                distance_from_home
                # ,
                # Month
                ) %>%
  left_join(., phila, by = "safegraph_place_id") %>% 
  st_as_sf() %>%
  st_transform('ESRI:102728') 

#Filter to just bars, restaurants, and performing arts and join to corridors
flows <- dat_2018 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>% #filter for business types
  mutate(category = case_when(top_category == "Drinking Places (Alcoholic Beverages)" ~ "Bars",
                              top_category == "Restaurants and Other Eating Places" ~ "Restaurants",
                              top_category == "Promoters of Performing Arts, Sports, and Similar Events" ~ "Arts",
                              top_category == "Performing Arts Companies" ~ "Arts")) %>%
  st_join(phl_corridors) %>% #join destinations to phl corridors (apply corridor destination to each trip)
  st_as_sf() %>%
  st_drop_geometry() %>% 
  select(safegraph_place_id, 
         # Month,
         category,
         poi_cbg,
         visitor_home_cbgs,
         distance_from_home,
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
  #mutate(distance = st_distance())
  dplyr::rename(., NAME = NAME.y) %>%
  drop_na(NAME) #Remove trips with destinations outside of corridors

#Join to corridors centroids
corr_summary <- flows %>%
  group_by(NAME, visitor_cbg) %>%
  summarize(Count = sum(count)-1,
            distance_from_home = mean(distance_from_home)) %>%
  left_join(., phl_corridors_cent, by = "NAME") 

#Join to blockgroups data
corr_summary1 <- corr_summary %>%
  right_join(., blockgroups2018, by = "visitor_cbg")  %>%
  dplyr::select(visitor_cbg, 
                NAME.x, 
                Count, 
                centroid_corr, 
                MedAge, 
                MedHHInc, 
                pctWhite, 
                pctBlack, 
                pctHisp, 
                centroid_cbg, 
                distance_from_home) %>%
  rename(NAME = NAME.x) 

#Group by destination corridor (One row per commercial corridor)
corr_group <- corr_summary1 %>%
  group_by(NAME) %>%
  summarize(total_trips = sum(Count),
            distance_from_home = mean(distance_from_home, na.rm = TRUE),
            weighted_age = weighted.mean(MedAge, Count, na.rm = TRUE),
            weighted_inc = weighted.mean(MedHHInc, Count, na.rm = TRUE),
            weighted_pctWhite = weighted.mean(pctWhite, Count, na.rm = TRUE),
            weighted_pctBlack = weighted.mean(pctBlack, Count, na.rm = TRUE),
            weighted_pctHisp = weighted.mean(pctHisp, Count, na.rm = TRUE))
         
#Rejoin to original corridor dataset for geometry (FINAL CORRIDOR DESTINATION DATASET)
corr_group_final <- corr_group %>%
  left_join(., phl_corridors %>% select(NAME, VAC_RATE), 
            by = "NAME") 

#Rejoin corridor origins to cbg dataset for geometry (FINAL CORRIDOR ORIGINS DATASET. EACH ROW IS ONE CORRIDOR/CBG COMBINATION)
corr_origins_final <- corr_summary1 %>%
  left_join(., corr_group %>% select(NAME, total_trips), by = "NAME") %>%
  dplyr::select(visitor_cbg,
                NAME,
                Count,
                MedAge,
                MedHHInc,
                pctWhite,
                pctBlack,
                pctHisp,
                distance_from_home,
                total_trips) %>%
  mutate(percent_of_trips = Count / total_trips) %>%
  left_join(., blockgroups2018 %>% select(visitor_cbg), by = "visitor_cbg")

#Mapping results
corr_origins_final %>%
  filter(NAME == "Market East") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(data = blockgroups2018, fill = "grey40", color = "transparent") +
  geom_sf(aes(fill = Count), color = "transparent") +
  geom_sf(data = counties2018, color = "white", fill = "transparent", lwd = .75) +
  scale_fill_viridis() +
  mapTheme()


####################################
# Combining predictions into dataset
####################################
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

st_c <- st_coordinates

#Load prediction data
dat2 <- dat2018 %>% 
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


#pull in relevant variables
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

sports.sf <- dat2 %>%
  filter(top_category == "Spectator Sports") %>%
  select(location_name) %>%
  st_as_sf()

#Philadelphia Features
phl_corridors_pred <- phl_corridors %>%
  st_as_sf() %>%
  dplyr::select(NAME, P_DIST, VAC_RATE) %>%
  rename(., corridor = NAME,
         district = P_DIST,
         vacancy_rate = VAC_RATE) %>%
  mutate(vacancy_rate = str_remove_all(vacancy_rate, pattern = "%"),
         vacancy_rate = str_remove_all(vacancy_rate, pattern = "\r\n"),
         vacancy_rate = as.numeric(vacancy_rate),
         corr_area_sqft = as.numeric(st_area(phl_corridors)),
         corr_area_sqmi = as.numeric(st_area(phl_corridors)*0.000000035870))

phl_corridors_pred$vacancy_rate <- phl_corridors_pred$vacancy_rate %>% replace_na(0)

phl_nhoods <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson", quiet = TRUE) %>%
  st_transform('ESRI:102728') %>%
  select(mapname) %>%
  rename(., neighborhood = mapname)

UPenn.sf <- 
  dat2 %>% 
  select(location_name, date_range_start, top_category) %>%
  filter(location_name == "Univ of Penn",
         top_category == "Colleges, Universities, and Professional Schools",
         date_range_start == "2018-01-01T05:00:00Z")

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

phl_building_footprints <-
  st_read("https://opendata.arcgis.com/datasets/ab9e89e1273f445bb265846c90b38a96_0.geojson", 
          quiet = TRUE) %>%
  dplyr::select(ADDRESS) %>%
  st_transform('ESRI:102728')

phl_building_footprints <- phl_building_footprints %>%
  mutate(bld_area_sqft = as.numeric(st_area(phl_building_footprints)),
         bld_area_sqmi = as.numeric(st_area(phl_building_footprints)*0.000000035870))

phl_trolley <-
  st_read("https://opendata.arcgis.com/datasets/e09e9f98bdf04eada214d2217f3adbf1_0.geojson") %>%
  st_transform('ESRI:102728') %>%
  .[PHL_boundary,] %>%
  filter(Mode == "Trolley")

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
         tract_area_mi = as.numeric(st_area(geometry)*0.000000035870),
         popdens_mi = TotalPop / tract_area_mi) %>%
  st_as_sf()

dat_pred_temp <- 
  dat2 %>% 
  left_join(dat_1_6, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_7_12, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_13_18, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_19_0, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_workday, by = c('safegraph_place_id', 'date_range_start')) %>%
  st_join(phl_blockgroups) %>%
  st_join(phl_nhoods) %>%
  st_join(phl_corridors_pred) %>%
  st_join(., phl_building_footprints) %>%
  drop_na(corridor)

#Engineer variables at the destination level.
# summary(dat_pred)
dat_pred <-
  dat_pred_temp %>%
  mutate(total = 1,
         bars = ifelse(top_category == 'Drinking Places (Alcoholic Beverages)', 1, 0),
         restaurant = ifelse(top_category == 'Restaurants and Other Eating Places', 1, 0),
         arts = ifelse(top_category == 'Promoters of Performing Arts, Sports, and Similar Events' |
                         top_category == 'Performing Arts Companies', 1, 0),
         restaurant = ifelse(top_category == 'Restaurants and Other Eating Places', 1, 0),
         grocery = ifelse(top_category == 'Grocery Stores', 1, 0),
         sports = ifelse(top_category == 'Spectator Sports', 1, 0),
         transit_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(septaStops)), 2),
         bars_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(bar.sf)), 4),
         rest_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(restaurant.sf)), 4),
         arts_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(arts.sf)), 4),
         sports_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(sports.sf)), 1),
         parks_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(parks)), 4),
         trolley_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(phl_trolley)), 4),
         late_night = if_else(grepl("Late Night", dat_pred_temp$category_tags), 1, 0),
         closing_time = ifelse(str_detect(open_hours, paste(c("20:00", "21:00", "22:00", "23:00", "0:00", "1:00", "2:00", "3:00"), collapse = "|")), "OPEN LATE", "NOT OPEN LATE"),
         closing_time = ifelse(is.na(closing_time) == TRUE, "NO DATA", closing_time),
         open_late = ifelse(closing_time == "OPEN LATE", 1, 0)
  )

dat_pred$UPenn <- as.numeric(st_distance(dat_pred, UPenn.sf))

#Aggregating by corridor
# summary(dat_pred_agg)
dat_pred_agg <- 
  dat_pred %>%
  group_by(corridor, date_range_start, corr_type) %>%
  summarize(Night_visits = sum(Hrs19_0),
            Workday_visits = sum(Hrs_workday),
            phl_area_sqmi = mean(corr_area_sqmi, na.rm = TRUE),
            Night_visits_sqmi = (Night_visits + 1) / phl_area_sqmi,
            Night_visits_sqmi_log = log(Night_visits_sqmi),
            Workday_visits_sqmi = (Workday_visits +1) / phl_area_sqmi,
            Workday_visits_sqmi_log = log(Workday_visits_sqmi),
            phl_building_size = mean(bld_area_sqft +1, na.rm = TRUE),
            phl_building_size_log = log(phl_building_size),
            phl_UPenn = mean(UPenn),
            sg_dwell = mean(median_dwell, na.rm = TRUE),
            sg_distance_home = mean(distance_from_home, na.rm = TRUE),
            sg_distance_home_log = log(sg_distance_home),
            count_bars_a = (sum(bars, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_rest_a = (sum(restaurant, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_arts_a = (sum(arts, na.rm = TRUE) +1)/phl_area_sqmi,
            count_sports_a = (sum(sports, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_grocery_a = (sum(grocery, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_retailmix_top = n_distinct(top_category),
            count_retailmix_sub = n_distinct(sub_category),
            count_late_tag = (sum(late_night, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_openlate = (sum(open_late, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_bars_a_log = log(count_bars_a),
            count_rest_a_log = log(count_rest_a),
            count_arts_a_log = log(count_arts_a),
            count_grocery_a_log = log(count_grocery_a),
            count_late_log = log(count_late_tag),
            count_retailmix_top_log = log(count_retailmix_top),
            count_retailmix_sub_log = log(count_retailmix_sub),
            count_openlate_log = log(count_openlate),
            nn_transit = mean(transit_nn),
            nn_parks = mean(parks_nn),
            nn_trolley = mean(trolley_nn),
            nn_parks_log = log(nn_parks),
            nn_transit_log = log(nn_transit),
            nn_busstop_log = log(nn_busstop),
            nn_trolley_log = log(nn_trolley),
            demo_popdens = mean(popdens_mi),
            demo_popdens_log = log(demo_popdens),
            demo_pctWhite = weighted.mean(pctWhite, Hrs19_0, na.rm = TRUE),
            demo_pctBlack = weighted.mean(pctBlack, Hrs19_0, na.rm = TRUE),
            demo_pctHisp = weighted.mean(pctHisp, Hrs19_0, na.rm = TRUE),
            demo_medAge = weighted.mean(MedAge, Hrs19_0, na.rm = TRUE),
            demo_MHI = weighted.mean(MedHHInc, Hrs19_0, na.rm = TRUE),
            demo_medrent = weighted.mean(MedRent, Hrs19_0, na.rm = TRUE),
            demo_pctHisp_log = log(demo_pctHisp + 1)) %>% 
  st_drop_geometry() %>%
  left_join(phl_corridors_pred %>% select(corridor)) %>%
  drop_na(corridor, demo_MHI, demo_medrent) %>% #Some of the census variables aren't reporting for our block groups
  st_as_sf() %>%
  ungroup() %>%
  na.omit(st_distance_home)

set.seed(414)
inTrain <- createDataPartition(y=dat_pred_agg$Night_visits_sqmi_log, p = .60, list = FALSE)

phl.training <- dat_pred_agg[inTrain,]
phl.test <- dat_pred_agg[-inTrain,]

reg.vars <- c('Night_visits_sqmi_log',
              'phl_building_size_log',
              'phl_UPenn',
              # 'phl_Temple',
              # 'phl_CityHall',
              # 'phl_CityHall_log',
              # 'phl_rest_sqft_log',
              # 'phl_bar_sqft_log',
              # 'phl_vacrate_log',
              # 'phl_CenterCity',
              'sg_distance_home_log',
              'sg_dwell',
              'count_bars_a_log',
              'count_rest_a_log',
              'count_arts_a_log',
              'count_grocery_a_log',
              # 'count_childcare_a_log',
              # 'count_religious_a_log',
              # 'count_bars_a',
              # 'count_rest_a',
              # 'count_arts_a',
              # 'count_college_a',
              'count_sports_a',
              'count_retailmix_top',
              'count_retailmix_sub_log',
              'count_openlate_log',
              # 'count_museums_a',
              # 'count_amuse_a_log',
              # 'count_hotels_a',
              'nn_transit_log',
              # 'nn_parking',
              'nn_parks_log',
              # 'nn_busstop',
              'nn_trolley',
              'count_late_tag',
              # 'count_barpub_tag',
              'corr_type',
              'count_late_log',
              # 'count_barpub_log',
              'demo_pctWhite',
              # 'demo_pctBlack',
              # 'demo_pctHisp_log',
              'demo_medAge',
              'demo_popdens',
              # 'demo_popdens_log',
              'demo_medrent',
              'demo_MHI')

#Multivariate regression
reg1 <- 
  lm(Night_visits_sqmi_log ~ ., #change lm to ranger, predictions are a little different
     data = st_drop_geometry(phl.training) %>% 
       select(reg.vars))

summary(reg1)


corr_group_final 
