#Creating prediction geojson (dummy version)
#Maddy Kornhauser
#4/20/2020

########
# SET UP
########
library(tidyverse)
library(tidycensus)
library(sf)
library(lubridate)
library(datetime)
library(gridExtra)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(FNN)
library(caret)
library(ranger)
library(rgdal)

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

#Boundaries shapefiles (projected & unprojected)
phl_boundary <- 
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728')

phl_boundary_unproj <- 
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)

#Corridor shapefiles (projected & unprojected)
phl_corridors <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728') %>%
  mutate(corr_type = ifelse(CORRIDOR_TYPE == 1, "1. Neighborhood Subcenter", 
                            ifelse(CORRIDOR_TYPE == 2, "2. Neighborhood Center", 
                                   ifelse(CORRIDOR_TYPE == 3, "3. Community Center", 
                                          ifelse(CORRIDOR_TYPE == 4, "4. Regional Center", 
                                                 ifelse(CORRIDOR_TYPE == 5, "5. Superregional Center", 
                                                        ifelse(CORRIDOR_TYPE == 6, "6. Speciality Center", "Other")))))))

phl_corridors_unproj <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson", quiet = TRUE)

#Neighborhood shapefiles (projected & unprojected)
phl_nhoods <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson", 
          quiet = TRUE) %>%
  st_transform('ESRI:102728')
phl_nhoods_unproj <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson", 
          quiet = TRUE)

##Unnesting hours column to create dependent variable
dat_hour_unnest <- 
  dat2 %>% 
  dplyr::select(safegraph_place_id, date_range_start, popularity_by_hour) %>%
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

dat_19_0 <- dat_hour_unnest %>%
  filter(Hour == "19" | 
           Hour == "20" | 
           Hour == "21" | 
           Hour == "22" | 
           Hour == "23" | 
           Hour == "0") %>%
  group_by(safegraph_place_id, date_range_start) %>%
  summarize(Hrs19_0 = sum(Count)) 

#SafeGraph Features
bar.sf <- dat2 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)") %>%
  dplyr::select(location_name) %>%
  st_as_sf(coords = "geometry", crs = 4326, agr = "constant")

restaurant.sf <- dat2 %>%
  filter(top_category == "Restaurants and Other Eating Places") %>%
  dplyr::select(location_name) %>%
  st_as_sf()

arts.sf <- dat2 %>%
  filter(top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  dplyr::select(location_name) %>%
  st_as_sf()

college.sf <- dat2 %>%
  filter(top_category == "Colleges, Universities, and Professional Schools") %>%
  dplyr::select(location_name) %>%
  st_as_sf()

sports.sf <- dat2 %>%
  filter(top_category == "Spectator Sports") %>%
  dplyr::select(location_name) %>%
  st_as_sf()

casinos.sf <- dat2 %>%
  filter(sub_category == "Casino Hotels") %>%
  dplyr::select(location_name) %>%
  st_as_sf()

hotels.sf <- dat2 %>%
  filter(sub_category == "Hotels (except Casino Hotels) and Motels") %>%
  dplyr::select(location_name) %>%
  st_as_sf()

parking.sf <- dat2 %>%
  filter(sub_category == "Parking Lots and Garages") %>%
  dplyr::select(location_name) %>%
  st_as_sf()

#Philadelphia Features
phl_corridors_pred <- phl_corridors %>%
  st_as_sf() %>%
  dplyr::select(4:5,18, 'corr_type') %>%
  rename(., corridor = NAME,
         district = P_DIST,
         vacancy_rate = VAC_RATE) %>%
  mutate(vacancy_rate = str_remove_all(vacancy_rate, pattern = "%"),
         vacancy_rate = str_remove_all(vacancy_rate, pattern = "\r\n"),
         vacancy_rate = as.numeric(vacancy_rate),
         corr_area_sqft = as.numeric(st_area(phl_corridors)),
         corr_area_sqmi = as.numeric(st_area(phl_corridors)*0.000000035870))

phl_corridors_pred$vacancy_rate <- phl_corridors_pred$vacancy_rate %>% replace_na(0)

phl_dist <-
  st_read("http://data.phl.opendata.arcgis.com/datasets/0960ea0f38f44146bb562f2b212075aa_0.geojson",
          quiet = TRUE) %>%
  st_transform('ESRI:102728')

phl_nhoods <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson", quiet = TRUE) %>%
  st_transform('ESRI:102728') %>%
  dplyr::select(mapname) %>%
  dplyr::rename(., neighborhood = mapname)

CenterCity.sf <- phl_dist %>% 
  dplyr::select(DIST_NAME) %>%
  filter(DIST_NAME == "Central") %>%
  st_centroid()

CityHall.sf <- 
  dat2 %>% 
  dplyr::select(location_name, date_range_start, top_category) %>%
  filter(location_name == "City Hall", date_range_start == "2018-01-01T05:00:00Z")

Temple.sf <- 
  dat2 %>% 
  dplyr::select(location_name, date_range_start, top_category) %>%
  filter(location_name == "Temple University", 
         top_category == "Colleges, Universities, and Professional Schools",
         date_range_start == "2018-01-01T05:00:00Z")

UPenn.sf <- 
  dat2 %>% 
  dplyr::select(location_name, date_range_start, top_category) %>%
  filter(location_name == "Univ of Penn",
         top_category == "Colleges, Universities, and Professional Schools",
         date_range_start == "2018-01-01T05:00:00Z")

septaStops <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/8c6e2575c8ad46eb887e6bb35825e1a6_0.geojson", quiet = TRUE) %>% 
      dplyr::mutate(Line = "El") %>% 
      st_transform('ESRI:102728') %>%
      dplyr::select(Station, Line),
    st_read("https://opendata.arcgis.com/datasets/2e9037fd5bef406488ffe5bb67d21312_0.geojson", quiet = TRUE) %>%
      dplyr::mutate(Line ="Broad_St") %>%
      st_transform('ESRI:102728') %>%
      dplyr::select(Station, Line)) 

parks <-
  st_read("https://opendata.arcgis.com/datasets/d52445160ab14380a673e5849203eb64_0.geojson", 
          quiet = TRUE) %>%
  st_transform('ESRI:102728') %>%
  dplyr::select(PUBLIC_NAME)

phl_building_footprints <-
  st_read("https://opendata.arcgis.com/datasets/ab9e89e1273f445bb265846c90b38a96_0.geojson", 
          quiet = TRUE) %>%
  dplyr::select(ADDRESS) %>%
  st_transform('ESRI:102728')

phl_building_footprints <- phl_building_footprints %>%
  dplyr::mutate(bld_area_sqft = as.numeric(st_area(phl_building_footprints)),
                bld_area_sqmi = as.numeric(st_area(phl_building_footprints)*0.000000035870))

phl_busstop <-
  st_read("https://opendata.arcgis.com/datasets/e09e9f98bdf04eada214d2217f3adbf1_0.geojson") %>%
  st_transform('ESRI:102728') %>%
  .[phl_boundary,] %>%
  filter(Mode == "Bus")

phl_trolley <-
  st_read("https://opendata.arcgis.com/datasets/e09e9f98bdf04eada214d2217f3adbf1_0.geojson") %>%
  st_transform('ESRI:102728') %>%
  .[phl_boundary,] %>%
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
  dplyr::rename(TotalPop = B01003_001E,
                White = B02001_002E,
                MedAge = B01002_001E,
                MedHHInc = B19013_001E,
                MedRent = B25064_001E,
                Hisp = B03002_012E,
                Black = 	B02001_003E,
                GEOID10 = GEOID) %>%
  dplyr::select(-ends_with("M")) %>%
  dplyr::mutate(pctWhite = ((White / TotalPop)*100),
                pctBlack = ((Black / TotalPop)*100),
                pctHisp = ((Hisp / TotalPop)*100),
                tract_area_mi = as.numeric(st_area(geometry)*0.000000035870),
                popdens_mi = TotalPop / tract_area_mi
                # Context_Race = ifelse(pctWhite > .5, "Majority White", "Majority Non-White"),
                # Context_Income = ifelse(MedHHInc > 46116, "High Income", "Low Income"),
                # Context_Age = ifelse(MedAge > 34.5, "High Median Age", "Low Median Age"),
                # Context_Rent = ifelse(MedRent > 1032, "High Median Rent", "Low Median Rent")
  ) %>%
  st_as_sf()

st_c <- st_coordinates

################
# DATA WRANGLING
################
dat_pred_temp <- 
  dat2 %>% 
  left_join(dat_19_0, by = c('safegraph_place_id', 'date_range_start')) %>%
  st_join(phl_blockgroups) %>%
  st_join(phl_nhoods) %>%
  st_join(phl_corridors_pred) %>%
  st_join(., phl_building_footprints) %>%
  drop_na(corridor)

dat_pred <-
  dat_pred_temp %>%
  dplyr::mutate(total = 1,
                bars = ifelse(top_category == 'Drinking Places (Alcoholic Beverages)', 1, 0),
                restaurant = ifelse(top_category == 'Restaurants and Other Eating Places', 1, 0),
                arts = ifelse(top_category == 'Promoters of Performing Arts, Sports, and Similar Events' |
                                top_category == 'Performing Arts Companies', 1, 0),
                restaurant = ifelse(top_category == 'Restaurants and Other Eating Places', 1, 0),
                grocery = ifelse(top_category == 'Grocery Stores', 1, 0),
                childcare = ifelse(top_category == "Child Day Care Services", 1, 0),
                gas = ifelse(top_category == "Gasoline Stations", 1, 0),
                religious = ifelse(top_category == "Religious Organization", 1, 0),
                personal = ifelse(top_category == "Personal Care Services", 1, 0),
                liquor = ifelse(top_category == "Beer, Wine, and Liquor Stores", 1, 0),
                amusement = ifelse(top_category == 'Other Amusement and Recreation Industries', 1, 0),
                college = ifelse(top_category == 'Colleges, Universities, and Professional Schools', 1, 0),
                sports = ifelse(top_category == 'Spectator Sports', 1, 0),
                museum = ifelse(top_category == 'Museums, Historical Sites, and Similar Institutions', 1, 0),
                hotels = ifelse(top_category == 'Traveler Accommodation', 1, 0),
                transit_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(septaStops)), 2),
                bars_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(bar.sf)), 4),
                rest_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(restaurant.sf)), 4),
                arts_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(arts.sf)), 4),
                college_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(college.sf)), 1),
                sports_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(sports.sf)), 1),
                hotels_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(hotels.sf)), 4),
                casinos_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(casinos.sf)), 1),
                parks_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(parks)), 4),
                # parking_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(parking.sf)), 1),
                busstop_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(phl_busstop)), 4),
                trolley_nn = nn_function(st_c(st_centroid(dat_pred_temp)), st_c(st_centroid(phl_trolley)), 4),
                late_night = if_else(grepl("Late Night", dat_pred_temp$category_tags), 1, 0),
                bar_pub = if_else(grepl("Bar or Pub", dat_pred_temp$category_tags), 1, 0),
                drinks = if_else(grepl("Drinks", dat_pred_temp$category_tags), 1, 0),
                cocktail = if_else(grepl("Cocktail Lounge", dat_pred_temp$category_tags), 1, 0),
                SocHill = if_else(neighborhood == "Society Hill", 1, 0),
                UCity = if_else(neighborhood == "University City", 1, 0),
                NavyYard = if_else(neighborhood == "Navy Yard", 1, 0),
                restaurant_sqft = ifelse(top_category == "Restaurants and Other Eating Places", bld_area_sqft, 0),
                bars_sqft = ifelse(top_category == "Drinking Places (Alcoholic Beverages)", bld_area_sqft, 0),
                closing_time = ifelse(str_detect(open_hours, paste(c("20:00", "21:00", "22:00", "23:00", "0:00", "1:00", "2:00", "3:00"), collapse = "|")), "OPEN LATE", "NOT OPEN LATE"),
                closing_time = ifelse(is.na(closing_time) == TRUE, "NO DATA", closing_time),
                open_late = ifelse(closing_time == "OPEN LATE", 1, 0)
  )

dat_pred$CenterCity <- as.numeric(st_distance(dat_pred, CenterCity.sf))
dat_pred$CityHall <- as.numeric(st_distance(dat_pred, CityHall.sf))
dat_pred$Temple <- as.numeric(st_distance(dat_pred, Temple.sf))
dat_pred$UPenn <- as.numeric(st_distance(dat_pred, UPenn.sf))

#Aggregating by corridor
# summary(dat_pred_agg)
dat_pred_agg <- 
  dat_pred %>%
  group_by(corridor, date_range_start, corr_type) %>%
  summarize(Night_visits = sum(Hrs19_0),
            # Workday_visits = sum(Hrs_workday),
            phl_area_sqmi = mean(corr_area_sqmi, na.rm = TRUE),
            Night_visits_sqmi = (Night_visits + 1) / phl_area_sqmi,
            Night_visits_sqmi_log = log(Night_visits_sqmi),
            # Workday_visits_sqmi = (Workday_visits +1) / phl_area_sqmi,
            # Workday_visits_sqmi_log = log(Workday_visits_sqmi),
            total = sum(total),
            phl_building_size = mean(bld_area_sqft +1, na.rm = TRUE),
            phl_building_size_log = log(phl_building_size),
            phl_building_size_med = median(bld_area_sqft, na.rm = TRUE),
            # phl_CenterCity = mean(CenterCity),
            # phl_CityHall = mean(CityHall),
            # phl_CityHall_log = log(phl_CityHall),
            # phl_Temple = mean(Temple),
            phl_UPenn = mean(UPenn),
            # phl_vacrate = mean(vacancy_rate + 1),
            # phl_vacrate_log = log(phl_vacrate),
            phl_rest_sqft = mean(restaurant_sqft +1,na.rm = T),
            phl_rest_sqft_log = log(phl_rest_sqft),
            phl_bar_sqft = mean(bars_sqft +1, na.rm = T),
            phl_bar_sqft_log = log(phl_bar_sqft),
            # sg_visitors = sum(raw_visitor_counts),
            # sg_visits = sum(raw_visit_counts),
            sg_dwell = mean(median_dwell, na.rm = TRUE),
            sg_distance_home = mean(distance_from_home, na.rm = TRUE),
            # sg_workday = mean(PM_Workday, na.rm = TRUE),
            # sg_AM_PM = mean(AM_PM, na.rm = TRUE),
            sg_distance_home_log = log(sg_distance_home),
            count_bars_a = (sum(bars, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_rest_a = (sum(restaurant, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_arts_a = (sum(arts, na.rm = TRUE) +1)/phl_area_sqmi,
            # count_jrcol_a = sum(jrcol, na.rm = TRUE)/phl_area_sqmi,
            # count_college_a = (sum(college, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_sports_a = (sum(sports, na.rm = TRUE) + 1)/phl_area_sqmi,
            # count_museums_a = (sum(museum, na.rm = TRUE)  + 1)/phl_area_sqmi,
            # count_amuse_a = (sum(amusement, na.rm = TRUE)  + 1)/phl_area_sqmi,
            count_hotels_a = (sum(hotels, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_grocery_a = (sum(grocery, na.rm = TRUE) + 1)/phl_area_sqmi,
            # count_childcare_a = (sum(childcare, na.rm = TRUE) + 1)/phl_area_sqmi,
            # count_gas_a = (sum(gas, na.rm = TRUE) + 1)/phl_area_sqmi,
            # count_religious_a = (sum(religious, na.rm = TRUE) + 1)/phl_area_sqmi,
            # count_personal_a = (sum(personal, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_retailmix_top = n_distinct(top_category),
            count_retailmix_sub = n_distinct(sub_category),
            count_late_tag = (sum(late_night, na.rm = TRUE) + 1)/phl_area_sqmi,
            # count_barpub_tag = (sum(bar_pub, na.rm = TRUE) + 1)/phl_area_sqmi,
            # count_drinks_tag = (sum(drinks, na.rm = TRUE) + 1)/phl_area_sqmi,
            # count_cocktail_tag = (sum(cocktail, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_openlate = (sum(open_late, na.rm = TRUE) + 1)/phl_area_sqmi,
            count_bars_a_log = log(count_bars_a),
            count_rest_a_log = log(count_rest_a),
            count_arts_a_log = log(count_arts_a),
            count_grocery_a_log = log(count_grocery_a),
            # count_religious_a_log = log(count_religious_a),
            # count_childcare_a_log = log(count_childcare_a),
            # log_jrcol_a = log(sum(jrcol, na.rm = TRUE)/phl_area_sqmi),
            # log_college_a = log(sum(college, na.rm = TRUE)/phl_area_sqmi),
            # log_sports_a = log(sum(sports, na.rm = TRUE)/phl_area_sqmi),
            # log_museums_a = log(sum(museum, na.rm = TRUE)/phl_area_sqmi),
            # count_amuse_a_log = log(count_amuse_a),
            # count_barpub_log = log(count_barpub_tag),
            count_late_log = log(count_late_tag),
            count_retailmix_top_log = log(count_retailmix_top),
            count_retailmix_sub_log = log(count_retailmix_sub),
            count_openlate_log = log(count_openlate),
            nn_transit = mean(transit_nn),
            nn_parks = mean(parks_nn),
            # nn_parking = mean(parking_nn),
            nn_busstop = mean(busstop_nn),
            nn_trolley = mean(trolley_nn),
            # nn_bars = mean(bars_nn),
            # nn_rest = mean(rest_nn),
            # nn_arts = mean(arts_nn),
            # nn_college = mean(college_nn),
            # nn_sports = mean(sports_nn),
            # nn_casinos = mean(casinos_nn),
            # nn_hotels = mean(hotels_nn),
            nn_parks_log = log(nn_parks),
            nn_transit_log = log(nn_transit),
            # nn_parking_log = log(nn_parking),
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
  # mutate_if(is.numeric, list(~na_if(., Inf))) %>%
  st_drop_geometry() %>%
  left_join(phl_corridors_pred %>% dplyr::select(corridor)) %>%
  drop_na(corridor, demo_MHI, demo_medrent) %>% #Some of the census variables aren't reporting for our block groups
  st_as_sf() %>%
  ungroup() %>%
  na.omit(st_distance_home)

#####################
# RANDOM FOREST MODEL
#####################
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

set.seed(414)
inTrain <- createDataPartition(y=dat_pred_agg$Night_visits_sqmi_log, p = .60, list = FALSE)

phl.training.rf <- dat_pred_agg[inTrain,]
phl.test.rf <- dat_pred_agg[-inTrain,]

#Multivariate regression
rf1 <- 
  ranger(Night_visits_sqmi_log ~ ., #change lm to ranger, predictions are a little different
         data = st_drop_geometry(phl.training.rf) %>%
           dplyr::select(reg.vars),
         importance = "impurity")

##########################
#BUILDING SCENARIO DATASET
##########################
dat_scenario <-
  dat_pred %>%
  group_by(corridor, date_range_start) %>%
  summarize(count_rest = sum(restaurant, na.rm = TRUE),
            count_bars = sum(bars, na.rm = TRUE),
            count_arts = sum(arts, na.rm = TRUE)) %>%
  st_drop_geometry() %>%
  left_join(dat_pred_agg) %>%
  group_by(corridor) %>%
  drop_na(corridor, demo_MHI, demo_medrent) #Some of the census variables aren't reporting for our block groups
# 
# # dat_scenario <- tibble::rowid_to_column(dat_scenario, "ID")
# 
# #Scenario 1 - 10% increase in restaurants 
# scenario1 <-
#   dat_scenario %>%
#   dplyr::mutate(restaurants = restaurants * 1.1 ,
#                 count_rest_a = restaurants/phl_area_sqmi, 
#                 count_rest_a_log = log(count_rest_a))
# scenario1.preds <- predict(rf1, data = scenario1)
# scenario1.preds <- exp(scenario1.preds$predictions)
# 
# #Scenario 2 - 10% decrease increase in restaurants
# scenario2 <-
#   dat_scenario %>%
#   mutate(restaurants = restaurants *.9,
#          count_rest_a = restaurants/phl_area_sqmi,
#          count_rest_a_log = log(count_rest_a))
# scenario2.preds <- predict(rf1, data = scenario2)
# scenario2.preds <- exp(scenario2.preds$predictions)
# 
# #Scenario 3 - 10% increase in bars 
# scenario3 <-
#   dat_scenario %>%
#   mutate(bars = bars *1.1,
#          count_bars_a = bars/phl_area_sqmi,
#          count_bars_a_log = log(count_bars_a))
# scenario3.preds <- predict(rf1, data = scenario3)
# scenario3.preds <- exp(scenario3.preds$predictions)
# 
# #Scenario 4 - 10% increase in bars 
# scenario4 <-
#   dat_scenario %>%
#   mutate(bars = bars *.9,
#          count_bars_a = bars/phl_area_sqmi,
#          count_bars_a_log = log(count_bars_a))
# scenario4.preds <- predict(rf1, data = scenario4)
# scenario4.preds <- exp(scenario4.preds$predictions)
# 
# #Combine into single dataframe
# Scenario_preds_wide <-
#   dat_pred_agg %>%
#   dplyr::select(corridor,
#                 date_range_start,
#                 corr_type,
#                 Night_visits,
#                 phl_area_sqmi) %>%
#   mutate(Preds.1 = scenario1.preds * phl_area_sqmi,
#          Preds.2 = scenario2.preds * phl_area_sqmi,
#          Preds.3 = scenario3.preds * phl_area_sqmi,
#          Preds.4 = scenario4.preds * phl_area_sqmi) %>% #adding above predictions
#   # mutate(PctChange.1 = (Preds.1 - Night_visits)/Preds.1,
#   #        PctChange.2 = (Preds.2 - Night_visits)/Preds.2,
#   #        PctChange.3 = (Preds.3 - Night_visits)/Preds.3,
#   #        PctChange.4 = (Preds.4 - Night_visits)/Preds.4) %>% #calculating % change
#   group_by(corridor) %>% #aggregate 
#   summarize(Night_visits = mean(Night_visits),
#             Preds.1 = mean(Preds.1), 
#             Preds.2 = mean(Preds.2), 
#             Preds.3 = mean(Preds.3),
#             Preds.4 = mean(Preds.4)) 
# 
# head(Scenario_preds_wide)
# 
# Scenario_preds_long <-
#   Scenario_preds_wide %>%
#   pivot_longer(.,
#                cols=3:6,
#                names_to = "scenario",
#                values_to = "prediction") %>%
#   st_as_sf()
# 
# head(Scenario_preds_long)
# 
# #####################################
# # WRITE GEOJSON TO WORKING DIRECTORY
# #####################################
# library(rgdal)
# 
# Scenario_preds_wide.SP  <- as_Spatial(Scenario_preds_wide)
# writeOGR(Scenario_preds_wide.SP, 
#          'scenario.wide.geojson', 
#          'Scenario_preds_wide',
#          driver = 'GeoJSON')
# 
# Scenario_preds_long.SP  <- as_Spatial(Scenario_preds_long)
# writeOGR(Scenario_preds_long.SP, 
#          'scenario.long.geojson', 
#          'Scenario_preds_long.SP',
#          driver = 'GeoJSON')

##########
# FUNCTION
##########

#Aggregating up to the corridor (modelling dataset is separated out by month)
# dat_agg_scenario <- 
#   dat_pred %>%
#   group_by(corridor, corr_type) %>%
#   summarize(Night_visits = sum(Hrs19_0),
#             phl_area_sqmi = mean(corr_area_sqmi, na.rm = TRUE),
#             Night_visits_sqmi = (Night_visits + 1) / phl_area_sqmi,
#             Night_visits_sqmi_log = log(Night_visits_sqmi),
#             total = sum(total),
#             phl_building_size = mean(bld_area_sqft +1, na.rm = TRUE),
#             phl_building_size_log = log(phl_building_size),
#             phl_building_size_med = median(bld_area_sqft, na.rm = TRUE),
#             phl_UPenn = mean(UPenn),
#             phl_rest_sqft = mean(restaurant_sqft +1,na.rm = T),
#             phl_rest_sqft_log = log(phl_rest_sqft),
#             phl_bar_sqft = mean(bars_sqft +1, na.rm = T),
#             phl_bar_sqft_log = log(phl_bar_sqft),
#             sg_dwell = mean(median_dwell, na.rm = TRUE),
#             sg_distance_home = mean(distance_from_home, na.rm = TRUE),
#             sg_distance_home_log = log(sg_distance_home),
#             count_bars = sum(bars, na.rm = TRUE)/12,
#             count_rest = sum(restaurant, na.rm = TRUE)/12,
#             count_arts = sum(arts, na.rm = TRUE)/12,
#             count_bars_a = (count_bars + 1)/phl_area_sqmi,
#             count_rest_a = (count_rest + 1)/phl_area_sqmi,
#             count_arts_a = (count_arts + 1)/phl_area_sqmi,
#             count_sports_a = (sum(sports, na.rm = TRUE) + 1)/phl_area_sqmi,
#             count_hotels_a = (sum(hotels, na.rm = TRUE) + 1)/phl_area_sqmi,
#             count_grocery_a = (sum(grocery, na.rm = TRUE) + 1)/phl_area_sqmi,
#             count_retailmix_top = n_distinct(top_category),
#             count_retailmix_sub = n_distinct(sub_category),
#             count_late_tag = (sum(late_night, na.rm = TRUE) + 1)/phl_area_sqmi,
#             count_openlate = (sum(open_late, na.rm = TRUE) + 1)/phl_area_sqmi,
#             count_bars_a_log = log(count_bars_a),
#             count_rest_a_log = log(count_rest_a),
#             count_arts_a_log = log(count_arts_a),
#             count_grocery_a_log = log(count_grocery_a),
#             count_late_log = log(count_late_tag),
#             count_retailmix_top_log = log(count_retailmix_top),
#             count_retailmix_sub_log = log(count_retailmix_sub),
#             count_openlate_log = log(count_openlate),
#             nn_transit = mean(transit_nn),
#             nn_parks = mean(parks_nn),
#             nn_busstop = mean(busstop_nn),
#             nn_trolley = mean(trolley_nn),
#             nn_parks_log = log(nn_parks),
#             nn_transit_log = log(nn_transit),
#             nn_busstop_log = log(nn_busstop),
#             nn_trolley_log = log(nn_trolley),
#             demo_popdens = mean(popdens_mi),
#             demo_popdens_log = log(demo_popdens),
#             demo_pctWhite = weighted.mean(pctWhite, Hrs19_0, na.rm = TRUE),
#             demo_pctBlack = weighted.mean(pctBlack, Hrs19_0, na.rm = TRUE),
#             demo_pctHisp = weighted.mean(pctHisp, Hrs19_0, na.rm = TRUE),
#             demo_medAge = weighted.mean(MedAge, Hrs19_0, na.rm = TRUE),
#             demo_MHI = weighted.mean(MedHHInc, Hrs19_0, na.rm = TRUE),
#             demo_medrent = weighted.mean(MedRent, Hrs19_0, na.rm = TRUE),
#             demo_pctHisp_log = log(demo_pctHisp + 1)) %>% 
#   st_drop_geometry() %>%
#   left_join(phl_corridors_pred %>% dplyr::select(corridor)) %>%
#   drop_na(corridor, demo_MHI, demo_medrent) %>% #Some of the census variables aren't reporting for our block groups
#   st_as_sf() %>%
#   ungroup() %>%
#   na.omit(st_distance_home)

######

#Input the scenario factors here. These apply to both ComboGrid and ScenarioGenerator functions
rest_factor_select <- c(.95, 1, 1.05)
bar_factor_select <- c(.95, 1, 1.05)
arts_factor_select <- c(.95, 1, 1.05)

ComboGrid <- function(rest_factor, bar_factor, arts_factor) {
  
  allCombos <- expand.grid(rest_factor = rest_factor*100,
                           bar_factor = bar_factor*100,
                           arts_factor = arts_factor*100) 
  
  allCombos <- tibble::rowid_to_column(allCombos, "scenario")
  
  allCombos <- 
    allCombos %>% 
    mutate(
      rest = "rest",
      bars = "bars",
      arts = "arts")
  
  allCombos <- allCombos %>%
    mutate(
      filename = 
        paste(allCombos$rest,
      allCombos$rest_factor, 
      allCombos$bars, 
      allCombos$bar_factor, 
      allCombos$arts, 
      allCombos$arts_factor, sep = "-"))
  
  return(allCombos)
}

ScenarioCombos <- ComboGrid(rest_factor = rest_factor_select,
                       bar_factor = bar_factor_select,
                       arts_factor = arts_factor_select)

ScenarioGenerator <- function(dataset, rest_factor, bar_factor, arts_factor) {

  allCombos <- expand.grid(rest_factor = rest_factor,
                           bar_factor = bar_factor,
                           arts_factor = arts_factor) 

  allCombos <- tibble::rowid_to_column(allCombos, "ID")

  comboList <- unique(allCombos[['ID']])
  allScenarios <- data.frame()

for (i in comboList) {
    
  thisScenario <-
    dataset %>%
    dplyr::mutate(scenario = as.numeric(allCombos %>% filter(ID==i) %>% dplyr::select(ID)),
                  count_rest = as.numeric(allCombos %>% filter(ID == i) %>% dplyr::select(rest_factor)) * count_rest,
                  count_rest_a = count_rest/phl_area_sqmi,
                  count_rest_a_log = log(count_rest_a + 1),
                  count_bars = as.numeric(allCombos %>% filter(ID == i) %>% dplyr::select(bar_factor)) * count_bars,
                  count_bars_a = count_bars/phl_area_sqmi,
                  count_bars_a_log = log(count_bars_a + 1),
                  count_arts = as.numeric(allCombos %>% filter(ID == i) %>% dplyr::select(arts_factor)) * count_arts,
                  count_arts_a = count_arts/phl_area_sqmi,
                  count_arts_a_log = log(count_arts_a + 1))
  
  
  Scenario_preds <- predict(rf1, data = thisScenario %>% as.data.frame())
  Scenario_preds <- exp(Scenario_preds$predictions) %>% as.data.frame()
  
  Scenario_final <- cbind(Scenario_preds, thisScenario %>% 
                            dplyr::select(corridor, 
                                          count_rest, 
                                          count_arts, 
                                          count_bars, 
                                          Night_visits, 
                                          phl_area_sqmi, 
                                          scenario, 
                                          geometry)) %>%
    dplyr::rename("predictions" = ".") %>% #clean-up
    mutate(predictions = predictions * phl_area_sqmi) %>%
    st_as_sf()
  
  Scenario_final <- Scenario_final %>% st_as_sf() %>% st_transform(crs = "EPSG: 4326")
  
  Scenario_final <- Scenario_final %>%
    group_by(corridor, scenario) %>%
    summarize(Night_visits = sum(Night_visits),
              predictions = sum(predictions))
  
  Scenario_final <- 
    Scenario_final %>% 
    mutate(pct_change = (predictions - Night_visits)/Night_visits * 100) %>%
    as.data.frame()  %>%
    left_join(., ScenarioCombos %>% dplyr::select(scenario, filename)) %>%
    st_as_sf()
  
  Scenario_final <- as_Spatial(Scenario_final)
  
  writeOGR(Scenario_final,
           dsn = paste(unique(Scenario_final$filename), ".geojson"),
           'allScenario_final_small',
           driver = 'GeoJSON')
  
}
}

ScenarioGenerator(dataset = dat_scenario,
                          rest_factor = rest_factor_select,
                          bar_factor = bar_factor_select,
                          arts_factor = arts_factor_select)



# OLD WORK

# nrow(allScenario_preds)
# as.list(all.preds)
#Predicting on the scenario dataset

# all.preds <- predict(rf1, data = allScenario_preds %>% as.data.frame())
# all.preds <- exp(all.preds$predictions) %>% as.data.frame()

# head(all.preds)
# ?as.data.frame

# allScenario_final <-
#   cbind(allScenario_preds %>% #bind dataset to predictions
#         select(corridor, count_rest, count_arts, count_bars, Night_visits, phl_area_sqmi, scenario, geometry), 
#       all.preds) %>%
#   st_as_sf() %>%
#   dplyr::rename("predictions" = ".") %>% #clean-up
#   mutate(predictions = predictions * phl_area_sqmi) 
# 
# allScenario_final <- allScenario_final %>%
#   group_by(corridor, scenario) %>%
#   summarize(Night_visits = sum(Night_visits),
#             predictions = sum(predictions))

# allScenario_final <- 
#   allScenario_final %>% 
#   mutate(pct.change = (predictions - Night_visits)/Night_visits * 100) %>%
#   left_join(., allCombos) 

# allScenario_final %>%
#   filter(corridor == "Market East") %>%
#   View()

##-----cahnging to lat/long
# allScenario_final_latlong <- allScenario_final %>% st_transform(crs = "EPSG: 4326")

###################
# WRITE TO GEOJSON
###################
# allScenario_final_small.SP  <- as_Spatial(allScenario_final)
# writeOGR(allScenario_final_small.SP, 
#          'allScenario_small.geojson', 
#          'allScenario_final_small',
#          driver = 'GeoJSON')
# 
# allScenario_final_latlong.SP  <- as_Spatial(allScenario_final_latlong)
# writeOGR(allScenario_final_latlong.SP, 
#          'allScenario_latlong.geojson', 
#          'allScenario_final_latlong',
#          driver = 'GeoJSON')

