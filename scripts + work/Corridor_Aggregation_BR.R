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

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    plot.background = element_rect(fill = "white"),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "white", fill=NA, size=2),
    strip.background = element_rect(fill = "white", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
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
View(v18)

census_api_key("c0e7f2bf1ed21adb8eca6c9652036dfd5c6e1040", overwrite = TRUE)
blockgroups2018 <-  
get_acs(geography = "block group", 
        variables = c("B01003_001E", 
                      "B02001_002E", 
                      "B01002_001E",
                      "B19013_001E", 
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
         Hisp = B03002_012E,
         Black = 	B02001_003E,
         visitor_cbg = GEOID) %>%
  dplyr::select(-ends_with("M")) %>%
  mutate(pctWhite = ((White / TotalPop)*100),
         pctBlack = ((Black / TotalPop)*100),
         pctHisp = ((Hisp / TotalPop)*100),
  ) %>%
  st_as_sf()

blockgroups2018 <- blockgroups2018 %>%
mutate(centroid_cbg = st_centroid(geometry), 
       visitor_cbg = as.numeric(visitor_cbg)) 
  

#Plot census blocks
blockgroups2018 %>%  
  ggplot() + 
  geom_sf( color = "white") + 
  scale_fill_distiller(palette="RdYlGn", direction = 1) +
  labs(title = "Restaurant Trip % Change April 2018-2020") +
  mapTheme()


# Modify 2018 data
dat2018 <- dat2018 %>% 
  dplyr::select(safegraph_place_id, 
                location_name,
                date_range_start, 
                date_range_end, 
                raw_visit_counts,
                raw_visitor_counts, 
                visits_by_day, 
                poi_cbg, 
                visitor_home_cbgs, 
                visitor_daytime_cbgs, 
                visitor_work_cbgs, 
                #visitor_country_of_origin,
                distance_from_home, 
                #median_dwell, 
                #bucketed_dwell_times, 
                #related_same_day_brand, 
                #related_same_month_brand, 
                #popularity_by_hour, 
                #device_type,
                #popularity_by_day,
                ) %>%
  mutate(month = substring(date_range_start,6,7)) %>%
  mutate(Month = case_when(month == "01" ~ "January",
                           month == "02" ~ "February",
                           month == "03" ~ "March",
                           month == "04" ~ "April",
                           month == "05" ~ "May", 
                           month == "06" ~ "Jun",
                           month == "07" ~ "May",
                           month == "08" ~ "May",
                           month == "09" ~ "May",
                           month == "10" ~ "May",
                           month == "11" ~ "May",
                           month == "12" ~ "May"))

#Join to places dataset
dat_2018 <- dat2018 %>% 
  dplyr::select(safegraph_place_id, 
                raw_visit_counts,
                poi_cbg, 
                visitor_home_cbgs, 
                visitor_daytime_cbgs, 
                visitor_work_cbgs, 
                distance_from_home,
                Month) %>%
  left_join(., phila, by = "safegraph_place_id") %>% 
  st_as_sf() %>%
  st_transform('ESRI:102728') 

#Filter to just bars, restaurants, and performing arts and join to corridors
flows <- dat_2018 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>% #filter for business types
  st_join(phl_corridors) %>% #join destinations to phl corridors (apply corridor destination to each trip)
  st_as_sf() %>%
  st_drop_geometry() %>% 
  select(safegraph_place_id, 
         Month,
         top_category,
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
  group_by(visitor_cbg, NAME) %>%
  summarize(Count = sum(count)-1,
            distance_from_home = mean(distance_from_home)) %>%
  left_join(., phl_corridors_cent, by = "NAME") 

#Join to blockgroups data
corr_summary1 <- corr_summary %>%
  dplyr::select(visitor_cbg, NAME, Count, VAC_RATE, centroid_corr, distance_from_home) %>%
  right_join(., blockgroups2018, by = "visitor_cbg") 

corr_summary1<- corr_summary1 %>%
  dplyr::select(visitor_cbg, NAME.x, Count, centroid_corr, MedAge, MedHHInc, pctWhite, pctBlack, pctHisp, centroid_cbg, distance_from_home) %>%
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
  left_join(., phl_corridors, by = "NAME") %>%
  dplyr::select(NAME, 
                total_trips, 
                weighted_age, 
                weighted_inc, 
                weighted_pctWhite, 
                weighted_pctBlack, 
                weighted_pctHisp, 
                VAC_RATE, 
                distance_from_home, 
                geometry)

#Rejoin corridor origins to cbg dataset for geometry (FINAL CORRIDOR ORIGINS DATASET. EACH ROW IS ONE CORRIDOR/CBG COMBINATION)
corr_origins_final <- corr_summary1 %>%
  left_join(., corr_group, by = "NAME") %>%
  dplyr::select(visitor_cbg, NAME, Count, MedAge, MedHHInc, pctWhite, pctBlack, pctHisp, distance_from_home.x, total_trips) %>%
  mutate(percent_of_trips = Count / total_trips) %>%
  left_join(., blockgroups2018, by = "visitor_cbg") %>%
  dplyr::select(visitor_cbg, NAME.x, Count, percent_of_trips, MedAge.x, MedHHInc.x, pctWhite.x, pctBlack.x, pctHisp.x, distance_from_home.x, total_trips, geometry) %>%
  rename(NAME = NAME.x,
         MedAge = MedAge.x,
         MedHHInc = MedHHInc.x,
         pctWhite = pctWhite.x,
         pctBlack = pctBlack.x,
         pctHisp = pctHisp.x,
         distance_from_home = distance_from_home.x)
  





