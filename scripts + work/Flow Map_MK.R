# FLOW MAP
# Safegraph Origins & Destinations
# Maddy Kornhauser
# 2/27/2021

#######
# SETUP
#######
# Load pacakages
library(sf)
library(tidyverse)

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
#LOAD DATA
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

#Boundaries shapefiles (projected & unprojected)
PHL_boundary <- 
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728') 
PHL_boundary_unproj <- 
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)

#Corridor shapefiles (projected & unprojected)
phl_corridors <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728') 
phl_corridors_unproj <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson", quiet = TRUE)

#Neighborhood shapefiles (projected & unprojected)
phl_nhoods <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson", quiet = TRUE) %>%
  st_transform('ESRI:102728')
phl_nhoods_unproj <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson", quiet = TRUE)

#Planning district shapefiles (projected & unprojected)
phl_dist <-
  st_read("http://data.phl.opendata.arcgis.com/datasets/0960ea0f38f44146bb562f2b212075aa_0.geojson", quiet = TRUE) %>%
  st_transform('ESRI:102728')

phl_dist_unproj <-
  st_read("http://data.phl.opendata.arcgis.com/datasets/0960ea0f38f44146bb562f2b212075aa_0.geojson", quiet = TRUE)

##########
#FLOW MAPS
##########
#Sources:
#https://gist.github.com/rafapereirabr/9a36c2e5ff04aa285fa3
#https://stackoverflow.com/questions/44283774/flow-maptravel-path-using-lat-and-long-in-r

#----BASE DATA (Used for all flow maps)
flows <- dat2 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>% #filter for business types
  st_join(phl_corridors) %>% #join destinations to phl corridors (apply corridor destination to each trip)
  st_as_sf() %>%
  st_drop_geometry() %>% 
  select(safegraph_place_id, 
         date_range_start,
         top_category,
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
  PHL_boundary_unproj %>% 
  st_coordinates() # split out coordinates 

PHL_boundary_unproj <-
  as.data.frame(PHL_boundary_unproj) # save as dataframe

#----MAP V1: NEIGHBORHOOD CENTROID TO CORRIDOR CENTROIDS
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
  group_by(nhood_origin, top_category, corridor_dest) %>% #grouping trip counts by origin neighborhood
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

#Maps - straight line segment example
flows_nhood %>% 
  filter(top_category == "Drinking Places (Alcoholic Beverages)",
         corridor_dest == "East Girard") %>% 
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y), fill = "grey20") + 
  geom_segment(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest,  
                   color=count, alpha = count),
               arrow = arrow(length = unit(0.01, "cm")), 
               size = 1,
               lineend = "round") +
  scale_colour_distiller(palette="Reds", name="Count", guide = "colorbar", direction = 1) +
  coord_equal() +
  mapTheme() + 
  # facet_wrap(~corridor) +
  labs(title = "Bar Trips to East Girard")

#Maps - curved line example
flows_nhood %>% 
  filter(top_category == "Drinking Places (Alcoholic Beverages)",
         corridor_dest == "East Girard") %>%
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y), fill = "grey20") +
  geom_curve(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest, 
                 color=count, alpha = count),
             curvature = -0.2, 
             arrow = arrow(length = unit(0.05, "cm")), 
             size = 1, 
             lineend = "round") +
  scale_colour_distiller(palette="YlGnBu", name="Count", guide = "colorbar", direction = 1) +
  coord_equal() +
  mapTheme() + 
  # facet_wrap(~corridor) +
  labs(title = "Bar Trips to East Girard")

#----MAP V2: DISTRICT CENTROID TO CORRIDOR CENTROIDS
#Join cbg to district shapefile
phl_cbg_dist <- 
  st_join(phl_dist_unproj, phl_cbg_cent, join = st_intersects) %>% #cbg centroid from above
  select(GEOID10, DIST_NAME) %>%
  st_centroid()

#Generate district centroid
phl_dist_cent <-
  phl_dist_unproj %>% 
  select(DIST_NAME) %>%
  st_centroid() 

#Data wrangling - join to centroid shapefiles and grouping by origin district
flows_dist <- flows %>%
  left_join(phl_cbg_dist, by=c("visitor_cbg"="GEOID10")) %>% #join CBGs to nhoods
  dplyr::rename(., dist_origin = DIST_NAME) %>%
  drop_na(dist_origin) %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  group_by(dist_origin, top_category, corridor_dest) %>%
  summarize(count = sum(count)) %>%
  left_join(phl_dist_cent, by=c("dist_origin"="DIST_NAME")) %>% 
  left_join(., phl_corr_cent, by=c("corridor_dest"="NAME")) %>%
  dplyr::rename(., origin.geom = geometry.x,
                dest.geom = geometry.y)

flows_dist <- as.data.frame(flows_dist)

flows_dist <- flows_dist %>% #split point data into lat and long columns
  mutate(lat.origin = unlist(map(flows_dist$origin.geom,1)),
         long.origin = unlist(map(flows_dist$origin.geom,2)),
         lat.dest = unlist(map(flows_dist$dest.geom,1)),
         long.dest = unlist(map(flows_dist$dest.geom,2)),
         id = as.character(c(1:nrow(.))))

#Mapping
##Bars - straight line segments
flows_dist %>% 
  filter(top_category == "Drinking Places (Alcoholic Beverages)",
         count < 2000) %>%
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y), fill = "grey20") +
  geom_segment(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest, 
                   color=count, alpha=count), 
               arrow = arrow(length = unit(0.02, "cm")),
               lineend = "round") +
  scale_colour_distiller(palette="OrRd", name="Count", guide = "colorbar", direction = 1) +
  coord_equal() +
  mapTheme() + 
  # facet_wrap(~corridor_dest) +
  labs(title = "Bar Trips")

##Restaurants - straight line segments
flows_dist %>% 
  filter(top_category == "Restaurants and Other Eating Places") %>%
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y), fill = "grey20") +
  geom_segment(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest, 
                   color=count, alpha=count), 
               arrow = arrow(length = unit(0.02, "cm")),
               lineend = "round") +
  scale_colour_distiller(palette="OrRd", name="Count", guide = "colorbar", direction = 1) +
  coord_equal() +
  mapTheme() + 
  # facet_wrap(~corridor_dest) +
  labs(title = "Restaurant Trips")

#Bars - curved line segments
flows_dist %>% 
  filter(top_category == "Drinking Places (Alcoholic Beverages)",
         corridor_dest == "East Girard") %>%
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y), fill = "grey20") +
  geom_curve(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest, 
                 color=count, alpha = count),
             curvature = -0.15, 
             arrow = arrow(length = unit(0.05, "cm")),
             size = 2,
             lineend = "round") +
  scale_colour_distiller(palette="YlGnBu", name="Count", guide = "colorbar", direction = 1) +
  coord_equal() +
  mapTheme() + 
  # facet_wrap(~corridor) +
  labs(title = "Bar Trips from East Girard")

#----MAP V3: CBG CENTROID TO Corridor CENTROID
#Generate cbg centroid
phl_cbg_cent <-
  phl_cbg_unproj %>% 
  select(GEOID10) %>%
  st_centroid() 

flows_cbg <- 
  flows %>%
  left_join(phl_cbg_cent, by=c("visitor_cbg"="GEOID10")) %>% 
  left_join(., phl_corr_cent, by=c("corridor_dest"="NAME")) %>%
  dplyr::rename(., origin.geom = geometry.x,
                dest.geom = geometry.y)

flows_cbg <- as.data.frame(flows_cbg)

flows_cbg <- 
  flows_cbg %>% #split point data into lat and long columns
  mutate(lat.origin = unlist(map(flows_cbg$origin.geom,1)),
         long.origin = unlist(map(flows_cbg$origin.geom,2)),
         lat.dest = unlist(map(flows_cbg$dest.geom,1)),
         long.dest = unlist(map(flows_cbg$dest.geom,2)),
         id = as.character(c(1:nrow(.))))

#Mapping
##Bars - straight line segments
flows_cbg %>% 
  filter(top_category == "Drinking Places (Alcoholic Beverages)",
         corridor_dest == "East Girard") %>%
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y), fill = "grey20") +
  geom_segment(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest, 
                   color=count, alpha=count), 
               arrow = arrow(length = unit(0.02, "cm")),
               lineend = "round") +
  scale_colour_distiller(palette="OrRd", name="Count", guide = "colorbar", direction = 1) +
  coord_equal() +
  mapTheme() + 
  # facet_wrap(~corridor_dest) +
  labs(title = "Bar Trips")

#----MAP V4 (OLD): CBG CENTROID TO CBG CENTROID
flows_cbg2 <- flows %>%
  left_join(phl_cbg_cent, by=c("visitor_cbg"="GEOID10")) %>% 
  left_join(., phl_cbg_cent, by=c("poi_cbg"="GEOID10")) %>%
  dplyr::rename(., origin.geom = geometry.x,
                dest.geom = geometry.y)

flows_cbg2 <- flows_cbg2 %>% #split point data into lat and long columns
  mutate(lat.origin = unlist(map(flows_cbg2$origin.geom,1)),
         long.origin = unlist(map(flows_cbg2$origin.geom,2)),
         lat.dest = unlist(map(flows_cbg2$dest.geom,1)),
         long.dest = unlist(map(flows_cbg2$dest.geom,2)),
         id = as.character(c(1:nrow(.))))

#Maps - straight line segment example
flows_cbg2 %>% 
  filter(top_category == "Drinking Places (Alcoholic Beverages)",
         corridor_dest == "East Girard") %>% 
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y), fill = "grey20") + 
  geom_segment(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest,  
                   color=count, alpha = count),
               arrow = arrow(length = unit(0.01, "cm")), 
               size = 1,
               lineend = "round") +
  scale_colour_distiller(palette="Reds", name="Count", guide = "colorbar", direction = 1) +
  coord_equal() +
  mapTheme() + 
  # facet_wrap(~corridor) +
  labs(title = "Bar Trips to East Girard")

