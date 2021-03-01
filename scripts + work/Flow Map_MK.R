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

#Load data
dat <- read.csv("./data/moves_2018.csv")
phila <- st_read("./demo/phila.geojson") 

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

phl_cbg <- st_read("http://data.phl.opendata.arcgis.com/datasets/2f982bada233478ea0100528227febce_0.geojson",
                   quiet = TRUE) %>%
  mutate(GEOID10 = as.numeric(GEOID10))%>%
  st_transform('ESRI:102728') 

phl_cbg_unproj <- st_read("http://data.phl.opendata.arcgis.com/datasets/2f982bada233478ea0100528227febce_0.geojson",
                          quiet = TRUE) %>%
  mutate(GEOID10 = as.numeric(GEOID10),
         Lat = as.numeric(INTPTLAT10),
         Lon = as.numeric(INTPTLON10))

PHL_boundary <- st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728') 

PHL_boundary_unproj <- st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)

phl_corridors <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728') 

phl_corridors_unproj <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728') 

phl_nhoods <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson", quiet = TRUE) %>%
  st_transform('ESRI:102728')

# START HERE!

flows <- dat2 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Traveler Accommodation" |
           top_category == "Gambling Industries" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  st_join(phl_corridors) %>% #join to phl corridor shapefile
  st_join(phl_nhoods) %>% #join to neighborhood shapefile
  st_as_sf() %>%
  st_drop_geometry()%>%
  select(safegraph_place_id, 
         date_range_start,
         top_category,
         poi_cbg, 
         visitor_home_cbgs,
         NAME.y,
         P_DIST,
         name) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>%
  separate(.,
           visitor_home_cbgs,
           c("visitor_cbg", "count"),
           sep = ":") %>%
  mutate(count = as.numeric(count),
         visitor_cbg = as.numeric(visitor_cbg)) %>%
  dplyr::rename(., corridor = NAME.y,
                corridor_dist = P_DIST,
                neighborhood = name)

cbg_list <- dplyr::pull(phl_cbg, GEOID10) #Generate list of PHL GEOIDs
flows <- flows[ flows$visitor_cbg %in% cbg_list, ] #Remove non Philly CBGs

flows <- left_join(flows, phl_cbg_unproj, by=c("visitor_cbg"="GEOID10")) %>%
  left_join(., phl_cbg_unproj, by = c("poi_cbg" = "GEOID10")) %>% #join unproj cbg data
  dplyr::mutate(origin_centroid = st_centroid(geometry.x),
                dest_centroid = st_centroid(geometry.y)) %>% #cacluate centroid
  select(safegraph_place_id, 
         top_category, 
         date_range_start,
         poi_cbg, 
         visitor_cbg, 
         count, 
         corridor,
         corridor_dist,
         neighborhood, 
         origin_centroid,
         dest_centroid) #select columns

flows <- flows %>% #split point data into lat and long columns
  mutate(lat.origin = unlist(map(flows$origin_centroid,1)),
         long.origin = unlist(map(flows$origin_centroid,2)),
         lat.dest = unlist(map(flows$dest_centroid,1)),
         long.dest = unlist(map(flows$dest_centroid,2)),
         id = as.character(c(1:nrow(.))))

PHL_boundary_unproj <- PHL_boundary_unproj %>% st_coordinates() # split out coordinates 
PHL_boundary_unproj <-as.data.frame(PHL_boundary_unproj) # save as dataframe

#Bars in East Girard
flows %>% 
  filter(top_category == "Drinking Places (Alcoholic Beverages)",
         corridor == "East Girard",
         date_range_start == "2018-06-01T04:00:00Z") %>%
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y)) +
  geom_segment(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest, 
                   color=count, alpha = count),
               arrow = arrow(length = unit(0.05, "npc"))) +
  scale_colour_distiller(palette="Reds", name="Count", guide = "colorbar") +
  coord_equal()

?geom_segment

#Bars by Corridor Districts
flows %>% 
  filter(top_category == "Drinking Places (Alcoholic Beverages)",
         date_range_start == "2018-06-01T04:00:00Z") %>%
  drop_na(corridor_dist) %>%
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y), fill = "grey80") +
  geom_segment(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest, 
                   color=count, size=count, alpha=count)) +
  scale_colour_distiller(palette="Reds", name="Count", guide = "colorbar") +
  coord_equal() +
  facet_wrap(~corridor, ncol = 6) +
  mapTheme() + 
  labs(title = "Bar Trips by Corridor District, June 2018")

#Restaurants by corridor District
flows %>% 
  filter(top_category == "Restaurants and Other Eating Places",
         date_range_start == "2018-06-01T04:00:00Z") %>%
  drop_na(corridor_dist) %>%
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y)) +
  geom_segment(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest, 
                   alpha=count, color=count)) +
  scale_colour_distiller(palette="Blues", name="Count", guide = "colorbar") +
  coord_equal() +
  facet_wrap(~corridor_dist, ncol = 6) +
  mapTheme() + 
  labs(title = "Restaurant Trips by Corridor District, June 2018")

#Art Venues by corridor District
flows %>% 
  filter(top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies",
         date_range_start == "2018-06-01T04:00:00Z") %>%
  drop_na(corridor_dist) %>%
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y), fill = "grey80") +
  geom_segment(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest, 
                   color=count, alpha=count)) +
  scale_colour_distiller(palette="Greens", name="Count", guide = "colorbar") +
  coord_equal() +
  facet_wrap(~corridor_dist, ncol = 2) +
  mapTheme() + 
  labs(title = "Trips to Arts Venues by Corridor District, June 2018")

?geom_segment

#Casinos by corridor District
flows %>% 
  filter(top_category == "Gambling Industries",
         date_range_start == "2018-06-01T04:00:00Z") %>%
  drop_na(corridor_dist) %>%
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y), fill = "grey80") +
  geom_segment(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest, 
                   color=count, alpha = .2)) +
  scale_colour_distiller(palette="BuPu", name="Count", guide = "colorbar", trans = "reverse") +
  coord_equal() +
  mapTheme() + 
  labs(title = "Trips to Casinos, June 2018")

#Hotels by corridor District
flows %>% 
  filter(top_category == "Traveler Accommodation",
         date_range_start == "2018-06-01T04:00:00Z") %>%
  drop_na(corridor_dist) %>%
  ggplot() + 
  geom_polygon(data = PHL_boundary_unproj, aes(x=X, y=Y), fill = "grey80") +
  geom_segment(aes(x = lat.origin, y = long.origin, xend = lat.dest, yend = long.dest, 
                   color=count, alpha = count)) +
  scale_colour_distiller(palette="BuPu", name="Count", guide = "colorbar") +
  coord_equal() +
  mapTheme() + 
  labs(title = "Trips to Hotels, June 2018")


######## OLD/UNFINISHED WORK
# 
# 
# unique(flows$top_category)
# 
# airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE) 
# flights <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/flights.csv", header=TRUE, as.is=TRUE)
# 
# 
# xlim <- c(-171.738281, -56.601563)
# ylim <- c(12.039321, 71.856229)
# map("county",  col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)
# 
# 
# 
# map("state", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)
# ?us.cities
# 
# map('county', 
#     'pennsylvania,philadelphia',
#     col="#f2f2f2", 
#     fill=TRUE, 
#     bg="white", 
#     lwd=0.05)
# fsub <- flows[flows$top_category == "Drinking Places (Alcoholic Beverages)",]
# for (j in 1:length(fsub$top_category)) {
#   cbg1 <- cbg_centroids[cbg_centroids$GEOID10 == fsub[j,]$visitor_cbg,]
#   cbg2 <- cbg_centroids[cbg_centroids$GEOID10 == fsub[j,]$visitor_cbg,]
#   
#   inter <- gcIntermediate(c(cbg1[1,]$Lon, 
#                             cbg1[1,]$Lat), 
#                           c(cbg2[1,]$Lon, 
#                             cbg2[1,]$Lat), 
#                           n=100, 
#                           addStartEnd=TRUE)
#   
#   lines(inter, col="black", lwd=0.8)
# }
# 
# # GGPLOT 
# # https://stackoverflow.com/questions/44283774/flow-maptravel-path-using-lat-and-long-in-r
# 
# library(maps)
# library(geosphere)
# library(dplyr)
# library(ggplot2)
# library(ggmap)
# library(rworldmap)
# library(plyr)
# library(data.table)
# library(ggthemes)
# 
# 
# 
# # Read data on airports and flights
# airports <- read.csv("http://www.stanford.edu/~cengel/cgi-bin/anthrospace/wp-content/uploads/2012/03/airports.csv", as.is=TRUE, header=TRUE)
# flights <- read.csv("http://www.stanford.edu/~cengel/cgi-bin/anthrospace/wp-content/uploads/2012/03/PEK-openflights-export-2012-03-19.csv", as.is=TRUE, header=TRUE)
# 
# # get airport locations
# airport_locations <- airports[, c("IATA","longitude", "latitude")]
# 
# # aggregate number of flights (frequency of flights per pair)
# flights.ag <- ddply(flights, c("From","To"), function(x) count(x$To))
# ###I think I have this in the flows data already
# 
# # Link airport lat  long to origin and destination
# OD <- left_join(flights.ag, airport_locations, by=c("From"="IATA") )
# OD <- left_join(OD, airport_locations, by=c("To"="IATA") )
# OD$id <-as.character(c(1:nrow(OD))) #create and id for each pair
#   
# 
# ggplot() + 
#   #geom_polygon(data= mapworld_df, aes(long,lat, group=group), fill="gray30") +
#   geom_segment(data = OD, aes(x = longitude.x, y = latitude.x, xend = longitude.y, yend = latitude.y, color=freq),
#                arrow = arrow(length = unit(0.01, "npc"))) +
#   scale_colour_distiller(palette="Reds", name="Frequency", guide = "colorbar") +
#   coord_equal()
# 
# phl_cbg2 <- phl_cbg %>%
#   mutate(Lat = as.numeric(INTPTLAT10),
#          Lon = as.numeric(INTPTLON10))
# 
# 
# 
# OD <- left_join(flows, phl_cbg2, by=c("visitor_cbg"="GEOID10")) %>%
#   left_join(., phl_cbg2, by = c("poi_cbg" = "GEOID10")) %>%
#   mutate(Lat.origin = Lat.x,
#          Lon.origin = Lon.x,
#          Lat.dest = Lat.y,
#          Lon.dest = Lon.y) %>%
#   select(safegraph_place_id, 
#          top_category, 
#          poi_cbg, 
#          visitor_cbg, 
#          count, 
#          Lat.origin, 
#          Lon.origin, 
#          Lat.dest, 
#          Lon.dest) %>%
#   mutate(id = as.character(c(1:nrow(.))))
# 
# 
# OD %>% 
#   filter(top_category == "Gambling Industries") %>%
#   # group_by(safegraph_place_id, Lat.origin, Lon.origin, Lat.dest, Lon.dest) %>%
#   # summarize(count = sum(count)) %>%
#   ggplot() + 
#   geom_sf(data = PHL_boundary) +
#   geom_segment(aes(x = Lon.origin, y = Lat.origin, xend = Lon.dest, yend = Lat.dest, color=count),
#                arrow = arrow(length = unit(0.01, "npc"))) +
#   scale_colour_distiller(palette="Blues", name="Count", guide = "colorbar") 
# 
# 
# OD %>% 
#   filter(top_category == "Gambling Industries") %>%
#   ggplot() + 
#   # geom_sf(data = PHL_boundary) +
#   geom_curve(data = OD, aes(x = Lon.origin, y = Lat.origin, xend = Lon.dest, yend = Lat.dest, color=count),
#              curvature = -0.2, arrow = arrow(length = unit(0.01, "npc")))
# 
# 
# 
# head(read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE))
# head(read.csv("http://datasets.flowingdata.com/tuts/maparcs/flights.csv", header=TRUE, as.is=TRUE))
# 
# 
# ###### Second Try on ggplot
# dat2_unproj <- dat %>% 
#   dplyr::select(safegraph_place_id, 
#                 date_range_start, 
#                 date_range_end, 
#                 raw_visit_counts,
#                 raw_visitor_counts, 
#                 visits_by_day, 
#                 poi_cbg, 
#                 visitor_home_cbgs, 
#                 visitor_daytime_cbgs, 
#                 visitor_work_cbgs, 
#                 visitor_country_of_origin,
#                 distance_from_home, 
#                 median_dwell, 
#                 bucketed_dwell_times, 
#                 related_same_day_brand, 
#                 related_same_month_brand, 
#                 popularity_by_hour, 
#                 popularity_by_day, 
#                 device_type) %>%
#   left_join(., phila, by = "safegraph_place_id") %>%
#   st_as_sf()
# 
# 
# PHL_boundary_unproj <- st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)
# 
# phl_cbg_unproj <- st_read("http://data.phl.opendata.arcgis.com/datasets/2f982bada233478ea0100528227febce_0.geojson") %>%
#   mutate(GEOID10 = as.numeric(GEOID10))
# 
# phl_corridors_unproj <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson")
# 
# phl_nhoods_unproj <- 
#   st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson") %>%
#   st_as_sf()
# 
# phl_nhoods <- 
#   st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson") %>%
#   st_transform('ESRI:102728') %>%
#   st_as_sf()
# 
# flows2 <- dat2 %>%
#   filter(top_category == "Drinking Places (Alcoholic Beverages)" |
#            top_category == "Restaurants and Other Eating Places" |
#            top_category == "Traveler Accommodation" |
#            top_category == "Gambling Industries" |
#            top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
#            top_category == "Performing Arts Companies") %>% #filtering for nightlife
#   st_join(phl_corridors) %>% #join to phl corridor shapefile
#   st_join(phl_nhoods) %>% #join to neighborhood shapefile
#   st_drop_geometry() %>% #drop geometry
#   mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
#   mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
#   mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
#   mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
#   unnest(visitor_home_cbgs) %>% #unnest visitor home cbg column
#   separate(.,
#            visitor_home_cbgs,
#            c("visitor_cbg", "count"),
#            sep = ":") %>% #separate counts and cbg
#   mutate(count = as.numeric(count),
#          visitor_cbg = as.numeric(visitor_cbg)) %>% #clean-up
#   select(safegraph_place_id,
#          top_category,
#          poi_cbg,
#          visitor_cbg,
#          count,
#          NAME.y,
#          P_DIST,
#          listname
#          ) %>%  #clean-up
#   dplyr::rename(., corridor = NAME.y,
#                 corridor_dist = P_DIST,
#                 neighborhood = listname)  #clean-up
# 
# OD2 <- left_join(flows2, phl_cbg_unproj, by=c("visitor_cbg"="GEOID10")) %>% #joining origin geometry
#   left_join(., phl_cbg_unproj, by = c("poi_cbg" = "GEOID10"))  %>% #joining destination geometry
#   mutate(origin_centroid = st_centroid(geometry.x),
#          dest_centroid = st_centroid(geometry.y)) %>%
#   drop_na(geometry.x)
# 
# test <- OD2 %>%
#   mutate(lat.origin = unlist(map(OD2$origin_centroid,1)),
#          long.origin = unlist(map(OD2$origin_centroid,2)),
#          lat.dest = unlist(map(OD2$dest_centroid,1)),
#          long.dest = unlist(map(OD2$dest_centroid,2))) %>%
#   select(safegraph_place_id,
#          top_category,
#          poi_cbg,
#          visitor_cbg,
#          count,
#          corridor,
#          corridor_dist,
#          neighborhood,
#          lat.origin,
#          long.origin,
#          lat.dest,
#          long.dest)
# 
# 
# test %>% 
#   filter(top_category == "Gambling Industries") %>%
#   ggplot() + 
#   geom_sf(data = PHL_boundary) +
#   geom_segment(aes(x = long.origin, y = lat.origin, xend = long.dest, yend = lat.dest, color=count),
#                arrow = arrow(length = unit(0.01, "npc"))) +
#   scale_colour_distiller(palette="Blues", name="Count", guide = "colorbar") 
# 
# test %>% 
#   filter(top_category == "Gambling Industries") %>%
#   ggplot() + 
#   geom_sf(data = PHL_boundary) +
#   geom_curve(aes(x = long.origin, y = lat.origin, xend = long.dest, yend = lat.dest, color=count),
#              curvature = -0.2, arrow = arrow(length = unit(0.01, "npc")))
# 
# 
# 
# %>%
#   mutate(Lat.origin = Lat.x,
#          Lon.origin = Lon.x,
#          Lat.dest = Lat.y,
#          Lon.dest = Lon.y) %>%
#   select(safegraph_place_id, 
#          top_category, 
#          poi_cbg, 
#          visitor_cbg, 
#          count, 
#          Lat.origin, 
#          Lon.origin, 
#          Lat.dest, 
#          Lon.dest) %>%
#   mutate(id = as.character(c(1:nrow(.))))
#   
#   
#   
#   colnames(flows2)
#   
#   ##################
#   #GEOSPHERE PACKAGE
#   ##################
#   #https://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/
#   colnames(dat2)
#   
#   #Coordinates of origins & destinations
#   #Need flows
#   dat_flows_dest <- dat2 %>%
#     filter(top_category == "Drinking Places (Alcoholic Beverages)" |
#              top_category == "Restaurants and Other Eating Places" |
#              top_category == "Traveler Accommodation" |
#              top_category == "Gambling Industries" |
#              top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
#              top_category == "Performing Arts Companies") %>%
#     select(poi_cbg) %>%
#     st_drop_geometry() %>%
#     dplyr::rename(., GEOID10 = poi_cbg) %>% 
#     left_join(phl_cbg, by = "GEOID10") %>%
#     mutate(centroid = st_centroid(geometry)) %>%
#     select(GEOID10, centroid)
#   
#   dat_flows_origins <- dat2 %>%
#     filter(top_category == "Drinking Places (Alcoholic Beverages)" |
#              top_category == "Restaurants and Other Eating Places" |
#              top_category == "Traveler Accommodation" |
#              top_category == "Gambling Industries" |
#              top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
#              top_category == "Performing Arts Companies") %>%
#     select(visitor_home_cbgs) %>%
#     st_drop_geometry() %>%
#     mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
#     mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
#     mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
#     mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
#     unnest(visitor_home_cbgs) %>%
#     separate(.,
#              visitor_home_cbgs,
#              c("visitor_CBG", "visitors"),
#              sep = ":") %>%
#     select(visitor_CBG) %>%
#     dplyr::rename(., GEOID10 = visitor_CBG) %>% 
#     mutate(GEOID10 = as.numeric(GEOID10)) %>%
#     left_join(phl_cbg, by = "GEOID10") %>%
#     drop_na(geometry) %>%
#     mutate(centroid = st_centroid(geometry)) %>%
#     select(GEOID10, centroid) 
#   
#   rbind(dat_flows_dest, dat_flows_origins2)
#   
#   ####
#   
#   cbg_centroids <- 
#     phl_cbg %>%
#     select(GEOID10, geometry) %>%
#     mutate(geometry = st_centroid(geometry))
#   
#   cbg_centroids <- 
#     phl_cbg %>%
#     select(GEOID10, INTPTLAT10, INTPTLON10) %>%
#     st_drop_geometry() %>%
#     mutate(Lat = as.numeric(INTPTLAT10),
#            Lon = as.numeric(INTPTLON10))