# COVID CHANGES
# Safegraph Origins & Destinations

#######
# SETUP
#######
# Load pacakages
library(sf)
library(tidyverse)

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
data2020 <- read.csv("./data/moves_monthly2020.csv")
phila <- st_read("./demo/phila.geojson", quiet = TRUE) 
#dat2018 <- read.csv("./moves_2018.csv")
#dat2020 <- read.csv("./moves_monthly2020.csv")
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


# Modify 2018 data
dat_2018 <- 
  dat2018 %>% 
  mutate(popularity_by_hour = str_remove_all(popularity_by_hour, pattern = "\\[|\\]")) %>% #remove brackets
  unnest(popularity_by_hour) %>% #unnest values
  separate(.,
           popularity_by_hour,
           c("18",
             "19", "20", "21", "22", "23"),
           sep = ",") %>%
  mutate(.,NightVisits2018 = as.numeric(`18`) + as.numeric(`19`) + as.numeric(`20`) + as.numeric(`21`) + as.numeric(`22`) + as.numeric(`23`))

dat_2018 <- dat_2018 %>% 
  dplyr::select(safegraph_place_id, 
                location_name,
                date_range_start, 
                date_range_end, 
                raw_visit_counts,
                raw_visitor_counts, 
                #visits_by_day, 
                #poi_cbg, 
                #visitor_home_cbgs, 
                #visitor_daytime_cbgs, 
                #visitor_work_cbgs, 
                #visitor_country_of_origin,
                #distance_from_home, 
                #median_dwell, 
                #bucketed_dwell_times, 
                #related_same_day_brand, 
                #related_same_month_brand, 
                #popularity_by_hour, 
                #device_type,
                popularity_by_day,
                NightVisits2018) %>%
  rename(raw_visit_counts2018 = raw_visit_counts, raw_visitor_counts2018 = raw_visitor_counts, popularity_by_day2018 = popularity_by_day) %>%
  mutate(month = substring(date_range_start,6,7))


dat_2020 <- 
  dat2020 %>% 
  mutate(popularity_by_hour = str_remove_all(popularity_by_hour, pattern = "\\[|\\]")) %>% #remove brackets
  unnest(popularity_by_hour) %>% #unnest values
  separate(.,
           popularity_by_hour,
           c("18",
             "19", "20", "21", "22", "23"),
           sep = ",") %>%
  mutate(.,NightVisits2020 = as.numeric(`18`) + as.numeric(`19`) + as.numeric(`20`) + as.numeric(`21`) + as.numeric(`22`) + as.numeric(`23`))



dat_join <- dat_2020 %>% 
  dplyr::select(safegraph_place_id, 
                location_name,
                date_range_start, 
                date_range_end, 
                raw_visit_counts,
                raw_visitor_counts, 
                #visits_by_day, 
                #poi_cbg, 
                #visitor_home_cbgs, 
                #visitor_daytime_cbgs, 
                #visitor_work_cbgs, 
                #visitor_country_of_origin,
                #distance_from_home, 
                #median_dwell, 
                #bucketed_dwell_times, 
                #related_same_day_brand, 
                #related_same_month_brand, 
                #popularity_by_hour, 
                #device_type,
                popularity_by_day,
                NightVisits2020) %>%
  rename(raw_visit_counts2020 = raw_visit_counts, raw_visitor_counts2020 = raw_visitor_counts, popularity_by_day2020 = popularity_by_day) %>%
  mutate(month = substring(date_range_start,6,7)) %>%
  inner_join(., dat_2018, by = c("safegraph_place_id", "month")) %>%
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

  
dat_join2 <- dat_join %>% 
  dplyr::select(safegraph_place_id, 
                location_name,
                raw_visit_counts2018,
                raw_visit_counts2020,
                NightVisits2018,
                NightVisits2020,
                popularity_by_day2018,
                popularity_by_day2020,
                month) %>%
  left_join(., phila, by = "safegraph_place_id") %>% 
  st_as_sf() %>%
  st_transform('ESRI:102728') 




##########
#COVID CHANGES
##########


#Citywide nighttime visits
dat_citywide <- dat_join2 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  group_by(month) %>%
  summarize(Total_Visits2018 = sum(NightVisits2018),
            Total_Visits2020 = sum(NightVisits2020)) %>%
  mutate(Percent_Change = (Total_Visits2020 - Total_Visits2018)/Total_Visits2018*100)

#Plot citywide chart
dat_citywide %>%
ggplot(., aes(x = month, y = Percent_Change, fill = Percent_Change)) + 
  geom_col() +
  scale_fill_distiller(palette="PiYG", direction = 1) +
  labs(title = "Philadelphia Commercial Trip % Change, 2018-2020",
       subtitle = "Figure X.X", y = "Percent Change", x = "Month") +
  plotTheme() 


#Separate by commercial use  
dat_citywide2 <- dat_join2 %>%
    filter(top_category == "Drinking Places (Alcoholic Beverages)" |
             top_category == "Restaurants and Other Eating Places" |
             top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
             top_category == "Performing Arts Companies") %>%
    group_by(top_category, month) %>%
    summarize(Total_Visits2018 = sum(NightVisits2018),
              Total_Visits2020 = sum(NightVisits2020)) %>%
    mutate(Percent_Change = (Total_Visits2020 - Total_Visits2018)/Total_Visits2018*100)  
  
#Plot citywide chart by commercial use
dat_citywide2 %>%
  ggplot(., aes(x = month, y = Percent_Change, fill = Percent_Change)) + 
  geom_col() +
  labs(title = "Philadelphia Commercial Trip % Change, 2018-2020",
       subtitle = "Figure X.X") +
  scale_fill_distiller(palette="PiYG", direction = 1) +
  facet_wrap(~top_category, scales = "free") +
  plotTheme()

#Plot category traffic change as line graph
dat_citywide2 %>%
  ggplot(., aes(x = month, y = Percent_Change, group = top_category, color = top_category)) + 
  geom_line(lwd = 1.5) +
  labs(title = "Philadelphia Commercial Trip % Change, 2018-2020",
       subtitle = "Figure X.X") +
  plotTheme()

#Filter by Restaurants
dat_restaurants <- dat_join2 %>%
  filter(top_category == "Restaurants and Other Eating Places") %>%
  filter(month == '04'| month == '05' | month == '06' | month == '07' | month == '08' | month == '09' | month =='10' | month == '11' | month == '12') %>%
  mutate(GEOID10 = as.numeric(GEOID)) %>%
  group_by(GEOID10) %>%
  summarize(Total_Visits2018 = sum(NightVisits2018),
            Total_Visits2020 = sum(NightVisits2020)) %>%
  st_drop_geometry() %>%
  left_join(phl_cbg) %>% 
  st_as_sf() %>%
  mutate(Percent_Change = (Total_Visits2020 - Total_Visits2018)/Total_Visits2018*100)

#Plot restaurant map
dat_restaurants %>%  
 # filter(Percent_Change < 300) %>%
  mutate(percent_change = case_when(Percent_Change > 100 ~ 100, Percent_Change <= 100 ~ Percent_Change)) %>%
  ggplot() + 
  geom_sf(data = PHL_boundary, fill = "grey20", color = "black")+
  geom_sf(aes(fill = percent_change), color = "transparent") + 
  scale_fill_distiller(palette="RdYlGn", direction = 1) +
  labs(title = "Restaurant Trip % Change April 2018-2020") +
  mapTheme()


#Filter by Restaurants
dat_restaurants <- dat_join2 %>%
  filter(top_category == "Restaurants and Other Eating Places") %>%
  filter(month == '04'| month == '05' | month == '06' | month == '07' | month == '08' | month == '09' | month =='10' | month == '11' | month == '12') %>%
  mutate(GEOID10 = as.numeric(GEOID)) %>%
  group_by(GEOID10) %>%
  summarize(Total_Visits2018 = sum(NightVisits2018),
            Total_Visits2020 = sum(NightVisits2020)) %>%
  st_drop_geometry() %>%
  left_join(phl_cbg) %>% 
  st_as_sf() %>%
  mutate(Percent_Change = (Total_Visits2020 - Total_Visits2018)/Total_Visits2018*100)

#Plot restaurant map
dat_restaurants %>%  
  # filter(Percent_Change < 300) %>%
  mutate(percent_change = case_when(Percent_Change > 100 ~ 100, Percent_Change <= 100 ~ Percent_Change)) %>%
  ggplot() + 
  geom_sf(data = PHL_boundary, fill = "grey20", color = "black")+
  geom_sf(aes(fill = percent_change), color = "transparent") + 
  scale_fill_distiller(palette="RdYlGn", direction = 1) +
  labs(title = "Restaurant Trip % Change April 2018-2020") +
  mapTheme()

-------------

#Filter by Bars
dat_bars <- dat_join2 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)") %>%
  filter(month == '04'| month == '05' | month == '06' | month == '07' | month == '08' | month == '09' | month =='10' | month == '11' | month == '12') %>%
  mutate(GEOID10 = as.numeric(GEOID)) %>%
  group_by(GEOID10) %>%
  summarize(Total_Visits2018 = sum(NightVisits2018),
            Total_Visits2020 = sum(NightVisits2020)) %>%
  st_drop_geometry() %>%
  left_join(phl_cbg) %>% 
  st_as_sf() %>%
  mutate(Percent_Change = (Total_Visits2020 - Total_Visits2018)/Total_Visits2018*100)

#Plot bar map
dat_bars %>%  
  # filter(Percent_Change < 300) %>%
  mutate(percent_change = case_when(Percent_Change > 100 ~ 100, Percent_Change <= 100 ~ Percent_Change)) %>%
  ggplot() + 
  geom_sf(data = PHL_boundary, fill = "grey20", color = "black")+
  geom_sf(aes(fill = percent_change), color = "transparent") + 
  scale_fill_distiller(palette="RdYlGn", direction = 1) +
  labs(title = "Bar Trip % Change 2018-2020") +
  mapTheme()


--------------------
#Corridors
#modify corridors
  corridors_filter <- phl_corridors %>%
  select(OBJECTID, NAME, GLA, P_DIST, ST_EXT, PT_ADD, VAC_RATE) %>%
  mutate(VAC_RATE = str_remove_all(VAC_RATE, pattern = "%")) %>%
  mutate(VAC_RATE = as.numeric(VAC_RATE)) %>%
  st_as_sf() %>%
  st_transform('ESRI:102728') 

  
dat_corr <- dat_join2 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  filter(month == '04'| month == '05' | month == '06' | month == '07' | month == '08' | month == '09' | month =='10' | month == '11' | month == '12') %>%
  mutate(GEOID10 = as.numeric(GEOID)) %>%
  st_join(corridors_filter) %>% 
  group_by(NAME.y, month) %>%
  summarize(Total_Visits2018 = sum(NightVisits2018),
            Total_Visits2020 = sum(NightVisits2020)) %>%
  mutate(Percent_Change = (Total_Visits2020 - Total_Visits2018)/Total_Visits2018*100) %>%
  drop_na(NAME.y) %>%
  st_as_sf() %>%
  st_transform('ESRI:102728') %>%
  st_drop_geometry() 

dat_corr <- dat_corr %>%
  mutate(NAME = NAME.y) %>%
  left_join(corridors_filter)
 

#Average across all months
dat_corr_avg <- dat_corr %>%  
  #filter(Total_Visits2018 > 200) %>%
  group_by(NAME) %>%
  summarize(Percent_Change = mean(Percent_Change))

###Corridor Visualizations 
#All Commercial Uses Corridor Map
dat_corr %>%  
  filter(month == "05") %>%
  mutate(Percent_Change = case_when(Percent_Change > 100 ~ 100, Percent_Change <= 100 ~ Percent_Change)) %>%
  ggplot() + 
  geom_sf(data = PHL_boundary, fill = "grey10", color = "darkgrey")+
  #geom_sf(data = phl_cbg, fill = "grey10", color = "grey20") +
  geom_sf(aes(fill = Percent_Change, geometry = geometry), color = "transparent") + 
  scale_fill_distiller(palette="RdYlGn", direction = 1) +
  labs(title = "Commercial Nighttime Trip % Change April 2018-2020") +
  mapTheme()


#% Commercial Trip Drop by Corridor
dat_corr_avg %>%
  ggplot(., aes(x = reorder(NAME,-Percent_Change), y = Percent_Change, fill = Percent_Change)) +
  geom_col() +
  scale_fill_distiller(palette="PiYG", direction = 1) +
  labs(title = "Commercial Nighttime Trip % Change April 2018-2020", x = "Commercial Corridors", y = "Percentage Change") +
  plotTheme() 





