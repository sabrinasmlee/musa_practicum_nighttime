# MUSA Pracricum
# Maddy's work - Week of 2/15/2021

# Looking at the Fishtown neigbhorhood

#######
# SETUP
#######
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(tidycensus)
library(lubridate)
library(datetime)
library(viridis)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(gganimate)
library(kableExtra)

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
palette3 <- viridis_pal()(3)
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
#census_api_key("53845fc057d94b7ce8d243f50bd9fa0d9237c612", overwrite = TRUE, install = TRUE)

setwd("~/GitHub/musa_practicum_nighttime")
#setwd("C:/Users/rawnb/Desktop/MUSA/Spring 2021/Practicum")

###########
# LOAD DATA
############
dat <- read.csv("./data/moves_2018.csv")
phila <- st_read("./demo/phila.geojson") %>%
  st_transform('ESRI:102728') %>%
  st_as_sf()

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
  st_as_sf()

phl_cbg <- 
  st_read("http://data.phl.opendata.arcgis.com/datasets/2f982bada233478ea0100528227febce_0.geojson") %>%
  st_transform('ESRI:102728') %>%
  mutate(GEOID10 = as.numeric(GEOID10))
phl_zip <- 
  st_read("http://data.phl.opendata.arcgis.com/datasets/b54ec5210cee41c3a884c9086f7af1be_0.geojson") %>%
  st_transform('ESRI:102728') %>%
  mutate(CODE = as.numeric(CODE))
phl_nhoods <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson") %>%
  st_transform('ESRI:102728') %>% 
  st_as_sf()
phl_corridors <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson") %>%
  st_transform('ESRI:102728')
fishtown <- 
  phl_nhoods %>% 
  filter(name == "FISHTOWN") %>% 
  st_as_sf()
EastGirard_corr <- 
  phl_corridors %>% 
  filter(NAME == "East Girard") %>% 
  st_as_sf()

WestGirard_corr <- 
  phl_corridors %>% 
  filter(NAME == "West Girard") %>% 
  st_as_sf()

SecondGirard_corr <- 
  phl_corridors %>% 
  filter(NAME == "2nd and Girard") %>% 
  st_as_sf()


ggplot() + 
  geom_sf(data = phl_nhoods) +
  #geom_sf(data = fishtown, fill = "red")
  geom_sf(data = SecondGirard_corr, color = "red", fill = "red")

#################################
# FISHTOWN NEIGHBORHOOD ANALYSIS
#################################
#Do we want to do this for the corridor?
#Just for Philadelphia visitors

dat_fishtown <- dat2[fishtown,]

dat_fishtown_cbg <- 
  dat_fishtown %>%
  select(safegraph_place_id, 
         date_range_start, 
         top_category, 
         sub_category, 
         visitor_home_cbgs, 
         geometry) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>%
  separate(.,
           visitor_home_cbgs,
           c("cbg_origin", "visitors"),
           sep = ":") %>%
  mutate(cbg_origin = as.numeric(cbg_origin),
         visitors = as.numeric(visitors))

# RESTAURANTS
dat_fishtown_rest <- 
  dat_fishtown_cbg %>%
  filter(top_category == "Restaurants and Other Eating Places") %>%
  group_by(cbg_origin) %>%
  summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry) 

## Restaurants (quintile)
dat_fishtown_rest %>% 
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = q5(visitors), geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = fishtown, color = "red", fill = "transparent", lwd = 1) +
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Visitor Count \n(Quintile)") +
  mapTheme() +
  labs(title = "Fishtown Restaurants: Where do visitors come from?") 

## Restaurants (continuous)
dat_fishtown_rest %>% 
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = visitors, geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = fishtown, color = "red", fill = "transparent", lwd = 1) +
  scale_fill_viridis() +
  mapTheme() +
  labs(title = "Fishtown Restaurants: Where do visitors come from?") 

# BARS
dat_fishtown_bars <- 
  dat_fishtown_cbg %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)") %>%
  group_by(cbg_origin) %>%
  summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry) 

## Bars (quintile)
dat_fishtown_bars %>% 
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = q5(visitors), geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = fishtown, color = "red", fill = "transparent", lwd = 1) +
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Visitor Count \n(Quintile)") +
  mapTheme() +
  labs(title = "Fishtown Bars: Where do visitors come from?") 
  
## Bars (continuous)
dat_fishtown_bars %>%
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = visitors, geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = fishtown, color = "red", fill = "transparent", lwd = 1) +
  scale_fill_viridis() +
  mapTheme() +
  labs(title = "Fishtown Bars: Where do visitors come from?") 

###############################
# EAST GIRARD CORRIDOR ANALYSIS
###############################
#Do we want to do this for the corridor?
#Just for Philadelphia visitors

dat_EGC <- dat2[EastGirard_corr,]

dat_EGC_cbg <- 
  dat_EGC %>%
  select(safegraph_place_id, 
         date_range_start, 
         top_category, 
         sub_category, 
         visitor_home_cbgs, 
         geometry) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>%
  separate(.,
           visitor_home_cbgs,
           c("cbg_origin", "visitors"),
           sep = ":") %>%
  mutate(cbg_origin = as.numeric(cbg_origin),
         visitors = as.numeric(visitors))

# RESTAURANTS
dat_EGC_rest <- 
  dat_EGC_cbg %>%
  filter(top_category == "Restaurants and Other Eating Places") %>%
  group_by(cbg_origin) %>%
  summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry) 

## Restaurants (quintile)
dat_EGC_rest %>% 
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = q5(visitors), geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = EastGirard_corr, color = "red", fill = "transparent", lwd = 1) +
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Visitor Count \n(Quintile)") +
  mapTheme() +
  labs(title = "East Girard Restaurants: Where do visitors come from?") 

## Restaurants (continuous)
dat_EGC_rest %>% 
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = visitors, geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = EastGirard_corr, color = "red", fill = "transparent", lwd = 1) +
  scale_fill_viridis() +
  mapTheme() +
  labs(title = "East Girard Restaurants: Where do visitors come from?") 

# BARS
dat_EGC_bars <- 
  dat_EGC_cbg %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)") %>%
  group_by(cbg_origin) %>%
  summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry) 

## Bars (quintile)
dat_EGC_bars %>% 
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = q5(visitors), geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = EastGirard_corr, color = "red", fill = "transparent", lwd = 1) +
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Visitor Count \n(Quintile)") +
  mapTheme() +
  labs(title = "East Girard Bars: Where do visitors come from?") 

## Bars (continuous)
dat_EGC_bars %>%
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = visitors, geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = EastGirard_corr, color = "red", fill = "transparent", lwd = 1) +
  scale_fill_viridis() +
  mapTheme() +
  labs(title = "East Girard Bars: Where do visitors come from?") 

#Figure out how to bring in regional visitors.
#Bring in census block groups from PA and NJ.

#################
# Census Analysis
#################
phl_blockgroups <- 
  get_acs(geography = "block group", 
        variables = c("B01003_001E", 
                      "B02001_002E", 
                      "B01002_001E",
                      "B19013_001E", 
                      "B25064_001E"),
        year=2018, 
        state=42, 
        county=101, 
        geometry=T, 
        output = "wide") %>%
  st_transform('ESRI:102728')%>%
  rename(TotalPop = B01003_001E,
         Whites = B02001_002E,
         MedAge = B01002_001E,
         MedHHInc = B19013_001E,
         MedRent = B25064_001E,
         GEOID10 = GEOID) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         GEOID10 = as.numeric(GEOID10)) %>%
  dplyr::select(-Whites) 

#join to census information
dat_fishtown_rest_census <- 
  dat_fishtown_rest %>%
  dplyr::select(GEOID10, visitors) %>%
  left_join(phl_blockgroups, by = "GEOID10") 

#Fishtown table with summary statistics
dat_fishtown_rest_census %>%
  summarize(Avg_Age = weighted.mean(MedAge, visitors, na.rm = TRUE), #average of median values?
            Avg_Income = weighted.mean(MedHHInc, visitors, na.rm = TRUE),
            Avg_Rent = weighted.mean(MedRent, visitors, na.rm = TRUE),
            Avg_pctWhite = weighted.mean(pctWhite, visitors, na.rm = TRUE)) %>%
  kable(caption = "Fishtown Restaurant Visitors") %>%
  kable_styling()

#pivot to longer
dat_fishtown_rest_long <- 
  dat_fishtown_rest_census %>%
  mutate(Geography = "Fishtown") %>%
  pivot_longer(.,
               cols = c("MedAge", "MedHHInc", "MedRent", "pctWhite"),
               names_to = "variable") %>%
  dplyr::select(GEOID10, Geography, visitors, variable, value)

#Fishtown histogram w/ summary staistics
#Is histogram/frequency polygon the right tool here?
dat_fishtown_rest_long %>%
  ggplot(., aes(x = value, weight = visitors)) + 
  geom_histogram() +
  facet_wrap(~variable, scales = "free")

dat_fishtown_rest_long %>%
  ggplot(., aes(x = value, weight = visitors)) + 
  geom_freqpoly() +
  facet_wrap(~variable, scales = "free")

dat_EGC_rest_census <- 
  dat_EGC_rest %>%
  dplyr::select(GEOID10, visitors) %>%
  left_join(phl_blockgroups, by = "GEOID10") 

dat_EGC_rest_long <- 
  dat_EGC_rest_census %>%
  mutate(Type = "Restaurants") %>%
  pivot_longer(.,
               cols = c("MedAge", "MedHHInc", "MedRent", "pctWhite"),
               names_to = "variable") %>%
  dplyr::select(GEOID10, Type, visitors, variable, value)

dat_EGC_rest_long %>%
  ggplot(., aes(x = value, weight = visitors)) + #Is there a way that we can normalize by the number of people from each BG?
  geom_freqpoly() +
  facet_wrap(~variable, scales = "free")

#Bars
dat_EGC_bars_census <- 
  dat_EGC_bars %>%
  dplyr::select(GEOID10, visitors) %>%
  left_join(phl_blockgroups, by = "GEOID10")

dat_EGC_bars_long <- 
  dat_EGC_bars_census %>%
  mutate(Type = "Bars") %>%
  pivot_longer(.,
               cols = c("MedAge", "MedHHInc", "MedRent", "pctWhite"),
               names_to = "variable") %>%
  dplyr::select(GEOID10, Type, visitors, variable, value)

dat_EGC_bars_long %>%
  ggplot(., aes(x = value)) + 
  geom_freqpoly() +
  facet_wrap(~variable, scales = "free")

# dat_EGC_bars_long %>%
#   rbind(., dat_EGC_rest_long) %>%
#   ggplot(., aes(x = value, weight = visitors)) + 
#   geom_freqpoly() +
#   facet_grid(Type ~ variable, scales = "free")
# 
# #Histogram
# dat_EGC_bars_long %>%
#   rbind(., dat_EGC_rest_long) %>%
#   ggplot(., aes(x = value, weight = visitors, colour = Type)) + 
#   geom_freqpoly(lwd = 1) +
#   scale_fill_manual(values = palette3,
#                     aesthetics = c("colour", "fill"),
#                     name = "Visitor Count \n(Quintile)") +
#   facet_wrap( ~ variable, scales = "free") +
#   plotTheme() +
#   labs(title = 'Vistor Profiles to Bars & Restaurants, East Girard Corridor')

#Density plot
dat_EGC_bars_long %>%
  rbind(., dat_EGC_rest_long) %>%
  ggplot(., aes(x = value, weight = visitors, fill = Type, ..scaled..)) + 
  geom_density(alpha = .5) +
  scale_fill_manual(values = palette3,
                    aesthetics = c("colour", "fill"),
                    name = "Visitor Count \n(Quintile)") +
  facet_wrap( ~ variable, scales = "free") +
  plotTheme() +
  labs(title = 'Vistor Profiles to Bars & Restaurants, East Girard Corridor')

#######################
# WEST GIRARD ANALYSIS
#######################
dat_WGC <- dat2[WestGirard_corr,]

dat_WGC_cbg <- 
  dat_WGC %>%
  select(safegraph_place_id, 
         date_range_start, 
         top_category, 
         sub_category, 
         visitor_home_cbgs, 
         geometry) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>%
  separate(.,
           visitor_home_cbgs,
           c("cbg_origin", "visitors"),
           sep = ":") %>%
  mutate(cbg_origin = as.numeric(cbg_origin),
         visitors = as.numeric(visitors))

dat_WGC_night <- 
  dat_WGC_cbg %>%
  filter(top_category == "Restaurants and Other Eating Places" | 
         top_category == "Drinking Places (Alcoholic Beverages)" ) %>%
  group_by(cbg_origin) %>%
  summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry)

#join to census information and pivot longer
dat_WGC_night_census <- 
  dat_WGC_night %>%
  dplyr::select(GEOID10, visitors) %>%
  left_join(phl_blockgroups, by = "GEOID10") %>%
  mutate(Geography = "West Girard") %>%
  pivot_longer(.,
               cols = c("MedAge", "MedHHInc", "MedRent", "pctWhite"),
               names_to = "variable") %>%
  dplyr::select(GEOID10, Geography, visitors, variable, value)

# East Girard data
dat_EGC_night <- 
  dat_EGC_cbg %>%
  filter(top_category == "Restaurants and Other Eating Places" | 
           top_category == "Drinking Places (Alcoholic Beverages)" ) %>%
  group_by(cbg_origin) %>%
  summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry)

#join to census information and pivot longer
dat_EGC_night_census <- 
  dat_EGC_night %>%
  dplyr::select(GEOID10, visitors) %>%
  left_join(phl_blockgroups, by = "GEOID10") %>%
  mutate(Geography = "East Girard") %>%
  pivot_longer(.,
               cols = c("MedAge", "MedHHInc", "MedRent", "pctWhite"),
               names_to = "variable") %>%
  dplyr::select(GEOID10, Geography, visitors, variable, value)

dat_EGC_night_census %>%
  rbind(., dat_WGC_night_census) %>%
  ggplot(., aes(x = value, fill = Geography, weight = visitors, ..scaled..)) + 
  geom_density(alpha = .5) +
  scale_fill_manual(values = palette3,
                    aesthetics = c("colour", "fill"),
                    name = "Visitor Count \n(Quintile)") +
  facet_wrap( ~ variable, scales = "free") +
  plotTheme() +
  labs(title = 'East Girard vs. West Girard Corridors')

dat_EGC_night_census %>%
  rbind(., dat_WGC_night_census) %>%
  ggplot(., aes(x = value, fill = Geography, weight = visitors, ..scaled..)) + 
  geom_density(alpha = .5) +
  scale_fill_manual(values = palette3,
                    aesthetics = c("colour", "fill"),
                    name = "Corridor") +
  facet_wrap( ~ variable, scales = "free") +
  plotTheme() +
  labs(title = 'East Girard vs. West Girard Corridors')

dat_EGC_night_census %>%
  rbind(., dat_WGC_night_census) %>%
  ggplot(., aes(x = value, color = Geography)) +
  geom_freqpoly(lwd = 1) +
  scale_fill_manual(values = palette3,
                    aesthetics = c("colour", "fill"),
                    name = "Visitor Count \n(Quintile)") +
  facet_wrap( ~ variable, scales = "free") +
  plotTheme() +
  labs(title = 'East Girard vs. West Girard Corridors')

#######################
# 2nd & GIRARD ANALYSIS
#######################
dat_2G_cbg <- 
  dat_SecondGirard %>%
  select(safegraph_place_id, 
         date_range_start, 
         top_category, 
         sub_category, 
         visitor_home_cbgs, 
         geometry) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>%
  separate(.,
           visitor_home_cbgs,
           c("cbg_origin", "visitors"),
           sep = ":") %>%
  mutate(cbg_origin = as.numeric(cbg_origin),
         visitors = as.numeric(visitors))

dat_2G_night <- 
  dat_2G_cbg %>%
  filter(top_category == "Restaurants and Other Eating Places" |
           top_category == "Drinking Places (Alcoholic Beverages)") %>%
  group_by(cbg_origin) %>%
  summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry) 

## Quintile
dat_2G_night %>% 
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = q5(visitors), geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = SecondGirard_corr, color = "red", fill = "transparent", lwd = 1) +
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Visitor Count \n(Quintile)") +
  mapTheme() +
  labs(title = "Second & Girard Nightlife: Where do visitors come from?") 

## Continuous
dat_2G_night %>% 
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = visitors, geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = SecondGirard_corr, color = "red", fill = "transparent", lwd = 1) +
  scale_fill_viridis() +
  mapTheme() +
  labs(title = "Second & Girard Nightlife: Where do visitors come from?") 

dat_SecondGirard <- dat2[SecondGirard_corr,]

dat_2G_cbg <- 
  dat_SecondGirard %>%
  select(safegraph_place_id, 
         date_range_start, 
         top_category, 
         sub_category, 
         visitor_home_cbgs, 
         geometry) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>%
  separate(.,
           visitor_home_cbgs,
           c("cbg_origin", "visitors"),
           sep = ":") %>%
  mutate(cbg_origin = as.numeric(cbg_origin),
         visitors = as.numeric(visitors))

dat_2G_night <- 
  dat_2G_cbg %>%
  filter(top_category == "Restaurants and Other Eating Places" | 
           top_category == "Drinking Places (Alcoholic Beverages)" ) %>%
  group_by(cbg_origin) %>%
  summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry)

#join to census information and pivot longer
dat_2G_night_census <- 
  dat_2G_night %>%
  dplyr::select(GEOID10, visitors) %>%
  left_join(phl_blockgroups, by = "GEOID10") %>%
  mutate(Geography = "2nd and Girard") %>%
  pivot_longer(.,
               cols = c("MedAge", "MedHHInc", "MedRent", "pctWhite"),
               names_to = "variable") %>%
  dplyr::select(GEOID10, Geography, visitors, variable, value)

# East Girard data
dat_2G_night <- 
  dat_2G_cbg %>%
  filter(top_category == "Restaurants and Other Eating Places" | 
           top_category == "Drinking Places (Alcoholic Beverages)" ) %>%
  group_by(cbg_origin) %>%
  summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry)

#join to census information and pivot longer
dat_2G_night_census <- 
  dat_2G_night %>%
  dplyr::select(GEOID10, visitors) %>%
  left_join(phl_blockgroups, by = "GEOID10") %>%
  mutate(Geography = "2nd & Girard") %>%
  pivot_longer(.,
               cols = c("MedAge", "MedHHInc", "MedRent", "pctWhite"),
               names_to = "variable") %>%
  dplyr::select(GEOID10, Geography, visitors, variable, value)

dat_2G_night_census %>%
  rbind(., dat_EGC_night_census) %>%
  ggplot(., aes(x = value, fill = Geography, weight = visitors, ..scaled..)) + 
  geom_density(alpha = .5) +
  scale_fill_manual(values = palette3,
                    aesthetics = c("colour", "fill"),
                    name = "Visitor Count \n(Quintile)") +
  facet_wrap( ~ variable, scales = "free") +
  plotTheme() +
  labs(title = 'East Girard vs. 2nd & Girard Corridors')

##################
# CORRIDOR DATASET
##################
dat_corridors <- 
  dat2[phl_corridors,] %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>%
  separate(.,
           visitor_home_cbgs,
           c("cbg_origin", "visitors"),
           sep = ":") %>%
  st_join(phl_corridors) 

dat_corridors <-
  dat_corridors %>% 
  mutate(cbg_origin = as.numeric(cbg_origin),
         visitors = as.numeric(visitors)) %>%
  rename(name = NAME.y) %>%
  st_drop_geometry() %>%
  rename(GEOID10 = cbg_origin) %>%
  mutate(GEOID10 = as.numeric(GEOID10)) %>%
  left_join(phl_blockgroups) %>%
  select(safegraph_place_id, 
       date_range_start, 
       top_category, 
       sub_category, 
       GEOID10,
       visitors, 
       geometry,
       name, 
       MedAge,
       MedHHInc,
       MedRent,
       pctWhite) %>%
  pivot_longer(.,
               cols = c("MedAge", "MedHHInc", "MedRent", "pctWhite"),
               names_to = "variable")
  
  %>%
  filter(top_category == "Restaurants and Other Eating Places" | 
           top_category == "Drinking Places (Alcoholic Beverages)" |
           name == "East Girard" | 
           name == "West Girard" | 
           name == "Pine Street" |
           name == "36th and Lancaster" |
           name == "Broad and Morris") %>%
  ggplot(., aes(x = value, fill = name, weight = visitors, ..scaled..)) + 
  geom_density(alpha = .5) +
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Corridor") +
  facet_wrap( ~ variable, scales = "free") +
  plotTheme() 



# dat_corridors <-
#   dat2[phl_corridors,] %>%
#   mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
#   mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
#   mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
#   mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
#   unnest(visitor_home_cbgs) %>%
#   separate(.,
#            visitor_home_cbgs,
#            c("cbg_origin", "visitors"),
#            sep = ":") %>%
#   mutate(cbg_origin = as.numeric(cbg_origin),
#          visitors = as.numeric(visitors)) %>%
#   filter(top_category == "Drinking Places (Alcoholic Beverages)" |
#            top_category == "Restaurants and Other Eating Places" |
#            top_category == "Traveler Accommodation" |
#            top_category == "Gambling Industries" |
#            top_category == "Promoters of Performing Arts, Sports, and Similar Events") %>%
#   st_join(phl_corridors) %>%
#   select(safegraph_place_id, 
#          date_range_start, 
#          top_category, 
#          sub_category, 
#          cbg_origin,
#          visitors,
#          NAME.y,
#          P_DIST,
#          geometry)
# 
# dat_corridors %>%
#   ggplot() +
#   geom_sf(data = phl_cbg, fill = "grey80", color = "transparent") +
#   geom_sf(data = phl_corridors, aes(fill = P_DIST))
# 
# 
# dat_corridors %>%
#   group_by(cbg_origin, P_DIST) %>%
#   summarize(visitors = sum(visitors)) %>%
#   st_drop_geometry() %>%
#   rename(., GEOID10 = cbg_origin) %>%
#   left_join(phl_cbg, by = "GEOID10") %>%
#   st_as_sf() %>%
#   drop_na(geometry) %>% 
#   ggplot() +
#   geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
#   geom_sf(aes(fill = visitors, geometry = geometry), color = "transparent") + 
#   geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
#   geom_sf(data = WestGirard_corr, color = "red", fill = "transparent", lwd = .5) +
#   scale_fill_viridis() +
#   mapTheme() +
#   labs(title = "East Girard Restaurants: Where do visitors come from?") +
#   facet_wrap(~top_category)
# 
# dat_corridors %>%
#   filter(top_category == "Restaurants and Other Eating Places") %>%
#   group_by(cbg_origin, P_DIST) %>%
#   summarize(visitors = sum(visitors)) %>%
#   st_drop_geometry() %>%
#   rename(., GEOID10 = cbg_origin) %>%
#   left_join(phl_cbg, by = "GEOID10") %>%
#   st_as_sf() %>%
#   drop_na(geometry) %>%
#   ggplot() +
#   geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
#   geom_sf(aes(fill = q5(visitors), geometry = geometry), color = "transparent") + 
#   geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
#   geom_sf(data = fishtown, color = "red", fill = "transparent", lwd = 1) +
#   scale_fill_manual(values = palette5,
#                     aesthetics = c("colour", "fill"),
#                     name = "Visitor Count \n(Quintile)") +
#   mapTheme() +
#   labs(title = "Central District Restaurants: Where do visitors come from?") %>%
#   facet_wrap(~P_DIST)