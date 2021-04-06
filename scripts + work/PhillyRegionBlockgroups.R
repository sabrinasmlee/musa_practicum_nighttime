#Load packages
library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)

#Load philly boundary
phl_boundary <- 
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728') 

#Demographic data
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

pa_nj_blockgroups <- rbind(pa_blockgroups, nj_blockgroups)

#Map geographies
ggplot() + 
  geom_sf(data = pa_nj_blockgroups) + 
  geom_sf(data = phl_boundary, color = "red", fill = "transparent", lwd = 2)
