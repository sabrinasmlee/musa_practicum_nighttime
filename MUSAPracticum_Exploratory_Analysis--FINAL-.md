MUSA Practicum Nighttime Economy: Exploratory Analysis
================
Maddy Kornhauser, Brian Rawn, Sabrina Lee
2/22/2021

  - [1. Use Case Development](#use-case-development)
  - [2. The SafeGraph Dataset](#the-safegraph-dataset)
  - [3. Exploratory Data Analysis](#exploratory-data-analysis)
      - [Philadelphia nightlife
        establishments](#philadelphia-nightlife-establishments)
      - [Nightlife hours](#nightlife-hours)
      - [Nightlife visitors](#nightlife-visitors)
          - [Philadelphia visitor
            origins](#philadelphia-visitor-origins)
          - [Philadelphia destinations](#philadelphia-destinations)
      - [Nightlife corridors in
        Philadelphia](#nightlife-corridors-in-philadelphia)
      - [Neighborhood and corridor
        comparisons](#neighborhood-and-corridor-comparisons)
  - [4. Next Steps](#next-steps)

# 1\. Use Case Development

How can we help business improvement districts (BIDs) and small
businesses better understand who is visiting their commercial corridor
for nightlife purposes to further economic recovery efforts following
the COVID-19 pandemic?

  - *Approach*: Use safegraph data to understand how nightlife patterns
    differ across commercial corridors in the city. This analysis would
    allow BIDs and business to understand who is coming to their area
    for nightlife (avg. income, demographics, and from which areas),
    where they are coming from, where and when they are visiting, and
    how these patterns changed before and after COVID-19.

  - *Potential Deliverable*: An interactive economic development tool to
    help BIDs, small businesses, and other public stakeholders
    understand how they size up against other commercial corridors in
    the city and gain basic statistic on their customer base.

# 2\. The SafeGraph Dataset

Safegraph uses anonymized cell phone GPS data to record trips to
commercial points of interest. This data can tell us from where a trip
was made, what time the trip was made, and how long the individual
stated at the point of interest.

  - Pros:
      - Brand new dataset.
      - Lots of unexplored applications and potential insights
  - Cons:
      - Data is new and has a significant amount of incorrectly
        attributed trips, especially in urban areas

# 3\. Exploratory Data Analysis

``` r
dat <- read.csv("./data/moves_2018.csv")
phila <- st_read("./demo/phila.geojson") %>%
  st_transform('ESRI:102728') %>%
  st_as_sf()
```

    ## Reading layer `phila' from data source `C:\Users\mlkor\OneDrive\Documents\GitHub\musa_practicum_nighttime\demo\phila.geojson' using driver `GeoJSON'
    ## Simple feature collection with 20857 features and 28 fields
    ## geometry type:  POINT
    ## dimension:      XY
    ## bbox:           xmin: -75.27877 ymin: 39.87417 xmax: -74.95779 ymax: 40.13447
    ## geographic CRS: NAD83

``` r
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

phl_cbg <- st_read("http://data.phl.opendata.arcgis.com/datasets/2f982bada233478ea0100528227febce_0.geojson") %>%
  st_transform('ESRI:102728') %>%
  mutate(GEOID10 = as.numeric(GEOID10))
```

    ## Reading layer `Census_Block_Groups_2010' from data source `http://data.phl.opendata.arcgis.com/datasets/2f982bada233478ea0100528227febce_0.geojson' using driver `GeoJSON'
    ## Simple feature collection with 1336 features and 15 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: -75.28031 ymin: 39.86747 xmax: -74.95575 ymax: 40.13793
    ## geographic CRS: WGS 84

``` r
phl_zip <- st_read("http://data.phl.opendata.arcgis.com/datasets/b54ec5210cee41c3a884c9086f7af1be_0.geojson") %>%
  st_transform('ESRI:102728') %>%
  mutate(CODE = as.numeric(CODE))
```

    ## Reading layer `Zipcodes_Poly' from data source `http://data.phl.opendata.arcgis.com/datasets/b54ec5210cee41c3a884c9086f7af1be_0.geojson' using driver `GeoJSON'
    ## Simple feature collection with 48 features and 5 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: -75.28031 ymin: 39.86747 xmax: -74.95575 ymax: 40.13793
    ## geographic CRS: WGS 84

``` r
phl_nhoods <- 
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson") %>%
  st_transform('ESRI:102728') %>% 
  st_as_sf()
```

    ## Reading layer `Neighborhoods_Philadelphia' from data source `https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson' using driver `GeoJSON'
    ## Simple feature collection with 158 features and 8 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -75.28027 ymin: 39.867 xmax: -74.95576 ymax: 40.13799
    ## geographic CRS: WGS 84

``` r
phl_corridors <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson") %>%
  st_transform('ESRI:102728')
```

    ## Reading layer `538ab13a-be43-4ab4-9a25-150600c805e62020329-1-gbhj2k.x1k7m' from data source `http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson' using driver `GeoJSON'
    ## Simple feature collection with 279 features and 79 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -75.27572 ymin: 39.87086 xmax: -74.95575 ymax: 40.13498
    ## geographic CRS: WGS 84

## Philadelphia nightlife establishments

Our first research question is where Philadelphia nightlife
establishments are located across the city. The following maps indicate
where businesses that contribute to the city’s nightlife economy are
located. The categories include:

  - Bars (Drinking Places)
  - Restaurants
  - Hotels (Travel Accomodation)
  - Casinos (Gambling Industries)
  - Arts Venues (Promotes of Performing Arts)

Figure 3.1 below shows the spatial patterns of the business categories.
Bars and restaurants represent the highest number of businesses which
are spread across the city. Hotels are mostly clustered in the central
district of the city and near the airport in the southwest portion of
the city. There are far fewer casinos and arts venues.

Throughout the analysis, we pay special attention to bars and
restaurants, as these organizations are well distributed throughout the
city and apply to a local Philadelphia customer base.

``` r
dat2 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Traveler Accommodation" |
           top_category == "Gambling Industries" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  ggplot() + 
  geom_sf(data = phl_cbg, fill = "grey80", color = "transparent") +
  geom_sf(color = "red", size = .1) +
  labs(title = "Location of Nightlife Establishments",
       subtitle = "Figure 3.1") +
  facet_wrap(~top_category) +
  mapTheme()
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/establishment locations-1.png" width="\textwidth" height="\textheight" />

To look at the spatial patterns another way, we review the distribution
of businesses with a fishnet grid. The fishnet allows us to visualize
clusters and hotspots. Starting first with restaurants, Figure 3.2 below
demonstrates the spatial patterns of thsee businesses. Though
restaurants are well distributed throughout the city, there appears to
be a higher concentration of yellow cells (indicating a higher count of
restaurants) in the central part of the city. This corresponds to what
we observe in the point data analysis.

``` r
#Unnesting popularity by hour variable
dat_hour <- 
  dat2 %>% 
  select(safegraph_place_id, 
         top_category, 
         sub_category, 
         popularity_by_hour, 
         poi_cbg) %>% #select variables
  mutate(popularity_by_hour = str_remove_all(popularity_by_hour, pattern = "\\[|\\]")) %>%   unnest(popularity_by_hour) %>% #unnest values
  separate(.,
           popularity_by_hour,
           c("0", "1", "2", "3", "4", "5", "6", 
             "7", "8", "9", "10", "11", "12", 
             "13", "14", "15", "16", "17", "18",
             "19", "20", "21", "22", "23"),
           sep = ",") %>% #separate into columns by hour
  pivot_longer(cols = 4:27,
               names_to = "Hour",
               values_to = "Visitors") %>% #pivot into long format
  mutate(Hour = as.numeric(Hour),
         Visitors = as.numeric(Visitors))

#Creating a fishnet
phillyBoundary <- 
  phl_zip %>%
  select(geometry) %>%
  st_union() %>%
   st_transform('ESRI:102728') %>% 
  st_as_sf() 

fishnet <- 
  st_make_grid(phillyBoundary, cellsize = 1500, square = FALSE) %>%
  .[phillyBoundary] %>% #MK added this line
  st_sf() %>%
  mutate(uniqueID = rownames(.))

# #Plot fishnet
# ggplot() +
#   geom_sf(data = fishnet, fill = "#440255", color = "black")

#Filter restaurants
restaurants <- dat2 %>%
  filter(top_category == "Restaurants and Other Eating Places")%>%
    st_transform('ESRI:102728')%>%
  mutate(Legend = "Restaurants")

#aggregate restaurant count by fishnet cell
restaurants_net <-
  dplyr::select(restaurants) %>% 
  mutate(countRestaurants = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countRestaurants = replace_na(countRestaurants, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

#Plot restaurant fishnet
ggplot() +
  geom_sf(data = restaurants_net, aes(fill = countRestaurants), color = NA) +
  scale_fill_viridis() +
  labs(title = "Count of Restaurants for Fishnet", 
       subtitle = "Figure 3.2") +
  mapTheme()
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/unnesting hour dataset-1.png" width="\textwidth" height="\textheight" />

Similar to restaurants, Figure 3.3 show that bars are also highly
concentrated around Center City.

``` r
bars <- dat2 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)")%>%
    st_transform('ESRI:102728')%>%
  mutate(Legend = "Bars")

bars_net <-
  dplyr::select(bars) %>% 
  mutate(countBars = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countBars = replace_na(countBars, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE)) %>%
  mutate(Legend = "Bars")

ggplot() +
  geom_sf(data = bars_net, aes(fill = countBars), color = NA) +
  scale_fill_viridis() +
  labs(title = "Count of Bars per Fishnet",
       subtitle = "Figure 3.3") +
  mapTheme()
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/bars grid-1.png" width="\textwidth" height="\textheight" />

The location of perfoming arts companies are more sparsely located
around the city and less concentrated in Center city, as shown in Figure
3.4 below.

``` r
performingarts <- dat2 %>%
  filter(top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  st_transform('ESRI:102728') %>%
  mutate(Legend = "Performing Arts")

performingarts_net <-
  dplyr::select(performingarts) %>% 
  mutate(countPerformingarts = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countPerformingarts = replace_na(countPerformingarts, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))
ggplot() +
  geom_sf(data = performingarts_net, aes(fill = countPerformingarts), color = NA) +
  scale_fill_viridis() +
  labs(title = "Count of Performing Arts Companies for Fishnet",
       subtitle = "Figure 3.4") +
  mapTheme()
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/unnamed-chunk-1-1.png" width="\textwidth" height="\textheight" />

Figure 3.5 below includes other sectors that contribute to
Philadelphia’s nightlife economy such as hotels and casinos. This
figure combines the fishnets into a single panel that allows us to
compare distribution of industries across business type.

``` r
#Casinos
casinos <- dat2 %>%
  filter(top_category == "Gambling Industries") %>%
  st_transform('ESRI:102728') %>%
  mutate(Legend = "Casinos")

casinos_net <-
  dplyr::select(casinos) %>% 
  mutate(countCasinos = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countCasinos = replace_na(countCasinos, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

#Hotels
hotels <- dat2 %>%
  filter(top_category == "Traveler Accommodation") %>%
  st_transform('ESRI:102728') %>%
  mutate(Legend = "Hotels")

hotels_net <-
  dplyr::select(hotels) %>% 
  mutate(countHotels = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countHotels = replace_na(countHotels, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

# Combining fishnets into a single dataframe
vars_net <- 
  rbind(restaurants, bars, performingarts, casinos, hotels) %>%
  st_join(., fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID, Legend) %>%
  dplyr::summarize(count = n()) %>%
    full_join(fishnet) %>%
    spread(Legend,count, fill=0) %>%
    st_sf() %>%
    dplyr::select(-`<NA>`) %>%
    na.omit() %>%
    ungroup()

vars_net.long <- 
  gather(vars_net, Variable, value, -geometry, -uniqueID)

vars <- unique(vars_net.long$Variable)
mapList <- list()

#Plotting small multiple maps
for(i in vars){
  mapList[[i]] <- 
    ggplot() +
      geom_sf(data = filter(vars_net.long, Variable == i), aes(fill=value), colour=NA) +
      scale_fill_viridis(name="") +
      labs(title=i) +
      mapTheme()}

do.call(grid.arrange, c(mapList, ncol=3, top="Count of Nightlife Businesses per Fishnet", bottom = "Figure 3.5"))
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/unnamed-chunk-2-1.png" width="\textwidth" height="\textheight" />

## Nightlife hours

Next, we explore when visitors make trips to nightlife establishments.
To do this, we worked with the popularity\_by\_hour SafeGraph variable,
which sums the total number of visitors by hour for each month.

Figure 3.5 shows the average foot traffic by business type over the
course of a day. This graphic indicates that though certain business
types are considererd part of the nightlife economy, they do not
exclusively experience traffic in the evenings. Restaurants are a good
example of this, where the data indicates that the highest levels of
traffic occur in the middle of the day. Arts venues, on the other hand,
have a clear patterns indicating that they are more popular later in the
day.

``` r
dat_hour %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Traveler Accommodation" |
           top_category == "Gambling Industries" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  group_by(Hour, top_category) %>%
  dplyr::summarize(Visitors = mean(Visitors)) %>%
  ggplot(., aes(x = Hour, y = Visitors)) + 
  geom_col() +
  labs(title = "Philadelphia Nightlife Organizations, Average Traffic by Hour",
       subtitle = "Figure 3.5") +
  facet_wrap(~top_category, scales = "free") +
  plotTheme()
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/traffic by nightlife establishment-1.png" width="\textwidth" height="\textheight" />

The following animation shows restaurant visitor counts by census block
group over the course of the day. We observe the highest amount of
traffic in center city in the middle of the day and then a resurgence of
traffic elsewhere in the city in the evening. This suggests that people
dine at neighborhood establishments close to their home in the evening.

``` r
dat_restaurants <-
  dat_hour %>%
  filter(top_category == "Restaurants and Other Eating Places") 

dat_restaurant_filter <-
  dat_restaurants %>%
  dplyr::rename(., GEOID10 = poi_cbg) %>%
  dplyr::group_by(GEOID10, Hour) %>%
  dplyr::summarize(Avg_Popularity = mean(Visitors),
            Total_Visits = sum(Visitors)) %>%
  left_join(phl_cbg) %>% 
  st_as_sf() %>%
  mutate(Visits_Per_Area = Total_Visits / Shape__Area * 100)

#Animation of restaurant popularity by hour
restaurant.animation.data <-
    dat_restaurant_filter %>%
    st_sf() %>%
    mutate(Pop_String = case_when(Visits_Per_Area < .4 ~ "5",
                              Visits_Per_Area >= .4 & Visits_Per_Area <.8 ~ "4",
                              Visits_Per_Area >= .8 & Visits_Per_Area <1.2 ~ "3",
                              Visits_Per_Area >= 1.2 & Visits_Per_Area <1.6 ~ "2",
                              Visits_Per_Area >= 2 ~ "1")) %>%
    mutate(Pop_String  = fct_relevel(Pop_String, "5","4","3","2","1"))

restaurant_animation <-
  ggplot() +
  geom_sf(data = phl_cbg, fill = "#440255", color = "transparent") +   
  geom_sf(data = restaurant.animation.data, aes(fill = Pop_String), color = "transparent") +
    scale_fill_manual(values = palette5) +
    labs(title = "Restaurant Popularity by Hour",
         subtitle = "One Hour Intervals: {current_frame}") +
  theme(panel.background = element_rect(fill = "black"),
         panel.grid.major = element_line(color = "transparent"),
          panel.grid.minor = element_line(colour = "transparent")) +
    transition_manual(Hour)
  

animate(restaurant_animation, duration=20, renderer = gifski_renderer())
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/unnamed-chunk-3-1.gif" width="\textwidth" height="\textheight" />

The following animation shows the same metric for bars. We observe
traffic increasing throughout the day starting in the afternoon.

``` r
#Bar Analysis
#Filter Bars
dat_bars <- dat_hour %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)") 

#Merge CBGs with popularity by hour data
dat_bars_filter <-
  dat_bars %>%
  dplyr::rename(., GEOID10 = poi_cbg) %>%
  dplyr::group_by(GEOID10, Hour) %>%
  dplyr::summarize(Avg_Popularity = mean(Visitors),
            Total_Visits = sum(Visitors)) %>%
  left_join(phl_cbg) %>% 
  st_as_sf() %>%
  mutate(Visits_Per_Area = Total_Visits / Shape__Area * 100)

#Animation of bar popularity by hour
bar.animation.data <-
    dat_bars_filter %>%
    st_sf() %>%
    mutate(Pop_String = case_when(Visits_Per_Area == 0 ~ "5",
                              Visits_Per_Area > 0 & Visits_Per_Area <.2 ~ "4",
                              Visits_Per_Area >= .2 & Visits_Per_Area <.4 ~ "3",
                              Visits_Per_Area >= .4 & Visits_Per_Area <.6 ~ "2",
                              Visits_Per_Area >= .6 ~ "1")) %>%
    mutate(Pop_String  = fct_relevel(Pop_String, "5","4","3","2","1"))

bar_animation <-
  ggplot() +
  geom_sf(data = phl_cbg, fill = "#440255", color = "transparent", ) +
  geom_sf(data = bar.animation.data, aes(fill = Pop_String), color = "transparent") +
    scale_fill_manual(values = palette5) +
    labs(title = "Bar Popularity by Hour",
         subtitle = "One Hour Intervals: {current_frame}") +
  theme(panel.background = element_rect(fill = "black"),
         panel.grid.major = element_line(color = "transparent"),
          panel.grid.minor = element_line(colour = "transparent")) +
    transition_manual(Hour)

animate(bar_animation, duration=20, renderer = gifski_renderer())
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/bar animation-1.gif" width="\textwidth" height="\textheight" />

## Nightlife visitors

### Philadelphia visitor origins

Next we explore where visitors come from. Separated by nightlife
establishment type, Figures 3.6a and 3.6b shows the average distance
travelled from each census block group. The distance is calculated by
measuring the euclidean distance between the centroid of each origin
block group and the destination. Then we calculate the weighted mean of
the distance travelled to account for destinations that people in a
given block group visit more frequently.

For business types that are well-dispersed throughout the city, such as
bars and restaurants, it appears that visitors typically travel shorter
distances. For business types with fewer destinations, such as casinos,
or that are clusters in specific areas of the city, such as hotels, it
is common for visitors to make longer trips to these destinations.

Census block groups colored grey have no recorded visitors to the
business type

``` r
#Preapring dataset to split out by individual cbgs
dat_cbg <- 
  dat2 %>%
  select(safegraph_place_id, 
         date_range_start, 
         top_category, 
         sub_category, 
         poi_cbg, 
         visitor_home_cbgs, 
         geometry) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\[|\\]")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = "\\{|\\}")) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, pattern = '\\"|\\"')) %>%
  mutate(visitor_home_cbgs = str_split(visitor_home_cbgs, pattern = ",")) %>%
  unnest(visitor_home_cbgs) %>%
  separate(.,
           visitor_home_cbgs,
           c("Visitor_CBG", "Visitors"),
           sep = ":") %>%
  mutate(Visitor_CBG = as.numeric(Visitor_CBG),
         poi_cbg = as.numeric(poi_cbg),
         Visitors = as.numeric(Visitors))

cbg_origin <- #takes a really long time!
  dat_cbg %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Traveler Accommodation" |
           top_category == "Gambling Industries" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>% #filter for nightlife establishments
  st_drop_geometry() %>% #drop geometry to join with cbg file
  dplyr::rename(., GEOID10 = Visitor_CBG) %>% #renaming to match cbg file
  left_join(phl_cbg, by = "GEOID10") %>% # join to cbg file
  select(safegraph_place_id, 
         poi_cbg, 
         GEOID10, 
         Visitors, 
         geometry) %>% #clean up dataset
  rename(., cbg_origin = GEOID10, #clean up column names
         cbg_dest = poi_cbg,
         geometry_origin = geometry) %>%
  left_join(phila, by = "safegraph_place_id") %>% #join back to the SafeGraph locations
  select(safegraph_place_id, 
         top_category, 
         sub_category, 
         cbg_dest, 
         cbg_origin, 
         Visitors, 
         geometry_origin, 
         geometry) %>%
  rename(., geometry_dest = geometry) %>%
  drop_na(geometry_origin) %>% #removing geometries that didn't match (ie outside philily)
  mutate(distance = mapply(st_distance, st_centroid(geometry_origin), geometry_dest)) #calculate distance
```

### Philadelphia destinations

Next, we study how far do Philadelphia visitors travel to nightlife
establishemnts. These maps show the average distance travelled to each
destination. Similar to the above, distance travelled is calculated by
meauring the euclidena distance from each destination to the centroid of
each visitor’s block group. The metric shown here represented the
weighted average of trips and distances.

In general, we see that visitors travel further to Center City
destinations. Importantly, these graphics exclude all trips that
originate outside of Philadelphia.

``` r
cbg_dest <-
  dat_cbg %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Traveler Accommodation" |
           top_category == "Gambling Industries" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  st_drop_geometry() %>%
  rename(., GEOID10 = Visitor_CBG) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  select(safegraph_place_id, 
         GEOID10, 
         poi_cbg, 
         Visitors, 
         geometry) %>%
  rename(., cbg_origin = GEOID10,
         geometry_origin = geometry,
         cbg_dest = poi_cbg) %>%
  left_join(phila, by = "safegraph_place_id") %>%
  select(safegraph_place_id, 
         top_category, 
         sub_category, 
         cbg_origin, 
         cbg_dest, 
         Visitors, 
         geometry_origin, 
         geometry) %>%
  rename(., geometry_dest = geometry) %>%
  drop_na(geometry_origin) %>% #dropping origins outside of philadelphia
  mutate(distance = mapply(st_distance, st_centroid(geometry_origin), st_centroid(geometry_dest)))
```

``` r
#Continuous
cbg_dest %>%
  group_by(safegraph_place_id, top_category) %>%
  summarize(avg_distance = weighted.mean(distance, Visitors)) %>%
  dplyr::select(safegraph_place_id, avg_distance) %>%
  left_join(phila, by = "safegraph_place_id") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(color = avg_distance), size = 1) + 
  scale_fill_viridis(aesthetics = "color") +
  mapTheme() +
  labs(title = "Which nightlife destinations do visitors travel the furthest?",
       subtitle = "Figure 3.7a") +
  facet_wrap(~top_category)
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/destination viz-1.png" width="\textwidth" height="\textheight" />

``` r
#Quintile
cbg_dest %>%
  group_by(safegraph_place_id, top_category) %>%
  summarize(avg_distance = weighted.mean(distance, Visitors)) %>%
  dplyr::select(safegraph_place_id, avg_distance) %>%
  left_join(phila, by = "safegraph_place_id") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(color = q5(avg_distance)), size = 1) + 
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Average Distance \n(Quintile)") +
  mapTheme() +
  labs(title = "Which nightlife destinations do visitors travel the farthest?",
       subtitle = "Figure 3.7b") +
  facet_wrap(~top_category)
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/destination viz-2.png" width="\textwidth" height="\textheight" />

## Nightlife corridors in Philadelphia

Next, we observe foot traffic along Philadelphia’s commercial corridors.
This analysis relies on a shapefile from the City of Philadelphia’s
Planning department which demarcates individual corridors and districts
throughout the city. According to the [available
metadata](https://metadata.phila.gov/#home/datasetdetails/564236a55737e1f263ae5e3f/representationdetails/56423a4e902dbdd813db9a55/)
“locations range from large, regional and specialty destinations to
corridors that reflect the evolving economy, culture, and aesthetic
traditions of surrounding neighborhoods.” This means that the
gegoraphies vary in size and character.

Figure 3.8 below maps the commercial corridors and shows the
corridor-wide vacancy rate, which varies from close to 0% to upwards of
60% throughout the city. Smaller corridors in far West Philadelphia,
North Philadelphia tend to have the highest vacancy rates.

``` r
###Corridors
#Load Philadelphia commercial corridors dataset and transform
corridors <- phl_corridors

dat_restaurants <-
dat_hour %>%
  filter(top_category == "Restaurants and Other Eating Places") 

corridors_filter <- corridors %>%
  select(OBJECTID, NAME, GLA, P_DIST, ST_EXT, PT_ADD, VAC_RATE) %>%
  mutate(VAC_RATE = str_remove_all(VAC_RATE, pattern = "%")) %>%
  mutate(VAC_RATE = as.numeric(VAC_RATE)) %>%
  st_as_sf() 
# %>%
#   st_transform('ESRI:102271') 
  
# Plot commercial corridors with colors as vacancy rate
ggplot() +
  geom_sf(data = phillyBoundary, fill = "black") +
  geom_sf(data = corridors_filter, aes(fill = VAC_RATE), color = "transparent") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Philadelphia Commercial Corridors",
       subtitle = "Figure 3.8") + 
  mapTheme()
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/unnamed-chunk-4-1.png" width="\textwidth" height="\textheight" />

Figure 3.9 below breaks down the count of restaunts in a given corridor
per square miile. We find that central corridors and districts tend to
have more restaurants per square mile. Other corridors. That said,
corridors across the city have a high concentration of restaurants as
well.

``` r
###Restaurants (takes a really long time)
dat_restaurants_grid <- dat_restaurants %>%
  select(geometry) %>%
  na.omit() %>%
  st_as_sf() %>%
  #st_transform('ESRI:102271') %>% 
    distinct()

#Merge restaurants with commercial corridors 
corridors_restaurants <- 
  select(dat_restaurants_grid) %>% 
  mutate(countRestaurant = 1) %>% 
  aggregate(., corridors_filter, sum) %>%
  mutate(countRestaurant = replace_na(countRestaurant, 0),
         uniqueID = rownames(.),
         area = st_area(geometry) * .00000038610,
         count_per_mile = as.numeric(countRestaurant / area),
         cvID = sample(round(nrow(corridors_filter) / 24), size=nrow(corridors_filter), replace = TRUE))

#Plot map of number of restaurants per square mile
ggplot() +
  geom_sf(data = phillyBoundary, fill = "black") +
  geom_sf(data = corridors_restaurants, aes(fill = count_per_mile), color = "transparent") +
  scale_fill_viridis(trans = "sqrt") +
  labs(title = "Count of Restaurants per Square Mile in Each Commercial Corridor",
       subtitle = "Figure 3.9") +
  mapTheme()
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/unnamed-chunk-5-1.png" width="\textwidth" height="\textheight" />

Figure 3.10 shows the number of bars in each commercial corridor per
square mile. Again, we see the corridors around Center City generally
showing a higher concentration of bars.

``` r
###Bars
dat_bars_grid <- dat_bars %>%
  select(geometry) %>%
  #na.omit() %>%
  st_as_sf() %>%
  # st_transform('ESRI:102271') %>% 
    distinct()
  
corridors_bars <- 
  dplyr::select(dat_bars_grid) %>% 
  mutate(countBar = 1) %>% 
  aggregate(., corridors_filter, sum) %>%
  mutate(countBar = replace_na(countBar, 0),
         uniqueID = rownames(.),
         area = st_area(geometry) * .00000038610,
         count_per_mile = as.numeric(countBar / area),
         cvID = sample(round(nrow(corridors_filter) / 24), size=nrow(corridors_filter), replace = TRUE))

#Plot map of number of bars per square mile
ggplot() +
  geom_sf(data = phillyBoundary, fill = "black") +
  geom_sf(data = corridors_bars, aes(fill = count_per_mile), color = "transparent") +
  scale_fill_viridis(trans = "sqrt") +
  labs(title = "Count of Bars per Square Mile in Each Commercial Corridor",
       subtitle = "Figure 3.10") +
  mapTheme()
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/Bars by corridor-1.png" width="\textwidth" height="\textheight" />

## Neighborhood and corridor comparisons

Finally, we began exploring businesses at the corridor level and
understand how different corridors attract different visitors from
across the city. We plan to systematically expand the scope of this
study to other corridors across the city. We also believe that this data
is particularly relevant to business and economic development
professionals in the COVID-19 recovery efforts.

Using the Commercial Corridor shapefile available on Open Data Philly,
we selected two corridors to compare: East Girard Avenue, a central
corridor in the Fishtown neighborhood, and West Girard Avenue in
Brewerytown.

``` r
#East Girard Corridor
EastGirard_corr <- 
  phl_corridors %>% 
  filter(NAME == "East Girard") %>% 
  st_as_sf()

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
         visitors = as.numeric(visitors)) %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Traveler Accommodation" |
           top_category == "Gambling Industries" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
    mutate(corridor = "East Girard")

#West Girard
WestGirard_corr <- 
  phl_corridors %>% 
  filter(NAME == "West Girard") %>% 
  st_as_sf()

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
         visitors = as.numeric(visitors)) %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)" |
           top_category == "Restaurants and Other Eating Places" |
           top_category == "Traveler Accommodation" |
           top_category == "Gambling Industries" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  mutate(corridor = "West Girard")
```

Figures 3.11a and 3.11b show the volume of visitors to the East Girard
Corridor (outlined in red) by census block group. While visitors from
across the city frequent East Girard corridor, particularly for its
restaurants, Figure 3.11b clarifies that the corridor mostly caters to a
local customer base of surrounding census tracts.

Block groups shaded gray indicate that no resident made a trip to this
corridor.

``` r
dat_EGC_cbg %>%
  dplyr::group_by(cbg_origin, top_category) %>%
  dplyr::summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  dplyr::rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry) %>% 
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = q5(visitors), geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = EastGirard_corr, color = "red", fill = "transparent", lwd = 1) +
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Visitor Count \n(Quintile)") +
  mapTheme() +
  labs(title = "East Girard Restaurants: Where do visitors come from?",
       subtitle = "Figure 3.11a") +
  facet_wrap(~top_category)
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/unnamed-chunk-6-1.png" width="\textwidth" height="\textheight" />

``` r
dat_EGC_cbg %>%
  dplyr::group_by(cbg_origin, top_category) %>%
  dplyr::summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  dplyr::rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry) %>% 
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = visitors, geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = EastGirard_corr, color = "red", fill = "transparent", lwd = .5) +
  scale_fill_viridis() +
  mapTheme() +
  labs(title = "East Girard Restaurants: Where do visitors come from?",
       subtitle = "Figure 3.8b") +
  facet_wrap(~top_category)
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/unnamed-chunk-6-2.png" width="\textwidth" height="\textheight" />

Figures 3.12a and 3.12b show the volume of visitors to West Girard
corridor. While the corridor appears to draw visitors from fewer census
tracts overall, the corridor draws from a similarly local customer base
of the surrounding block groups.

As before, block groups shaded gray have no residents that made trips to
this corridor.

``` r
dat_WGC_cbg %>%
  dplyr::group_by(cbg_origin, top_category) %>%
  dplyr::summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  dplyr::rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry) %>% 
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = q5(visitors), geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = WestGirard_corr, color = "red", fill = "transparent", lwd = 1) +
  scale_fill_manual(values = palette5,
                    aesthetics = c("colour", "fill"),
                    name = "Visitor Count \n(Quintile)") +
  mapTheme() +
  labs(title = "West Girard: Where do visitors come from?",
       subtitle = "Figure 3.12a") +
  facet_wrap(~top_category)
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/West Girard viz-1.png" width="\textwidth" height="\textheight" />

``` r
dat_WGC_cbg %>%
  dplyr::group_by(cbg_origin, top_category) %>%
  dplyr::summarize(visitors = sum(visitors)) %>%
  st_drop_geometry() %>%
  dplyr::rename(., GEOID10 = cbg_origin) %>%
  left_join(phl_cbg, by = "GEOID10") %>%
  st_as_sf() %>%
  drop_na(geometry) %>% 
  ggplot() +
  geom_sf(data = phl_cbg, fill = "grey70", color = "transparent") +
  geom_sf(aes(fill = visitors, geometry = geometry), color = "transparent") + 
  geom_sf(data = phl_nhoods, color = "white", fill = "transparent") +
  geom_sf(data = WestGirard_corr, color = "red", fill = "transparent", lwd = .5) +
  scale_fill_viridis() +
  mapTheme() +
  labs(title = "East Girard Restaurants: Where do visitors come from?",
       subtitle = "Figure 3.12b") +
  facet_wrap(~top_category)
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/West Girard viz-2.png" width="\textwidth" height="\textheight" />

We can further explore the individual corridor analysis by tying the
trip origins to census data. This allows us to construct a customer
profile of the people visiting specific corridors throughout the city.

``` r
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
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |==                                                                    |   4%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |=========================                                             |  35%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  55%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%

The below Figure 3.13 compares demographic indicators pulled from the
census across the two customer bases. East Girard corridor’s customer
base, shown in purple, tend to come from whiter census tracts with a
higher median household income and median rent. West Girard corridor’s
customer base tend to come from less white census tracts with lower
incomes and median rent. Both corridors, however, pull from census
tracts with a similar median age.

``` r
rbind(dat_EGC_cbg, dat_WGC_cbg) %>%
  rename(GEOID10 = cbg_origin) %>%
  dplyr::select(GEOID10, visitors, corridor) %>%
  st_drop_geometry() %>%
  left_join(phl_blockgroups, by = "GEOID10") %>%
  pivot_longer(.,
               cols = c("MedAge", "MedHHInc", "MedRent", "pctWhite"),
               names_to = "variable") %>%
  dplyr::select(GEOID10, corridor, visitors, variable, value) %>%
  ggplot(., aes(x = value, fill = corridor, weight = visitors)) + 
  geom_density(alpha = .5) +
  scale_fill_manual(values = palette3,
                    aesthetics = c("colour", "fill"),
                    name = "Visitor Count \n(Quintile)") +
  facet_wrap( ~ variable, scales = "free") +
  plotTheme() +
  labs(title = 'East Girard vs. West Girard Corridors',
       subtitle = "Figure 3.13")
```

<img src="MUSAPracticum_Exploratory_Analysis--FINAL-_files/figure-gfm/corridor comparison-1.png" width="\textwidth" height="\textheight" />

# 4\. Next Steps

We will continue our Exploratory Data Analysis and plan to focus on the
following topics:

  - Defining and demarcating distinct Philadelphia nightlife corridors.
  - Expanding analysis to the regional scale. Currently we only review
    trips originating within Phialdelphia, but it will be important to
    understand which districts have a more regional approach.
  - Understand how COVID-19 has impacted nightlife foot traffic in
    Philadelphia? Have corridors been impacted evenly?