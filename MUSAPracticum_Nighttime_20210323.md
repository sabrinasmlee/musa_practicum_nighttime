MUSA Practicum Nighttime Economy: Exploratory Analysis
================
Maddy Kornhauser, Brian Rawn, Sabrina Lee
3/23/2021

  - [0. Weekly Updates 3/23/2021](#weekly-updates-3232021)
  - [1. Use Case Development](#use-case-development)
      - [Purpose](#purpose)
      - [The Tool](#the-tool)
      - [Applications](#applications)
  - [2. The SafeGraph Dataset](#the-safegraph-dataset)
  - [3. Exploratory Data Analysis](#exploratory-data-analysis)
      - [Philadelphia nightlife
        establishments](#philadelphia-nightlife-establishments)
      - [Nightlife hours](#nightlife-hours)
      - [Trip Flows](#trip-flows)
          - [Origins & destinations](#origins-destinations)
      - [Nightlife corridors in
        Philadelphia](#nightlife-corridors-in-philadelphia)
      - [Visit duration to bars and restaurants in
        Philadelphia](#visit-duration-to-bars-and-restaurants-in-philadelphia)
      - [Neighborhood and corridor
        comparisons](#neighborhood-and-corridor-comparisons)
  - [4. Prediction](#prediction)
  - [5. Next Steps](#next-steps)

# 0\. Weekly Updates 3/23/2021

This week, we have reworked our Exploratory Data Analysis to incorporate
corridor typology. We have also begun doing work on the prediciton
model. Detailed update below:

  - Corridor exploratory data analysis: We have incorporated corridor
    typologies into our analysis as well as logitudinal data on trips
    over the course of a day.
  - For our prediction we have changed our independent variable from the
    raw visit counts to a subset of visits during the evening hours.
    This requires that we make some assumption about the global
    SafeGraph variables, but we feel that this predicting ultimately
    serves our use case better.
  - We have continued to work on our wireframe where we plan to showcase
    the following metrics.
      - Corridor popularity over time: This metric looks at the volume
        of trips over time. Since we want to stay away from generating
        raw counts, we will likely show this as a percent change in foot
        traffic over time. We will likely use the popularity\_by\_hour
        and popularity\_by\_day variables in the SafeGraph dataset.
      - Demographic characteristics: Based on the origin census tracts,
        we will look at the following demographic indicators. These will
        likely be presented as a weighted average based on how many
        visitors from that census tract: Age (Median Age variable),
        Income (Median Household Income variable), Race / Ethnicity,
        Educational Attainment, Household composition.
      - Average distance travelled to nightlife destinations: the
        weighted average of the origin block group centroid and the
        desitnation corridor centroid.
      - Average dwell time by commercial use: Aggregate SafeGraph’s
        median\_dwell variable by businesses in the commercial corridor.
      - The percentage drop in trips that nightlife establishments
        experienced as a result of COVID-19.
  - Prediction Parameters: We plan to predict the number of trips by
    commercial corridor. This prediciton will be transformed into a
    percentage increase or decrease relative to average traffic.
  - Feature Engineering:
      - So far distance\_from\_home appears to have the strongest
        relationship with the number of trips.
      - We have started looking at clustering and counts of amenities
        along corridors, but there doesn’t seem to be a particularly
        strong relationship.
      - Tips on how to mine SafeGraph data for meaningful variables.
      - Want to incorporate outside data next: Census, transit, land
        use, vacancy, building foot print.

# 1\. Use Case Development

## Purpose

Our goal is to help business improvement districts (BIDs) and small
businesses understand the patterns of nightlife across Philadelphia’s
commercial corridors in order to further recovery efforts following the
COVID-19 pandemic.

## The Tool

An interactive data dashboard that allows users to understand nightlife
patterns at the corridor level across Philadelphia. This dashboard
would, for each corridor, answer the questions of “who, what, where, and
when?” of nightlife visitors. The dashboard would also allow for
comparisons to other corridors. Potential metrics that would be
displayed include:

  - Corridor popularity (volume of trips) over time.
  - The percentage of visitors attributable to different origin census
    tracts and a breakdown of associated demographic characteristics.
  - Average distance traveled to restaurants/bars/theaters.
  - Average dwell time by commercial use.
  - The percentage drop in trips that restaurants/bars/theaters
    experienced as a result of COVID-19.
  - For each, a comparison to the average of other commercial corridors.

**(In progress)** In addition, the tool will have the additional
functionality of being able to predict the future popularity of a
commercial corridor given changes in time (hour, day, month), weather,
volume of commercial establishments (count or floor area), and retail
mix. This predictive functionality would help inform decisions relating
to the regulation and expansion of nightlife activities.

## Applications

How will this tool be utilized for economic development? Potential
applications include use by BIDs and small businesses to:

  - Understand peak trip times by use and location in order to inform
    parking, transit, or commercial policies.
  - Understand relative popularity of commercial corridors to inform
    decisions to grant licenses or small business assistance.
  - Understand origins and demographics of visitors to more effectively
    target marketing resources.
  - Understand the future effect of more commercial establishments or
    square footage in a given commercial corridor.

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

#Planning district shapefiles (projected & unprojected)
phl_dist <-
  st_read("http://data.phl.opendata.arcgis.com/datasets/0960ea0f38f44146bb562f2b212075aa_0.geojson", 
          quiet = TRUE) %>%
  st_transform('ESRI:102728')

phl_dist_unproj <-
  st_read("http://data.phl.opendata.arcgis.com/datasets/0960ea0f38f44146bb562f2b212075aa_0.geojson", 
          quiet = TRUE)

#Philadelphia zip codes
phl_zip <- st_read("http://data.phl.opendata.arcgis.com/datasets/b54ec5210cee41c3a884c9086f7af1be_0.geojson", quiet = TRUE) %>%
  st_transform('ESRI:102728') %>%
  mutate(CODE = as.numeric(CODE))
```

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
           # top_category == "Traveler Accommodation" |
           # top_category == "Gambling Industries" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  ggplot() + 
  geom_sf(data = phl_cbg, fill = "grey80", color = "transparent") +
  geom_sf(color = "red", size = .1) +
  labs(title = "Location of Nightlife Establishments",
       subtitle = "Figure X.X") +
  facet_wrap(~top_category, nrow = 1) +
  mapTheme()
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/establishment locations-1.png" width="100%" />

To look at the spatial patterns another way, we review the distribution
of businesses with a fishnet grid. The fishnet allows us to visualize
clusters and hotspots. Starting first with restaurants, Figure X.X below
demonstrates the spatial patterns of these businesses. Though
restaurants are well distributed throughout the city, there appears to
be a higher concentration of yellow cells (indicating a higher count of
restaurants) in the central part of the city. This corresponds to what
we observe in the point data analysis.

``` r
#Unnesting popularity by hour variable
dat_hour <- 
  dat2 %>% 
  select(safegraph_place_id, top_category, sub_category, popularity_by_hour, poi_cbg, median_dwell) %>%
  mutate(popularity_by_hour = str_remove_all(popularity_by_hour, pattern = "\\[|\\]")) %>%
  unnest(popularity_by_hour) %>%
  separate(.,
           popularity_by_hour,
           c("0", "1", "2", "3", "4", "5", "6", 
             "7", "8", "9", "10", "11", "12", 
             "13", "14", "15", "16", "17", "18",
             "19", "20", "21", "22", "23"),
           sep = ",") %>%
  pivot_longer(cols = 4:27,
               names_to = "Hour",
               values_to = "Count") %>%
  mutate(Hour = as.numeric(Hour),
         Count = as.numeric(Count))

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
```

Figure X.X below includes other sectors that contribute to
Philadelphia’s nightlife economy such as hotels and casinos. This
figure combines the fishnets into a single panel that allows us to
compare distribution of industries across business type.

``` r
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

#Bars
bars <- dat2 %>%
  filter(top_category == "Drinking Places (Alcoholic Beverages)")%>%
    st_transform('ESRI:102728')%>%
  mutate(Legend = "Bars")

#aggregate bars by fishnet cell
bars_net <-
  dplyr::select(bars) %>% 
  mutate(countBars = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countBars = replace_na(countBars, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE)) %>%
  mutate(Legend = "Bars")

# #Casinos
# casinos <- dat2 %>%
#   filter(top_category == "Gambling Industries") %>%
#   st_transform('ESRI:102728') %>%
#   mutate(Legend = "Casinos")
# 
# casinos_net <-
#   dplyr::select(casinos) %>% 
#   mutate(countCasinos = 1) %>% 
#   aggregate(., fishnet, sum) %>%
#   mutate(countCasinos = replace_na(countCasinos, 0),
#          uniqueID = rownames(.),
#          cvID = sample(round(nrow(fishnet) / 24), 
#                        size=nrow(fishnet), replace = TRUE))

#Performing arts
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

# #Hotels
# hotels <- dat2 %>%
#   filter(top_category == "Traveler Accommodation") %>%
#   st_transform('ESRI:102728') %>%
#   mutate(Legend = "Hotels")
# 
# hotels_net <-
#   dplyr::select(hotels) %>% 
#   mutate(countHotels = 1) %>% 
#   aggregate(., fishnet, sum) %>%
#   mutate(countHotels = replace_na(countHotels, 0),
#          uniqueID = rownames(.),
#          cvID = sample(round(nrow(fishnet) / 24), 
#                        size=nrow(fishnet), replace = TRUE))

# Combining fishnets into a single dataframe
vars_net <- 
  rbind(restaurants, 
        bars, 
        performingarts
        # , 
        # casinos, 
        # hotels
        ) %>%
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

do.call(grid.arrange, c(mapList, ncol=3, top="Count of Nightlife Businesses per Fishnet", bottom = "Figure X.X"))
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-1-1.png" width="100%" />

## Nightlife hours

Next, we explore when visitors make trips to nightlife establishments.
To do this, we worked with the popularity\_by\_hour SafeGraph variable,
which sums the total number of visitors by hour for each month.

Figure X.X shows the average foot traffic by business type over the
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
           # top_category == "Traveler Accommodation" |
           # top_category == "Gambling Industries" |
           top_category == "Promoters of Performing Arts, Sports, and Similar Events" |
           top_category == "Performing Arts Companies") %>%
  group_by(Hour, top_category) %>%
  dplyr::summarize(Count = mean(Count)) %>%
  ggplot(., aes(x = Hour, y = Count)) + 
  geom_col() +
  labs(title = "Philadelphia Nightlife Organizations, Average Traffic by Hour",
       subtitle = "Figure X.X") +
  facet_wrap(~top_category, scales = "free") +
  plotTheme()
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/traffic by nightlife establishment-1.png" width="100%" />

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
  dplyr::summarize(Avg_Popularity = mean(Count),
            Total_Visits = sum(Count)) %>%
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

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-2-1.gif" width="100%" />

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
  dplyr::summarize(Avg_Popularity = mean(Count),
            Total_Visits = sum(Count)) %>%
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

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/bar animation-1.gif" width="100%" />

## Trip Flows

### Origins & destinations

The SafeGraph data fundamentally captures flow of people across space by
connecting a series of origins and destinations. By mapping the origins
and destinations of trips taking place across Philadlephia, we can
observe which regions of the city draw from a larger crowd across the
city, and which areas cater to a more local population.

The following maps look at the origins and destinations for trips to
restaurants, bars, and arts venues across the city. Specifically, it
shows the distance between the centroid of the origin neighborhood to
the centroid of the destination commercial corridor. We selected 6
corridors across Philadelphia in different areas of the city that we
felt represented a diverse array of commercial corridors in the city.
Going forward, we would like to run a K-means clustering test to help
sort the corridors into like categories that will help us understand the
distinct profiles of corridors in Philadelphia.

The code draws each line segments based on lat/long coordinates of the
origin an destination centroid (could not get this to work with
projected data). Note that trips to destinations located outside of the
commercial corridors are left off of these maps.

The following code block loads unprojected shapefiles for the block
groups and city boundary as well as wrangle the data into a list of
individual origins and destinations for nightlife-related businesses.

``` r
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
  filter(corridor_dest == "East Girard" | 
           corridor_dest == 'Navy Yard' | 
           corridor_dest == '5th and Olney' | 
           corridor_dest == 'East Passyunk' |
           corridor_dest == '36th Street and vicinity' |
           corridor_dest == 'Market West') %>% 
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
  facet_wrap(~corridor_dest, ncol = 2) +
  labs(title = "Trips")
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/flows data wrangling-1.png" width="100%" />

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

Figure X.X shows the different types of commercial corridors based on
classifcation by Philadelphia. \[INCLUDE DESCRIPTION OF COMMERCIAL
CORRIDORS\]

``` r
###Corridors
phl_boundary <- 
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728')

phl_corridors %>%
  ggplot() +
  geom_sf(data = phl_boundary, fill = "grey60", color = "transparent") +
  geom_sf(aes(fill = corr_type), color = "transparent") +
  scale_fill_viridis_d() +
  labs(title = "Philadelphia Corridor Typologies",
       subtitle = "Figure X.X") +
  mapTheme() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-3-1.png" width="100%" />

The below Figure maps the commercial corridors and shows the
corridor-wide vacancy rate, which varies from close to 0% to upwards of
60% throughout the city. Smaller corridors in far West Philadelphia,
North Philadelphia tend to have the highest vacancy rates.

``` r
#Load Philadelphia commercial corridors dataset and transform
corridors <- phl_corridors

dat_restaurants <-
dat_hour %>%
  filter(top_category == "Restaurants and Other Eating Places") 

corridors_filter <- corridors %>%
  select(OBJECTID, NAME, GLA, P_DIST, ST_EXT, PT_ADD, VAC_RATE) %>%
  mutate(VAC_RATE = str_remove_all(VAC_RATE, pattern = "%")) %>%
  mutate(VAC_RATE = as.numeric(VAC_RATE)) %>%
  st_as_sf() %>%
  st_transform('ESRI:102271') 

# Plot commercial corridors with colors as vacancy rate
ggplot() +
  geom_sf(data = phillyBoundary, fill = "black") +
  geom_sf(data = corridors_filter, aes(fill = VAC_RATE), color = "transparent") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Philadelphia Commercial Corridors") + 
  mapTheme()
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-4-1.png" width="100%" />

Figure X.X below breaks down the count of restaunts in a given corridor
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
  st_transform('ESRI:102271') %>% 
    distinct()

#Merge restaurants with commercial corridors 
corridors_restaurants <- 
  dplyr::select(dat_restaurants_grid) %>% 
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
       subtitle = "Figure X.X") +
  mapTheme()
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-5-1.png" width="100%" />

Below is an animation showing restaurant popularity by corridor.

``` r
#Create animated map of number of restaurant trips by corridor per hour
dat_restaurants_cord <- dat_restaurants %>%
  st_as_sf() %>%
  st_transform('ESRI:102271') 

dat_corridors_restaurants <-
  st_join(corridors_filter, dat_restaurants_cord, ) %>% 
  group_by(NAME, GLA, Hour) %>%
  summarize(
            Avg_Popularity = mean(Count),
            Total_Visits = sum(Count),
            Med_Dwell_Time = mean(median_dwell)) %>%
  st_as_sf() 

dat_corridors_restaurants <- dat_corridors_restaurants %>%
  mutate(
    GLA = as.numeric(gsub(",", "", GLA)),
    Visits_Per_Area = Total_Visits / GLA * 5)

#Plot of restaurant popularity between 7 and 8pm
# dat_corridors_restaurants %>%
#   subset(Hour == 19) %>% #selecting for nighttime hours
#   ggplot() + 
#   geom_sf(data = phl_cbg, fill = "#440255", color = "transparent") +
#   geom_sf(aes(fill = Visits_Per_Area), color = "transparent") + 
#   scale_fill_viridis(trans = "sqrt") +
#   labs(title = "Total Restaurant Visits by Commercial Corridor") 


#Animation of restaurant popularity by hour
restaurant.corr.animation.data <-
    dat_corridors_restaurants %>%
    st_sf() %>%
    mutate(Pop_String = case_when(Visits_Per_Area < .4 ~ "5",
                              Visits_Per_Area >= .4 & Visits_Per_Area <.8 ~ "4",
                              Visits_Per_Area >= .8 & Visits_Per_Area <1.2 ~ "3",
                              Visits_Per_Area >= 1.2 & Visits_Per_Area <1.6 ~ "2",
                              Visits_Per_Area >= 2 ~ "1")) %>%
    mutate(Pop_String  = fct_relevel(Pop_String, "5","4","3","2","1"))

restaurant_corr_animation <-
  ggplot() +
  geom_sf(data = phl_cbg, fill = "#440255", color = "transparent") +   
  geom_sf(data = restaurant.corr.animation.data, aes(fill = Visits_Per_Area), color = "transparent") +
     scale_fill_viridis(trans = "sqrt") +
    labs(title = "Restaurant Popularity by Hour",
         subtitle = "One Hour Intervals: {current_frame}") +
  theme(panel.background = element_rect(fill = "black"),
         panel.grid.major = element_line(color = "transparent"),
          panel.grid.minor = element_line(colour = "transparent")) +
    transition_manual(Hour)

animate(restaurant_corr_animation, duration=20, renderer = gifski_renderer())
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-6-1.gif" width="100%" />

Figure X.X shows the number of bars in each commercial corridor per
square mile. Again, we see the corridors around Center City generally
showing a higher concentration of bars.

``` r
###Bars
dat_bars_grid <- dat_bars %>%
  select(geometry) %>%
  #na.omit() %>%
  st_as_sf() %>%
  st_transform('ESRI:102271') %>%
    distinct()

###Bars
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
       subtitle = "Figure X.X") +
  mapTheme()
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/Bars by corridor-1.png" width="100%" />

Below is an animation showing bar popularity aggregated by commercial
corridor.

``` r
#Create animated map of number of bar trips by corridor per hour
dat_bars_cord <- dat_bars %>%
  st_as_sf() %>%
  st_transform('ESRI:102271') 

dat_corridors_bars <-
  st_join(corridors_filter, dat_bars_cord, ) %>% 
  group_by(NAME, GLA, Hour) %>%
  summarize(Avg_Popularity = mean(Count),
            Total_Visits = sum(Count),
            Med_Dwell_Time = mean(median_dwell)) %>%
  st_as_sf() 

dat_corridors_bars <- dat_corridors_bars %>%
  mutate(
    GLA = as.numeric(gsub(",", "", GLA)),
    Visits_Per_Area = Total_Visits / GLA * 5)

#Plot of bar popularity between 7 and 8pm
# dat_corridors_bars %>%
#   subset(Hour == 19) %>% #selecting for nighttime hours
#   ggplot() + 
#   geom_sf(data = phl_cbg, fill = "#440255", color = "transparent") +
#   geom_sf(aes(fill = Visits_Per_Area), color = "transparent") + 
#   scale_fill_viridis(trans = "sqrt") +
#   labs(title = "Total Bar Visits by Commercial Corridor") 

#Animation of bar popularity by hour
bar.corr.animation.data <-
    dat_corridors_bars %>%
    st_sf() %>%
    mutate(Pop_String = case_when(Visits_Per_Area < .4 ~ "5",
                              Visits_Per_Area >= .4 & Visits_Per_Area <.8 ~ "4",
                              Visits_Per_Area >= .8 & Visits_Per_Area <1.2 ~ "3",
                              Visits_Per_Area >= 1.2 & Visits_Per_Area <1.6 ~ "2",
                              Visits_Per_Area >= 2 ~ "1")) %>%
    mutate(Pop_String  = fct_relevel(Pop_String, "5","4","3","2","1"))

bar_corr_animation <-
  ggplot() +
  geom_sf(data = phl_cbg, fill = "#440255", color = "transparent") +   
  geom_sf(data = bar.corr.animation.data, aes(fill = Visits_Per_Area), color = "transparent") +
     scale_fill_viridis(trans = "sqrt") +
    labs(title = "Bar Popularity by Hour",
         subtitle = "One Hour Intervals: {current_frame}") +
  theme(panel.background = element_rect(fill = "black"),
         panel.grid.major = element_line(color = "transparent"),
          panel.grid.minor = element_line(colour = "transparent")) +
    transition_manual(Hour)

animate(bar_corr_animation, duration=20, renderer = gifski_renderer())
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/bars animation-1.gif" width="100%" />

Next, we look at SafeGraph variables in the context of commercial
corridor typology. The following code wrangles the data to split it out
into time segments. This will help us better understand which corridor
types are most associated with nightlife.

``` r
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
```

    ## [38;5;246m# A tibble: 4,288,224 x 5[39m
    ##    safegraph_place_id     date_range_start                  geometry  Hour Count
    ##    [3m[38;5;246m<chr>[39m[23m                  [3m[38;5;246m<chr>[39m[23m             [3m[38;5;246m<POINT [US_survey_foot]>[39m[23m [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<dbl>[39m[23m
    ## [38;5;250m 1[39m sg:1343a6b2f36a4d8bbb~ 2018-01-01T05:00~      (2699978 -63512.19)     0     8
    ## [38;5;250m 2[39m sg:1343a6b2f36a4d8bbb~ 2018-01-01T05:00~      (2699978 -63512.19)     1     3
    ## [38;5;250m 3[39m sg:1343a6b2f36a4d8bbb~ 2018-01-01T05:00~      (2699978 -63512.19)     2     2
    ## [38;5;250m 4[39m sg:1343a6b2f36a4d8bbb~ 2018-01-01T05:00~      (2699978 -63512.19)     3     1
    ## [38;5;250m 5[39m sg:1343a6b2f36a4d8bbb~ 2018-01-01T05:00~      (2699978 -63512.19)     4     0
    ## [38;5;250m 6[39m sg:1343a6b2f36a4d8bbb~ 2018-01-01T05:00~      (2699978 -63512.19)     5     0
    ## [38;5;250m 7[39m sg:1343a6b2f36a4d8bbb~ 2018-01-01T05:00~      (2699978 -63512.19)     6     0
    ## [38;5;250m 8[39m sg:1343a6b2f36a4d8bbb~ 2018-01-01T05:00~      (2699978 -63512.19)     7     0
    ## [38;5;250m 9[39m sg:1343a6b2f36a4d8bbb~ 2018-01-01T05:00~      (2699978 -63512.19)     8     2
    ## [38;5;250m10[39m sg:1343a6b2f36a4d8bbb~ 2018-01-01T05:00~      (2699978 -63512.19)     9     9
    ## [38;5;246m# ... with 4,288,214 more rows[39m

``` r
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

#Join new dataset
dat3 <- 
  dat2 %>% 
  left_join(dat_1_6, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_7_12, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_13_18, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_19_0, by = c('safegraph_place_id', 'date_range_start')) %>%
  left_join(dat_workday, by = c('safegraph_place_id', 'date_range_start')) %>%
  st_join(phl_corridors, join = st_intersects) %>%
  st_join(phl_nhoods, join = st_intersects) %>%
  mutate(WorkDay_Evening_Ratio =  Hrs_workday / Hrs19_0)
```

Figure X.X below shows the total daily traffic volume by time segment.
While there is not drastic differentiation by corridor, this chart
indicates that Regional and Superregional corridors have a larger
relative share of daytime traffic. This is likely related to these
corridors being job hubs in Phialdelphia.

``` r
dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  group_by(corr_type) %>%
  summarise(Early_AM = sum(Hrs1_6),
            Late_AM = sum(Hrs7_12),
            Early_PM = sum(Hrs13_18),
            Late_PM = sum(Hrs19_0)) %>%
  pivot_longer(cols = 2:5,
               names_to = "Time",
               values_to = "Traffic") %>%
  ggplot(aes(fill=factor(Time, levels=c("Late_PM", 
                                        "Early_PM", 
                                        "Late_AM", 
                                        "Early_AM")), 
             y=Traffic, 
             x=factor(corr_type, levels=c("6. Speciality Center",
                                          "5. Superregional Center",
                                          "4. Regional Center",
                                          "3. Community Center",
                                          "2. Neighborhood Center",
                                          "1. Neighborhood Subcenter")))) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Total Daily Traffic Volume by Corridor Type",
       subtitle = "Figure X.X") +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/popularity by day barplot-1.png" width="100%" />

Figure X.X calculates the ratio between daytime traffic between 9AM and
6PM and evening traffic between 7PM and 12AM. The higher the ratio, the
more daytime a corridor type is. Similar to the above Figure, This
indicates that Regional and Superregional traffic sees relatively more
traffic during work hours.

``` r
#Workday to Evening Traffic Ratio by Corridor Type
dat3 %>%
  st_drop_geometry() %>%
  drop_na(corr_type) %>%
  filter_at(vars(WorkDay_Evening_Ratio), all_vars(!is.infinite(.))) %>%
  group_by(corr_type) %>%
  summarise(WorkDay_Evening_Ratio = mean(WorkDay_Evening_Ratio, na.rm = TRUE)) %>%
  ggplot(aes(y=WorkDay_Evening_Ratio, 
             x=corr_type)) + 
  geom_bar(stat="identity") +
  scale_fill_viridis_d() +
  labs(title = "Average Workday to Evening Traffic Ratio by Corridor Type",
       subtitle = "Figure X.X") +
  guides(fill=guide_legend(title=NULL)) +
  plotTheme()
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-8-1.png" width="100%" />

## Visit duration to bars and restaurants in Philadelphia

The following Figures look at the median dwell time by bars and
restaurants per commercial corridor.

``` r
#Restaurant data
dat_corridors_restaurants <-
  st_join(corridors_filter, dat_restaurants_cord, ) %>% 
  group_by(NAME, GLA, Hour) %>%
  summarize(
            Avg_Popularity = mean(Count),
            Total_Visits = sum(Count),
            Med_Dwell_Time = mean(median_dwell)) %>%
  st_as_sf() 

dat_corridors_restaurants <- dat_corridors_restaurants %>%
  mutate(
    GLA = as.numeric(gsub(",", "", GLA)),
    Visits_Per_Area = Total_Visits / GLA * 5)

#Plot of restaurant median dwell time 
dat_corridors_restaurants %>%
  subset(Hour == 19) %>% #selecting for nighttime hours
  ggplot() + 
  geom_sf(data = phl_cbg, fill = "light gray", color = "white") +
  geom_sf(aes(fill = Med_Dwell_Time), color = "transparent") + 
  scale_fill_viridis(trans = "sqrt") +
  labs(title = "Median Restaurant Dwell Time by Commercial Corridor",
       subtitle = "Figure X.X") +
  mapTheme()
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-9-1.png" width="100%" />

``` r
#Plot of bar median dwell time 
dat_corridors_bars %>%
  subset(Hour == 19) %>% #selecting for nighttime hours
  ggplot() + 
  geom_sf(data = phl_cbg, fill = "light gray", color = "white") +
  geom_sf(aes(fill = Med_Dwell_Time), color = "transparent") + 
  scale_fill_viridis(trans = "sqrt") +
  labs(title = "Median Bar Dwell Time by Commercial Corridor",
       subtitle = "Figure X.X") +
  mapTheme()
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-9-2.png" width="100%" />

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

Figures X.Xa and X.Xb show the volume of visitors to the East Girard
Corridor (outlined in red) by census block group. While visitors from
across the city frequent East Girard corridor, particularly for its
restaurants, Figure X.Xb clarifies that the corridor mostly caters to a
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
       subtitle = "Figure X.Xa") +
  facet_wrap(~top_category)
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-10-1.png" width="100%" />

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
       subtitle = "Figure X.Xb") +
  facet_wrap(~top_category)
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-10-2.png" width="100%" />

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
       subtitle = "Figure X.Xa") +
  facet_wrap(~top_category)
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/West Girard viz-1.png" width="100%" />

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
       subtitle = "Figure X.Xb") +
  facet_wrap(~top_category)
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/West Girard viz-2.png" width="100%" />

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

    ##   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  48%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |================================================                      |  68%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%

The below Figure X.X compares demographic indicators pulled from the
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
       subtitle = "Figure X.X")
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/corridor comparison-1.png" width="100%" />

# 4\. Prediction

Our model will predict the number of trips to commercial corridors given
features of the corridor such as retail mix, amenities, location, etc.
Ultimately, we hope this will be a scenario-based tool that allows users
to adjust the business mix for a given corriodr as a way to understand
how different economic development strategies will impact popularity.

While the model will predict the number of trips, the ultimate output
for the user will be relative popularity of the corridor compared to the
average. This is becasue the data is inherently noisy and we cannot be
confident that a raw trip count is accurate.

This first code block loads in data that we will use for the predictive
model.

``` r
library(FNN)
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


#Corridor shapefile
phl_corridors_pred <- st_read("http://data.phl.opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson", quiet = TRUE)%>%
  st_transform('ESRI:102728') %>%
  select(4:5,18, 'Shape__Area') %>%
  rename(., corridor = NAME,
         district = P_DIST,
         vacancy = VAC_RATE,
         area = Shape__Area) %>%
  mutate()

#Spatial Features from SafeGraph Data
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

st_c <- st_coordinates

dat_corr <-
  dat2 %>% 
  st_join(phl_corridors_pred, join = st_intersects) %>%
  # st_join(phl_nhoods_pred, join = st_intersects) %>%
  drop_na(corridor)
```

Next, we generate features for our model. These features include counts
of business types and tags (such as “late night”), nearest neighbor
variables, and distance to specific locations.

``` r
dat_corr <-
  dat_corr %>%
  mutate(total = 1,
         bars = ifelse(top_category == 'Drinking Places (Alcoholic Beverages)', 1, 0),
         restaurant = ifelse(top_category == 'Restaurants and Other Eating Places', 1, 0),
         arts = ifelse(top_category == 'Promoters of Performing Arts, Sports, and Similar Events' |
                         top_category == 'Performing Arts Companies', 1, 0),
         restaurant = ifelse(top_category == 'Restaurants and Other Eating Places', 1, 0),
         jrcol = ifelse(top_category == 'Junior Colleges', 1, 0),
         college = ifelse(top_category == 'Colleges, Universities, and Professional Schools', 1, 0),
         sports = ifelse(top_category == 'Spectator Sports', 1, 0),
         museum = ifelse(top_category == 'Museums, Historical Sites, and Similar Institutions', 1, 0),
         hotels = ifelse(top_category == 'Traveler Accommodation', 1, 0),
         bars_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(bar.sf)), 4),
         rest_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(restaurant.sf)), 4),
         arts_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(arts.sf)), 4),
         college_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(college.sf)), 1),
         sports_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(sports.sf)), 1),
         hotels_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(hotels.sf)), 4),
         casinos_nn = nn_function(st_c(st_centroid(dat_corr)), st_c(st_centroid(casinos.sf)), 1),
         late_night = if_else(grepl("Late Night", dat_corr$category_tags), 1, 0),
         bar_pub = if_else(grepl("Bar or Pub", dat_corr$category_tags), 1, 0))
```

Next we wrangle the data to aggregate it at the corridor level. To
start, we are looking at aggregated SafeGraph variables, such as median
dwell time and average distance from home.

We also review the share of certain business types and characteristics
along a specific corridor. These variables count the SafeGraph points of
interest that fit into given category (bars, restaurants, college) and
divide it by hte total number of businesses along the corridor. We also
divide by 12 since the data is disaggregated by month.

Finally, this code generates nearest neighbor variables for business by
corridor.

``` r
corr_test <- 
  dat_corr %>%
  group_by(corridor, area) %>%
  summarize(visits = sum(raw_visit_counts),
            total = sum(total),
            area = mean(area),
            sg_visitors = sum(raw_visitor_counts),
            sg_dwell = mean(median_dwell, na.rm = TRUE),
            sg_distance_home = mean(distance_from_home, na.rm = TRUE),
            share_bars = sum(bars, na.rm = TRUE)/12/area,
            share_rest = sum(restaurant, na.rm = TRUE)/12/area,
            share_arts = sum(arts, na.rm = TRUE)/12/area,
            share_jrcol = sum(jrcol, na.rm = TRUE)/12/area,
            share_college = sum(college, na.rm = TRUE)/12/area,
            share_sports = sum(sports, na.rm = TRUE)/12/area,
            share_museums = sum(museum, na.rm = TRUE)/12/area,
            nn_bars = mean(bars_nn),
            nn_rest = mean(rest_nn),
            nn_arts = mean(arts_nn),
            nn_college = mean(college_nn),
            nn_sports = mean(sports_nn),
            nn_casinos = mean(casinos_nn),
            nn_hotels = mean(hotels_nn),
            share_late_tag = sum(late_night, na.rm = TRUE)/12/area,
            share_barpub_tag = sum(bar_pub, na.rm = TRUE)/12/area)
```

The following correlation plots summarize our findings. Looking first at
the SafeGraph summary variables, the distnace from home has a strong
relationship with total visits. The count of visitors also demonstrates
a strong relationship, but this is likely collinear with the the number
of visits.

``` r
#Safegraph summary variables
corr_test %>% 
  st_drop_geometry() %>%
  pivot_longer(., cols = starts_with("sg_"), names_to = "variable", values_to="value") %>%
  ggplot(aes(value, visits)) +
  geom_point(size = .5) + 
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~variable, ncol = 3, scales = "free") +
  labs(title = "Visits as a function of SafeGraph SUmmary Variables",
       subtitle = "Figure X.X") +
  plotTheme()
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/corr plots-1.png" width="100%" />

The next set of plots show the relationship between the share a specific
business type by corridor and the number of trips. There seems to be a
relationship with businesses that could be classified as anchor
institutions, such as colleges and museums, and the number of trips.
These corrplots also look at businesses with a specific tag, such as a
“late night” or “bar and pub”. These corrplots rely on the businesses
recorded in the SafeGraph dataset and represent a sample. Do we want to
use another, more comprehensive dataset instead (e.g. OSM)?

``` r
#Count variables
corr_test %>% 
  st_drop_geometry() %>%
  pivot_longer(., cols = starts_with("share_"), names_to = "variable", values_to="value") %>%
  ggplot(aes(value, visits)) +
  geom_point(size = .5) + 
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~variable, ncol = 3, scales = "free") +
  labs(title = "Visits as a function of Continuous Count Variables",
       subtitle = "Figure X.X") +
  plotTheme()
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-11-1.png" width="100%" />

Finally, we’ve generated nearest neighbor variables for the same
businesses. So far, these are not demonstrating a particualrly strong
relationship with foot traffic.

``` r
#Nearest neighbor variables
corr_test %>% 
  st_drop_geometry() %>%
  pivot_longer(., cols = starts_with("nn_"), names_to = "variable", values_to="value") %>%
  ggplot(aes(value, visits)) +
  geom_point(size = .5) + 
  geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~variable, ncol = 3, scales = "free") +
  labs(title = "Visits as a function of Nearest Neighbor Variables",
       subtitle = "Figure X.X") +
  plotTheme()
```

<img src="MUSAPracticum_Nighttime_20210323_files/figure-gfm/unnamed-chunk-12-1.png" width="100%" />

# 5\. Next Steps

For next week, we will primarily focus on preparing our presentation by
doing the following:

  - Feature Engineering\!
