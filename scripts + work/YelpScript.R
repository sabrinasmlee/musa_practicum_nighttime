#Yelp API example

library(tidyverse)
library(httr)
library(sf)
library(ggplot2)

client_id <- "K6YOIF1eZkNbvCcy43X-Sg"
client_secret <- "QnkL-GQLn9AdJvcd7GUNv3yCjhMuxjXTuZzez0lrhVJHIxNrQu24fMxrXdR4eJFiSeJhtGnrIiLrCJf4jE7aUNiZ5XEkqIpjYDUzQzyVNB-k7HiG9buOXZpKVT4gYHYx"

res <- POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = "client_credentials",
                        client_id = client_id,
                        client_secret = client_secret))

token <- content(res)$access_token

yelp <- "https://api.yelp.com"
term <- "cookies"
location <- "Philadelphia, PA"
categories <- "restaurant"
limit <- 50
radius <- 8800
url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                  query = list(term = term, location = location, 
                               limit = limit,
                               radius = radius))
res <- GET(url, add_headers('Authorization' = paste("bearer", client_secret)))

results <- cont==ent(res)

yelp_httr_parse <- function(x) {
  
  parse_list <- list(id = x$id, 
                     name = x$name, 
                     rating = x$rating, 
                     review_count = x$review_count, 
                     latitude = x$coordinates$latitude, 
                     longitude = x$coordinates$longitude, 
                     address1 = x$location$address1, 
                     city = x$location$city, 
                     state = x$location$state, 
                     distance = x$distance)
  
  parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
  
  df <- data_frame(id=parse_list$id,
                   name=parse_list$name, 
                   rating = parse_list$rating, 
                   review_count = parse_list$review_count, 
                   latitude=parse_list$latitude, 
                   longitude = parse_list$longitude, 
                   address1 = parse_list$address1, 
                   city = parse_list$city, 
                   state = parse_list$state, 
                   distance= parse_list$distance)
  df
}

results_list <- lapply(results$businesses, FUN = yelp_httr_parse)

business_data <- do.call("rbind", results_list)

business_data

#Mapping Yelp Data
phl <- st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson")

business_data_map <- 
  st_as_sf(business_data, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform('ESRI:102728')

ggplot() + 
  geom_sf(data = phl) +
  geom_sf(data = business_data_map, aes(color = rating))
