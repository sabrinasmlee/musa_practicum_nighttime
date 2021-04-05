```{r feature engineering for opening hours}

#Unnest the opening hour column
open_hours <- 
  dat2 %>%
  select(open_hours)%>%
  mutate(open_hours = str_remove_all(open_hours, pattern = "\\{\\}\\:\\[\\]\\,"))%>%
  unnest(open_hours)%>%
  separate(.,
           open_hours,
           c("1", "2", "3","4", "5", "6", 
             "7", "8", "9", "10", "11", "12", 
             "13", "14"),
           sep = ",")%>%
  pivot_longer(cols = 1:15, 
               names_to = "Opening_hours",
               values_to = "Hours")%>%
  mutate(Hours = as.numeric(Hours),
         Opening_hours = as.numeric(Opening_hours))%>%
  mutate(Open_late = ifelse(Opening_hours > 1800, "Open late", "Not open late"))

dat2 <- st_join(dat2 %>% st_transform(crs=4326),
                open_hours %>%
                  st_transform(crs=4326),
                join=st_intersects, 
                left = TRUE)

```