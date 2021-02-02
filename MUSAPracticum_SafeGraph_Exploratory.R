#Nightlife

########
# SETUP
########

library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(sp)
library(rgdal) 
#library(timeDate)
#install.packages("datetime")
library(datetime)
library(lubridate)

options(scipen=999)
options(tigris_class = "sf")

# ---- Load Styling options -----

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
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

# Load Quantile break functions

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

# Load hexadecimal color palette

#palette5 <- c("#3552F2", "5E7EBF", "4F6573", "#F2E85C", "#F2CA52")
palette5 <- c("#F2E85C", "#F2CA52", "5E7EBF", "4F6573", "#3552F2")



###########
# LOAD DATA
############
dat <- read.csv("moves_2018.csv")

head(dat)

#Add date column based off start date
dat <- dat %>% mutate(date = as.date(dat$date_range_start))

colnames(dat)

dat_day <- dat %>% select(safegraph_place_id, location_name, postal_code, date, popularity_by_day) %>%
  head(200) %>%
  separate(.,
           popularity_by_day,
           c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
           sep = ",")

dat_day <- dat_day %>%
  mutate(Mon_no = parse_number(Monday),
         Tue_no = parse_number(Tuesday),
         wed_no = parse_number(Wednesday),
         Thurs_no = parse_number(Thursday),
         Fri_no = parse_number(Friday),
         Sat_no = parse_number(Saturday),
         Sun_no = parse_number(Sunday)) 

dat_day_zip <- dat_day %>%
  dplyr::select(location_name, postal_code, Mon_no:Sun_no) %>%
  pivot_longer(., 
               cols = Mon_no:Sun_no, 
               names_to = "Day",
               values_to = "Count") %>%
  group_by(postal_code, Day) %>%
  summarise(sum = sum(Count)) 

dat_day_zip %>% group_by(postal_code) %>%
  ggplot() +
  geom_bar(aes(sum))

?geom_bar


ggplot(correlation.long, aes(Value, countheroin_cases)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Heroin Crime Count as a Function of Risk Factors",
       subtitle = "Figure 5.1") +
  plotTheme()

?gather
gather(Variable, Value, -countheroin_cases)
?pivot_longer





?separate
head(dat_day)

