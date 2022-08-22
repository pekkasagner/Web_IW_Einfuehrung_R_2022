#Dataanalysis wirtten by Christian Emonds
#Date 12.01.2022

install.packages("tidyverse")
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
library(sf)

forest_dataframe <- read.csv("E:\\Christian\\Uni Skripte\\Semester 5\\Dataanalysis\\Projects\\CaliforniaWildfire\\Data.csv", na="NA")
california_state <- read_sf("E:\\Christian\\Uni Skripte\\Semester 5\\Dataanalysis\\Projects\\CaliforniaWildfire\\CA_Counties\\CA_Counties_TIGER2016.shx")
cal_incidents <-read.csv("E:\\Christian\\Uni Skripte\\Semester 5\\Dataanalysis\\Projects\\CaliforniaWildfire\\mapdataall.csv")

new_forest_dataframe <- subset(forest_dataframe, YEAR_!="1878" & YEAR_!="1879" & YEAR_!="1880" & YEAR_!="1881" & YEAR_!="1882" & YEAR_!="1883" & YEAR_!="1884" & YEAR_!="1885" & YEAR_!="1886" & YEAR_!="1887" & YEAR_!="1888" & YEAR_!="1889" & YEAR_!="1890"& YEAR_!="1891" & YEAR_!="1892" & YEAR_!="1893" & YEAR_!="1894" & YEAR_!="1895" & YEAR_!="1896" & YEAR_!="1897" & YEAR_!="1898" & YEAR_!="1899" & YEAR_!="1990")

AVG_HA_per_Year <- new_forest_dataframe %>% 
  group_by(YEAR_) %>% 
  mutate(Hectar = GIS_ACRES * 0.4) %>% 
  summarise(mean_hectar = mean(Hectar))

ggplot(AVG_HA_per_Year) +
  aes(x = YEAR_, y = mean_hectar) +
  geom_line() + 
  geom_smooth() +
  theme_bw() +
  labs(title = "Durchschnittliche Größe Waldbrand (Monat)", x = "Monat", y = "Fläche (Hektar)") + 
  theme(panel.background = element_rect(fill = "white"))

#Visualisierung California State

california_state %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf()

joined_cal_states <- california_state %>%
  left_join(cal_incidents, by = c("NAME" = "incident_county"))

joined_cal_states %>% 
  ggplot(aes(geometry = geometry, fill = "is_active")) +
  geom_sf() +
  theme_void()
            

