#Dataanalysis wirtten by Christian Emonds
#Date 12.01.2022

install.packages("tidyverse")
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(purrr)

forest_wheather_data <- read.csv("forestfires.csv")

forest_wheather_data <- forest_wheather_data %>% 
  mutate(month = factor(month, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))

head(forest_wheather_data)

fireinmonths <- forest_wheather_data %>% 
  group_by(month) %>% 
  summarize(count = n ())

ggplot(fireinmonths) +
  aes(x = month, y = count) +
  geom_bar(stat = "identity") + 
  labs(title = "Monatliche Anzahl an Waldbränden (Portugal)", x = "Monat", y = "Anzahl Brände") + 
  theme(panel.background = element_rect(fill = "white"))

create_box_by_month <- function(x, y) {
  ggplot(forest_wheather_data) +
    aes_string(x, y) + 
    geom_boxplot() +
    labs(title = "Verteilung Temperatur per Monat", x = "Monat", y = "Temperatur (C)")
}

x_var <- c("month")
y_var <- c("temp")

map2(x_var, y_var, create_box_by_month)




  

