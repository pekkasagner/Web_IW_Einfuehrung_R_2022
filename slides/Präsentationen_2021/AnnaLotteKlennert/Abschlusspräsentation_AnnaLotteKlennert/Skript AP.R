#Autorin: Anna Lotte Klennert
#Datum: 07.01.2022
#Zweck: Abschlussprüfung EF Einführung in R 


#Load Packages

library(tidyverse)
library(tidytuesdayR)
library(sf)

#Load Data: 

forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')
forest_area <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv')
brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')
soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')
vegetable_oil <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/vegetable_oil.csv')

suedamerika <- read_sf("/Users/annalotteklennert/Desktop/LatinAmerica.shx")

#Plots: 
unique(forest_area$entity)

#1
forest %>%
  filter(entity == "World") %>%
  group_by(entity, year) %>%
  mutate(forest_lost = (net_forest_conversion/1e6)) %>%
  ggplot(aes(x = year, 
             y = forest_lost,
             col = entity)) +
  geom_line()+
  geom_point() +
  labs(title = "Entwicklung der Waldfläche weltweit",
       subtitle = "in Millionen Hektar 1990 bis 2015",
       x = "",
       y = "Hektar Waldfläche", 
       col = "") +
  theme_minimal() +
  scale_color_viridis_d()

#2
forest_area %>%
  filter(entity %in% c("Africa", "Northern America", "Europe", 
                       "South America", "Asia", "Oceania")) %>%
  group_by(entity, year) %>%
  ggplot(aes(x=year,
             y=forest_area,
             col=entity)) +
  geom_point()+
  geom_line() +
  scale_color_viridis_d() +
  labs(title = "Entwicklung des Anteil an Waldfläche zwischen den Kontinenten",
       subtitle = "von 1976 bis 2016",
       x = "",
       y = "Prozentualer Anteil",
       col = "Kontinente") +
  theme_minimal()


#3
forest_sa <- forest_area %>%
  filter(entity %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Ecuador", 
                       "Guyana", "French Guyana", "Kolumbia", "Paraguay", "Peru",
                       "Suriname", "Uruguay", "Venezuela", "Colombia","Guatemala", 
                       "Mexico" ,"Belize", "Costa Rica","El Salvador", "Nicaragua", 
                       "Panama", "Honduras" )) %>%
  group_by(entity, year)

forest_sa_joined <- suedamerika %>%
  left_join(forest_sa, by = c("CNTRY_NAME" = "entity"))

forest_sa_joined  %>% 
  mutate(gruppe = cut_width(forest_area, width = 2.0)) %>%
  filter(gruppe != "NA",
         year == 2016) %>%
  ggplot (aes(geometry = geometry, 
             fill = gruppe)) + 
  geom_sf() +
  labs(title = "Anteil an Waldfläche in Lateinamerika",
       subtitle = "in Prozent",
       x = "",
       y = "",
       fill = "prozentualer Anteil") +
  theme_void() +
  scale_fill_viridis_d()


#4
forest %>%
  filter(entity == "Brazil") %>%
  mutate(forest_lost = (net_forest_conversion/1e3)) %>%
  group_by(entity, year) %>%
  ggplot(aes(x = year, 
             y = forest_lost ,
             col = entity)) +
  geom_line()+
  geom_point() +
  labs(title = "Entwicklung der Waldfläche in Brasilien",
       subtitle = "in Tausend Hektar, 1990 bis 2015",
       x = "",
       y = "Hektar Waldfläche", 
       col = "") +
  theme_minimal() +
  scale_color_viridis_d()

 
#5 
names(brazil_loss)

brazil_loss %>%
  rename(c("Nutzpflanzen" ="commercial_crops" ,             
           "Überflutung" = "flooding_due_to_dams" ,          
           "natürliche Ursachen" = "natural_disturbances" ,          
           "Weideland" = "pasture" ,                        
           "selektive Abholzung" = "selective_logging",              
           "Feuer" = "fire",                           
           "Bergbau" = "mining" ,                         
           "Infrastruktur" = "other_infrastructure",          
           "Straßen" = "roads" ,                          
           "Palmplantagen" = "tree_plantations_including_palm" ,
           "kleine Abholzungen" = "small_scale_clearing")) %>%
  pivot_longer(cols = c("Nutzpflanzen" ,          
                        "natürliche Ursachen",           
                        "Weideland",              
                        "Feuer" ,           
                        "Straßen",                          
                        "Palmplantagen",
                        "kleine Abholzungen" )) %>%
  filter(year == 2013) %>%
  group_by( name) %>%
  mutate(hoehe = (value/1e3)) %>%
  ggplot(aes(x= reorder(name, -hoehe), 
             y= hoehe , 
             fill = name))+
  geom_col() +
  labs(title = "Höhe der Abholzung nach Zweck",
       subtitle = "in Tausend Hektar, 2013",
       x = "",
       y = "Abholzung in Hektar",
       fill = "") +
  scale_fill_viridis_d()+
  theme_minimal()

#6
brazil_loss  %>% 
  rename(c("Nutzpflanzen" ="commercial_crops" ,             
           "Überflutung" = "flooding_due_to_dams" ,          
           "natürliche Ursachen" = "natural_disturbances" ,          
           "Weideland" = "pasture" ,                        
           "selektive Abholzung" = "selective_logging",              
           "Feuer" = "fire",                           
           "Bergbau" = "mining" ,                         
           "Infrastruktur" = "other_infrastructure",          
           "Straßen" = "roads" ,                          
           "Palmplantagen" = "tree_plantations_including_palm" ,
           "kleine Abholzungen" = "small_scale_clearing")) %>%
  pivot_longer(cols = c("Nutzpflanzen" ,          
                        "natürliche Ursachen",           
                        "Weideland",              
                        "Feuer" ,           
                        "Straßen",                          
                        "Palmplantagen",
                        "kleine Abholzungen" )) %>%
  filter(name == "Weideland") %>%
  group_by( year) %>%
  mutate(hoehe = (value/1e3)) %>%
  ggplot(aes(x= year, 
             y= hoehe , 
             col = name))+
  geom_line() +
  geom_point() +
  labs(title = "Entwicklung der Abholzung für Weideland",
       subtitle = "in Tausend Hektar, 2001-2013",
       x = "",
       y = "Abholzung in Hektar",
       col = "") +
  scale_color_viridis_d()+
  theme_minimal()

  

















