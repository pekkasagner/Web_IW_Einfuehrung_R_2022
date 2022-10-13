library(tidyverse)
library(sf)
library(readxl)


#1
(alq <- read_csv("data/Arbeitslosenquote_Bundeslaender_clean.csv"))


(bundeslaender <- read_sf("data/Bundesland_clean/Bundesland_shape_clean.shp"))


bundeslaender %>% 
          left_join(alq, by = c("ARS" = "Kennziffer")) %>% 
          ggplot(aes(fill = Arbeitslosenquote)) +
          geom_sf() +
          theme_void() +
          scale_fill_viridis_c() 


alq %>% 
          ggplot(aes(x = fct_reorder(Raumeinheit, Arbeitslosenquote, .desc = T) , y = Arbeitslosenquote)) +
          geom_col() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))


#2
(geo_corona <- read_sf("data/Geometrie_Corona_Landkreise.geojson"))

(daten_corona <- read_excel("data/Fallzahlen_Inzidenz_aktualisiert.xlsx",
                            sheet = "LK_7-Tage-Inzidenz-aktualisiert",
                            skip = 2))


(daten_corona_clean <- daten_corona %>% 
                    select(1:2, last_col()) %>% 
                    rename(inzidenz =  `16.12.2021`)
)

#Daten joinen:
(corona_joined <- geo_corona %>% 
                    left_join(daten_corona_clean, by = c("RS" = "IdMeldeLandkreis")))
          

#Visualisierung
corona_joined %>% 
          mutate(inzidenz_gruppen = cut_number(inzidenz, 10)) %>% 
          add_count(inzidenz_gruppen) %>% 
          mutate(label = paste(inzidenz_gruppen, "-", n, "Regionen", sep = " ")) %>% 
          mutate(label = fct_reorder(label, as.numeric(inzidenz_gruppen))) %>% 
          ggplot(aes(fill = label)) +
          geom_sf() +
          scale_fill_viridis_d(direction = -1, option = "magma") +
          theme_void() +
          labs(fill = "Hallo")
