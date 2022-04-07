#Übung 6 - Geodaten
#Autor: Pekka Sagner
#Zuletzt bearbeitet: 17.12.2021
#Header
library(tidyverse)
library(sf)
library(readxl)

#Daten einlesen
#Geodaten
(bundeslaender <- read_sf("data/Bundesland_clean/Bundesland_shape_clean.shp")
)

#Arbeitslosenquote einlesen
(alq <- read_csv("data/Arbeitslosenquote_Bundeslaender_clean.csv")
)

#Daten joinen 
(bundeslaender_alq_joined <- bundeslaender %>% 
                    left_join(alq, by = c("AGS" = "Kennziffer", "GEN" = "Raumeinheit"))
)

#Geodaten und Attribute visualisieren
bundeslaender_alq_joined %>% 
          ggplot(aes(geometry = geometry, fill = Arbeitslosenquote)) +
          geom_sf()

#Gruppierung und dann Visualisierungs
bundeslaender_alq_joined %>% 
          mutate(gruppe = cut_interval(Arbeitslosenquote, n = 4)) %>% 
          ggplot(aes(fill = gruppe)) +
          geom_sf()

#case_When
bundeslaender_alq_joined %>% 
          mutate(gruppe = case_when(Arbeitslosenquote < 4 ~ "kleiner als 4 Prozent",
                                    between(Arbeitslosenquote, 4, 7) ~ "4 bis 7 Prozent",
                                    Arbeitslosenquote > 7 ~ "größer als 7 Prozent")) %>% 
          mutate(gruppe = factor(gruppe, levels = c("kleiner als 4 Prozent",
                                                    "4 bis 7 Prozent",
                                                    "größer als 7 Prozent"))) %>% 
          ggplot(aes(fill = gruppe)) +
          geom_sf() +
          scale_fill_viridis_d()

#Aufgabenteil 2 - Visualisierung Coronafallzahlen
#Geodaten
(geo_corona <- read_sf("data/Geometrie_Corona_Landkreise.geojson")
)
#Coronafallzahlen
(daten_corona <- read_excel("data/Fallzahlen_Inzidenz_aktualisiert.xlsx", 
                            sheet = "LK_7-Tage-Inzidenz-aktualisiert",
                            skip = 2) %>% 
                    select("IdMeldeLandkreis", "MeldeLandkreis", "17.12.2021") %>% 
                    rename(Inzidenz = "17.12.2021")
)

#Daten joinen
(corona_joined <- geo_corona %>% 
                    left_join(daten_corona, by = c("RS" = "IdMeldeLandkreis"))
)


#Visualisierung
corona_joined %>% 
          ggplot(aes(geometry = geometry, fill = Inzidenz)) +
          geom_sf() +
          scale_fill_viridis_c(direction = -1)

#Gruppieren
corona_joined %>% 
          mutate(gruppe = cut_number(Inzidenz, n = 10)) %>% 
          ggplot(aes(fill = gruppe)) +
          geom_sf() +
          scale_fill_viridis_d(option = "plasma", direction = -1)
