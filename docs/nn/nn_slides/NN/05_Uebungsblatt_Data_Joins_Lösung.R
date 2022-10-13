library(tidyverse)

#b)
#Einlesen und Überblick
(studis_insg_2017 <- read_csv("data/studis_insg_2017.csv")
)

(studis_fh_2017 <- read_csv("data/studis_fh_2017.csv")
)

       
studis_insg_2017 %>% 
          ggplot(aes(studierende_insg)) +
          geom_boxplot()

studis_insg_2017 %>% 
          ggplot(aes(studierende_insg)) +
          geom_histogram()

studis_fh_2017 %>% 
          ggplot(aes(studierende_fh)) +
          geom_boxplot()

studis_fh_2017 %>% 
          ggplot(aes(studierende_fh)) +
          geom_histogram()

#Min, max, mean, median


#c) Daten joinen
(studis_joined_2017 <- studis_insg_2017 %>% 
          left_join(studis_fh_2017, by = c("nr", "name")) 
)         

# Kreise mit dem höchsten Anteil an FH-Studis
studis_joined_2017 %>% 
          mutate(anteil_fh_studis = studierende_fh / studierende_insg) %>% 
          filter(anteil_fh_studis == max(anteil_fh_studis, na.rm = T))


studis_joined_2017 %>% 
          mutate(anteil_fh_studis = studierende_fh / studierende_insg) %>% 
          filter(anteil_fh_studis == min(anteil_fh_studis, na.rm = T))


#d)
#Einlesen und Überblick
(studis_insg <- read_csv("data/studis_insg.csv")
)

(studis_fh <- read_csv("data/studis_fh.csv")
)


studis_insg %>% 
          ggplot(aes(x = jahr, y = studierende_insg, group = jahr)) +
          geom_boxplot()


studis_insg %>% 
          ggplot(aes(studierende_insg)) +
          geom_histogram() +
          facet_wrap(~jahr)

#e)
(studis_joined <- studis_insg %>% 
                    left_join(studis_fh, by = c("nr", "name", "jahr")) 
)         

#Anteil FH Studis an allen Studis im Rhein-Sieg-Kreis:
studis_joined %>% 
          mutate(anteil_fh_studis = studierende_fh / studierende_insg) %>% 
          filter(name == "Rhein-Sieg-Kreis") %>% 
          ggplot(aes(x = jahr, y = anteil_fh_studis)) +
          geom_line()

studis_joined %>% 
          mutate(anteil_fh_studis = studierende_fh / studierende_insg) %>% 
          filter(anteil_fh_studis > 1) 

#f) Zwei Wege führen zu Ziel:

#1. Weg (mit Funktionen, die wir schon kennen)
(studis_joined_long <- studis_joined %>% 
          pivot_longer(cols = c("studierende_insg", "studierende_fh"),
                       names_to = "typ",
                       values_to = "anz_studis",
                       names_prefix = "studierende_")
)

studis_joined_long %>% 
          filter(name %in% c("Berlin, Stadt", "Düsseldorf, Stadt", "Frankfurt am Main, Stadt",
                             "Hamburg, Stadt", "Köln, Stadt", "München, Stadt", "Stuttgart, Stadt")) %>% 
          ggplot(aes(x = jahr, y = anz_studis, col = name, shape = typ)) +
          geom_point() +
          geom_line()

#2. Weg
(studis_insg_bind <- studis_insg %>% 
          rename(anz_studis = studierende_insg) %>% 
          mutate(typ = "insg")
)


(studis_fh_bind <- studis_fh %>% 
          rename(anz_studis = studierende_fh) %>% 
          mutate(typ = "fh")
)

(studis_bind_long <- studis_insg_bind %>% 
                    bind_rows(studis_fh_bind)
)

studis_bind_long %>% 
          filter(name %in% c("Berlin, Stadt", "Düsseldorf, Stadt", "Frankfurt am Main, Stadt",
                             "Hamburg, Stadt", "Köln, Stadt", "München, Stadt", "Stuttgart, Stadt")) %>% 
          ggplot(aes(x = jahr, y = anz_studis, col = name, shape = typ)) +
          geom_point() +
          geom_line()
