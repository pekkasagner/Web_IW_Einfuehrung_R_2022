#Übung 5 - Joins
#Autor: Pekka Sagner
#Zuletzt bearbeitet: 3.12.2021

#Header
library(tidyverse)

(studis_fh_2017 <- read_csv("data/studis_fh_2017.csv")
)

(studis_insg_2017 <- read_csv("data/studis_insg_2017.csv")
)

#Überblick 
#Visualisierung 
studis_insg_2017 %>% 
          ggplot(aes(x = name, y = studierende_insg)) +
          geom_col()

studis_insg_2017 %>% 
          ggplot(aes(studierende_insg)) +
          geom_boxplot()

studis_fh_2017 %>% 
          ggplot(aes(studierende_fh)) +
          geom_boxplot()

studis_insg_2017 %>% 
          summarise(mean_studis = mean(studierende_insg),
                    median_studis = median(studierende_insg),
                    min_studis = min(studierende_insg),
                    max_studis = max(studierende_insg))

#Ziel: Anteil FH-Studis an allen Studis bestimmen
# Wie? Mit einem Join
#Im Zweifel: Left Join

studis_insg_2017 %>% 
          left_join(studis_fh_2017, by = "nr") %>% View()

studis_insg_2017 %>% 
          left_join(studis_fh_2017, by = c("name")) %>% View()

(studis_joined_2017 <- studis_insg_2017 %>% 
          left_join(studis_fh_2017, by = c("nr", "name")) 
)

studis_joined_2017 %>% 
          mutate(anteil = studierende_fh / studierende_insg) %>% 
          filter(anteil == max(anteil, na.rm = TRUE)) %>% View()

studis_joined_2017 %>% 
          mutate(anteil = studierende_fh / studierende_insg) %>% 
          filter(anteil == min(anteil, na.rm = TRUE)) %>% View()


#c) 
#Zuerst Daten laden

(studis_insg <- read_csv("data/studis_insg.csv")
)

(studis_fh <- read_csv("data/studis_fh.csv")
)

studis_insg %>% 
          ggplot(aes(x = name, y = studierende_insg, col = factor(jahr))) +
          geom_point()

studis_insg %>% 
          ggplot(aes(studierende_insg)) +
          geom_histogram() +
          facet_wrap(~jahr)

#c) Anteil der FH-Studis an allen Studis im Rhein-Sieg-Kreis über die Zeit.
(studis_joined <- studis_insg %>% 
          left_join(studis_fh, by = c("nr", "name", "jahr"))  
)        

(studis_joined_rhein_sieg <- studis_joined %>% 
          mutate(anteil = studierende_fh / studierende_insg) %>% 
          filter(name == "Rhein-Sieg-Kreis")
)
 
studis_joined_rhein_sieg %>% 
          ggplot(aes(x = jahr, y = anteil)) +
          geom_point() +
          geom_line()
#Stimmen die Daten überhaupt?

studis_joined %>% 
          mutate(anteil = studierende_fh / studierende_insg) %>% 
          filter(anteil > 1)



#d) Ziel: Entwicklung Studis insg. und FH in Top-7 Städten über die Zeit 
#in einer Abbildung
studis_joined %>% 
          mutate(name = str_remove_all(name, pattern = ", Stadt")) %>% 
          filter(name %in% c("Berlin", "Düsseldorf", "Frankfurt am Main",
                             "Hamburg", "Köln", "München", "Stuttgart")) %>% 
          count(name)


studis_joined %>% 
          filter(name %in% c("Berlin, Stadt", "Düsseldorf, Stadt",
                             "Frankfurt am Main, Stadt", "Hamburg, Stadt",
                             "Köln, Stadt", "München, Stadt", 
                             "Stuttgart, Stadt")) %>% 
          mutate(name = str_remove_all(name, pattern = ", Stadt")) %>% 
          pivot_longer(cols = c("studierende_insg", "studierende_fh"),
                       names_to = "Studierende",
                       values_to = "Anzahl_je_Tausend") %>% 
          ggplot(aes(x = jahr, y = Anzahl_je_Tausend, col = name, shape = Studierende)) +
          geom_line() +
          geom_point()

#Alternativer Weg, Daten zu verbinden
studis_fh_trick <- studis_fh %>% 
          rename(Anzahl_je_Tausend = studierende_fh) %>% 
          mutate(Studierende = "studierende_fh")

studis_insg_trick <- studis_insg %>% 
          rename(Anzahl_je_Tausend = studierende_insg) %>% 
          mutate(Studierende = "studierende_insg")


studis_fh_trick %>% 
          bind_rows(studis_insg_trick)


