#Übungsblatt 4
#Autor: Pekka Sagner
#Zuletzt bearbeitet: 19.11.2021

#b)
#install.packages("readxl")

#Libraries
library(tidyverse)
library(readxl)

#Daten einlesen
(daten_einkommen <- read_excel(path = "data/Einkommen_Kreise.xlsx")
)

#Was stimmt nicht mit den Daten?
#1) Jahre in extra Zeile, sollte so nicht sein.
#2) Daten nicht tidy, weil Jahr nicht in extra Spalte
#3) Haushaltseinkommen sollte auch eine eigene Spalte erhalten

(daten_einkommen_skip <- read_excel(path = "data/Einkommen_Kreise.xlsx", skip = 1)
)
          
#neues Problem: Spaltennamen sind "merkwürdig"
#install.packages("janitor")
library(janitor)

(daten_einkommen_skip_varname <- daten_einkommen_skip %>% 
          janitor::clean_names()
)

#Klingende Variablennamen vergeben:
(daten_einkommen_skip_varname_clean <- daten_einkommen_skip_varname %>% 
          rename(kreisnummer = x1,
                 kreisname = x2,
                 kreistyp = x3)
)

#Daten von wide in long formatieren:
(daten_final <- daten_einkommen_skip_varname_clean %>% 
          pivot_longer(cols = x2000:x2017,
                       names_to = "jahr",
                       values_to = "haushaltseinkommen",
                       names_prefix = "x") %>% 
          mutate(jahr = as.numeric(jahr))
)


#d)zeitliche Entwicklung des Einkommens in Bonn, Düsseldorf, Köln, Rhein-Sieg-Kreis
daten_final %>% 
          filter(kreisname %in% c("Bonn, Stadt", "Düsseldorf, Stadt",
                                  "Köln, Stadt", "Rhein-Sieg-Kreis")) %>% 
          ggplot(aes(x = jahr, y = haushaltseinkommen, col = kreisname)) +
          geom_line() +
          geom_point() +
          labs(title = "Entwicklung der Haushaltseinkommen",
               subtitle = "von 2000 bis 2017, in Euro", 
               x = "",
               y = "Haushaltseinkommen in Euro",
               col = "Kreis bzw. Stadt") +
          scale_x_continuous(breaks = c(2000:2017)) +
          scale_color_viridis_d() +
          theme_minimal()


#e) wa zu tun ist: 1) Datensatz filtern auf vier Städte von Interesse. 2) Relation berechnen

#nicht so gut:
daten_final %>% 
          filter(kreisname %in% c("Bonn, Stadt", "Düsseldorf, Stadt",
                                  "Köln, Stadt", "Rhein-Sieg-Kreis")) %>% 
          mutate(kreisname = str_remove_all(kreisname, pattern = ", Stadt")) %>% 
          pivot_wider(names_from = c("kreisnummer", "kreisname", "kreistyp"),
                      values_from = "haushaltseinkommen") 


#besser:
(daten_relation_wide <- daten_final %>% 
          filter(kreisname %in% c("Bonn, Stadt", "Düsseldorf, Stadt",
                                  "Köln, Stadt", "Rhein-Sieg-Kreis")) %>% 
          mutate(kreisname = str_remove_all(kreisname, pattern = ", Stadt")) %>% 
          select(jahr, kreisname, haushaltseinkommen) %>% 
          pivot_wider(names_from = "kreisname",
                      values_from = "haushaltseinkommen") %>% 
          mutate(relation_Bonn = Bonn / Düsseldorf,
                 relation_Köln = Köln / Düsseldorf,
                 `relation_Rhein-Sieg-Kreis` = `Rhein-Sieg-Kreis` / Düsseldorf) 
)



#f) Visualisierung der Relation
(daten_relation_long <- daten_relation_wide %>% 
          select(jahr, starts_with("relation_")) %>% 
          pivot_longer(cols = !"jahr",
                    # cols = starts_with("relation_"),
                       names_to = "stadt",
                       values_to = "relation_einkommen",
                       names_prefix = "relation_") 
)

daten_relation_long %>% 
          ggplot(aes(x = jahr, y = relation_einkommen, col = stadt)) +
          geom_line() +
          geom_point() +
          labs(title = "",
               subtitle = "", 
               x = "",
               y = "",
               col = "Kreis bzw. Stadt") +
          scale_x_continuous(breaks = c(2000:2017)) +
          scale_color_viridis_d() +
          theme_minimal()

