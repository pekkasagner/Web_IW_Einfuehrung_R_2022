#Übungsblatt 3
#Autor: Pekka Sagner

#b)
#install.packages("readxl")

#Libraries
library(tidyverse)
library(readxl)

#b)
#Daten einlesen
(daten_einkommen <- read_excel(path = "data/Einkommen_Kreise.xlsx")
)

#Was stimmt nicht mit den Daten?
#1) Jahre in extra Zeile, sollte so nicht sein.
#2) Daten nicht tidy, weil Jahr nicht in extra Spalte
#3) Haushaltseinkommen sollte auch eine eigene Spalte erhalten


#c)
#Beginn Daten Säubern
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


#d) wa zu tun ist: 1) Datensatz filtern auf vier Städte von Interesse. 2) Relation berechnen

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


#Daten bei Bedarf wieder in Long-Format
(daten_relation_long <- daten_relation_wide %>% 
          select(jahr, starts_with("relation_")) %>% 
          pivot_longer(cols = !"jahr",
                    # cols = starts_with("relation_"),
                       names_to = "stadt",
                       values_to = "relation_einkommen",
                       names_prefix = "relation_") 
)



