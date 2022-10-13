
library(tidyverse)

#Neues Paket (schon mit tidyverse installiert):
library(readxl)
#Neues Paket (muss installiert werden):
library(janitor)

#b)
(daten <- read_excel("data/Einkommen_Kreise.xlsx")
)


#c)
(daten_clean <- read_excel("data/Einkommen_Kreise.xlsx", 
           sheet = "Daten",
           skip = 1) %>% 
          janitor::clean_names() %>% 
          pivot_longer(cols = c(x2000:x2017), 
                       names_to = "jahr",
                       values_to = "haushaltseinkommen",
                       names_prefix = "x") %>% 
          rename(nr = x1,
                 name = x2,
                 kreistyp = x3)
)


#d)
daten_clean %>% 
          mutate(jahr = as.numeric(jahr)) %>% 
          mutate(name = str_remove(name, ", Stadt")) %>% 
          filter(name %in% c("Bonn", "Düsseldorf", "Köln", "Rhein-Sieg-Kreis")) %>% 
          ggplot(aes(x = jahr, y = haushaltseinkommen, col = name)) +
          geom_point() +
          geom_line() +
          labs(col = "Kreis",
               x = "",
               y = "Haushaltseinkommen in Euro",
               title = "Entwicklung der Haushaltseinkommen",
               subtitle = "In den Jahren 2000 bis 2017", 
               caption= "Quelle: Inkar-Datenbank, BBSR (2020)") +
          theme_minimal() +
          scale_color_viridis_d()

ggsave("test.png", dpi = 600)




#e) Relation der Einkommen

(relation_reinkommen <- daten_clean %>% 
          mutate(jahr = as.numeric(jahr)) %>% 
          mutate(name = str_remove(name, ", Stadt")) %>% 
          select(jahr, name, haushaltseinkommen) %>% 
          filter(name %in% c("Bonn", "Düsseldorf", "Köln", "Rhein-Sieg-Kreis")) %>% 
          pivot_wider(names_from = name,
                      values_from = haushaltseinkommen) %>% 
          mutate(relation_Bonn = Bonn/Düsseldorf,
                 relation_Köln = Köln/Düsseldorf,
                 `relation_Rhein-Sieg-Kreis` = `Rhein-Sieg-Kreis`/Düsseldorf)
)          
          

#f) Grafisch Darstellung der Einkommensrelation
relation_reinkommen %>% 
          select(jahr, starts_with("relation_")) %>% 
          pivot_longer(cols = -jahr,
                       names_to = "name", 
                       values_to = "relation", 
                       names_prefix = "relation_") %>% 
          ggplot(aes(x = jahr, y = relation, col = name)) +
          geom_line() +
          geom_point() +
          labs(col = "Kreis",
               x = "",
               y = "Relation der Haushaltseinkommen",
               title = "Relation der Haushaltseinkommen",
               subtitle = "zur Landshauptstadt Düsseldorf, in Prozent, in den Jahren 2000 bis 2017", 
               caption= "Quelle: Inkar-Datenbank, BBSR (2020)") +
          theme_minimal() +
          scale_color_viridis_d() +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1))
