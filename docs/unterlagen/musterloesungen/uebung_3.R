#Übung 3 Visualisierungen mit ggplot
#Autor: Pekka Sagner
#Zuletzt bearbeitet: 5.11.2021

#Header (Pakete laden)
library(tidyverse)

#Daten einladen
datasaurus <- read_tsv("data/datasaurus.tsv")

#b) Wie viele Gruppen? Wie viele Beobachtungen je Gruppe?
datasaurus %>% 
          count(gruppe)

#c)
datasaurus %>% 
          # filter(gruppe == "01" | gruppe == "04" | gruppe == "06" | gruppe == "10")
          filter(gruppe %in% c("01", "04", "06", "10")) %>% 
          group_by(gruppe) %>% 
          summarise(mean_x = mean(x),
                    mean_y = mean(y))

#d)
datasaurus %>% 
          filter(gruppe == "01") %>% 
          ggplot(aes(x = x, y = y)) +
          geom_point()

datasaurus %>% 
          filter(gruppe == "04") %>% 
          ggplot(aes(x = x, y = y)) +
          geom_point()

datasaurus %>% 
          filter(gruppe == "06") %>% 
          ggplot(aes(x = x, y = y)) +
          geom_point()

datasaurus %>% 
          filter(gruppe == "10") %>% 
          ggplot(aes(x = x, y = y)) +
          geom_point()


#e) #Plot für alle Gruppen
datasaurus %>% 
          ggplot(aes(x = x, y = y, col = gruppe)) +
          geom_point()

datasaurus %>% 
          ggplot(aes(x = x, y = y)) +
          geom_point() +
          facet_wrap(~gruppe)

#Aufgabe 2

#a)
#Header
library(tidyverse)
library(ggthemes)
#Daten laden
olympics <- read_csv("data/olympics.csv")
#Daten manipulieren
olympics_gold_2016 <- olympics %>% 
          filter(games == "2016 Summer") %>% 
          filter(medal == "Gold") %>% 
          mutate(bmi = weight / (height/100)^2)
#Plot erstellen
# olympics_gold_2016 %>% 
#           drop_na(weight, height) %>% 
#           ggplot(aes(x = weight, y = height)) +
#           geom_point(aes(color = bmi, shape = sex)) +
#           geom_smooth() +
#           theme_economist()

olympics_gold_2016 %>% 
          drop_na(weight, height) %>% 
          ggplot(aes(x = weight, y = height)) +
          geom_point(aes(color = bmi, shape = sex)) +
          geom_smooth(color = "gray", 
                      se = F, 
                      size = 0.2, 
                      linetype = "dotted") +
          scale_color_viridis_c() +
          labs(title = "Gewicht und Größe der GoldmedaillengewinnerInnen",
               subtitle = "Olympische Sommerspiele 2016",
               x = "Gewicht in kg",
               y = "Größe in cm",
               col = "Body-Mass-Index",
               shape = "Geschlecht",
               caption = "Quelle: TidyTuesday") +
          theme_minimal()

ggsave(filename = "plots/gewicht_groesse_gold.pdf",
       width = 8, height = 6, units = "cm")

##Stand: 5.11.


#b) Anteil männliche und weibliche MedaillengewinnerInnen
# olympics %>%
#           filter(games == "2016 Summer", !is.na(medal)) 

#Variante 1:
(variante_1 <- olympics %>%
          filter(games == "2016 Summer") %>% 
          drop_na(medal) %>% 
          group_by(sex) %>% 
          count(medal) %>% 
          mutate(total_medals = sum(n)) %>% 
          distinct(total_medals, .keep_all = T) %>% 
          ungroup() %>% 
          select(sex, total_medals) 
)     

#Visualisierung
variante_1 %>% 
          ggplot(aes(x = sex, y = total_medals)) +
          geom_col()

###Variante 2
olympics %>% 
          filter(games == "2016 Summer") %>% 
          drop_na(medal) %>% 
          count(sex) %>% 
          ggplot(aes(x = sex, y = n)) +
          geom_col()


#c)
(medal_teams <- olympics %>% 
          filter(games == "2016 Summer") %>% 
          drop_na(medal) %>% 
          group_by(team) %>% 
          count(medal) %>% 
          mutate(total_medals = sum(n)) %>% 
          ungroup()
)

#nur die meisten Medaillen behalten
medal_teams %>% 
          mutate(medal_reordered = factor(medal, 
                                          levels = c("Silver", "Gold", "Bronze"))) %>% 
          filter(total_medals == max(total_medals)) %>% 
          ggplot(aes(x = medal_reordered, y = n, fill = medal_reordered)) +
          geom_col() +
          scale_fill_manual(values = c("grey69", "Gold", "darkorange4") )

#http://sape.inf.usi.ch/quick-reference/ggplot2/colour



#d)
olympics %>% 
          group_by(year, season) %>% 
          distinct(id, .keep_all = T) %>% 
          count() %>% 
          ungroup() %>% 
          ggplot(aes(x = year, y = n, col = season)) +
          geom_line() +
          geom_point() +
          scale_x_continuous(breaks = c(seq(from = 1900, to = 2016, by = 20), 2016))


#e)
olympics %>% 
          filter(games == "2016 Summer") %>% 
          distinct(id, .keep_all = T) %>%
          ggplot(aes(x = age, y = "")) +
          geom_boxplot()

olympics %>% 
          filter(games == "2016 Summer") %>% 
          distinct(id, .keep_all = T) %>%
          ggplot(aes(x = sex, y = age)) +
          geom_boxplot()
