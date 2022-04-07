#Lösungen zur Übung 2 R-Kurs
#Autor: Pekka Sagner
#Zuletzt bearbeitet: 22.10.2021

#Aufgabe 1
#b) Installieren Tidyverse
install.packages("tidyverse")

#c) tidyverse laden
library(tidyverse)

#d) Übungsdatensätze einladen und betrachten
datensatz1 <- table1
datensatz2 <- table2
datensatz3 <- table3

#Aufgabe 2
#a)
lernen(fahren(trinken(anziehen(aufstehen(Zeit = "6:00 Uhr"), 
          Hose = TRUE, Pullover = TRUE), 
         Kaffee = "stark"), 
       Auto = F, Fahrrad = T), 
       Stimmung = "herausragend")
#b)
aufstehen(Zeit = "6:00 Uhr") %>% 
          anziehen(Hose = TRUE, Pullover = TRUE) %>% 
          trinken(Kaffee = "stark") %>% 
          fahren(Auto = F, Fahrrad = T) %>% 
          lernen(Stimmung = "herausragend")


#Augabe 3
#b) Daten einlesen
olympics <- read_csv("data/olympics.csv")
#c) Beobachtungen: 271.116
#Eine AthletIn bei einem der olympischen Spiele und einem Wettbewerb

#d)
filter(olympics, games == "2016 Summer")

#mit Pipe:
olympics %>% 
          filter(games == "2016 Summer")
#Beobachtungen: 13.688

#e)
olympics_2016 <- olympics %>% 
          filter(games == "2016 Summer") %>% 
          select(c(id, name, sex, age, height, weight, team, sport, 
                   event, medal))

#f)
#i)
#größte
olympics_2016 %>% 
          summarise(groesste = max(height, na.rm = T))
#kleinste
olympics_2016 %>% 
          summarise(kleinste = min(height, na.rm = T))

#filtern:
olympics_2016 %>% 
          filter(height == max(height, na.rm = T))

olympics_2016 %>% 
          filter(height == min(height, na.rm = T))

#arrange:
olympics_2016 %>% 
          arrange(height)

olympics_2016 %>% 
          arrange(desc(height))

#"billo-variante"
olympics_2016 %>% 
          select(height) %>% 
          max(na.rm = T)

#ii)
olympics_2016 %>% 
          summarise(groesste = max(height, na.rm = T), 
                    kleinste = min(height, na.rm = T)) %>% 
          mutate(differenz = groesste - kleinste) %>% 
          mutate(differenz_proz = groesste / kleinste - 1)

#iii)
olympics_2016 %>% 
          filter(height == max(height, na.rm = T) |
                 height == min(height, na.rm = T)) %>% 
          View()


#iv)
#wichtig: wir möchten jeden AthletIn nur einmal berücksichtigen!
olympics_2016 %>% 
          distinct(id, .keep_all = TRUE) %>% 
          summarise(mean_height = mean(height, na.rm = T)) %>% 
          View()

#v)
olympics_2016 %>% 
          group_by(sport) %>% 
          distinct(id, .keep_all = T) %>% 
          summarise(mean_height = mean(height, na.rm = T)) %>% 
          filter(mean_height == max(mean_height) |  
                           mean_height == min(mean_height))


#vi)
olympics_2016 %>% 
          group_by(sex) %>% 
          distinct(id, .keep_all = T) %>% 
          summarise(mean_height = mean(height, na.rm = T))

#vii)
olympics_2016 %>% 
          group_by(sport, sex) %>% 
          distinct(id, .keep_all = T) %>% 
          summarise(mean_height = mean(height, na.rm = T)) %>% 
          View()

#viii)
olympics_2016 %>%
          distinct(id, .keep_all = T) %>% 
          filter(sex == "M") %>%
          filter(height > 180)

#count()
olympics_2016 %>% 
          distinct(id, .keep_all = T) %>% 
          filter(sex == "M") %>% 
          drop_na(height) %>% 
          count(height > 180) %>% 
          mutate(total = sum(n)) %>% 
          mutate(share = n / total)

olympics_2016 %>% 
          distinct(id, .keep_all = T) %>% 
          filter(sex == "F") %>% 
          drop_na(height) %>% 
          count(height > 160) %>% 
          mutate(total = sum(n)) %>% 
          mutate(share = n / total)

#Anteile für Gruppen innerhalb einer Operation
olympics_2016 %>% 
          distinct(id, .keep_all = T) %>% 
          drop_na(height) %>% 
          group_by(sex) %>% 
          count(height > 180) %>%
          mutate(total = sum(n)) %>% 
          ungroup() %>% 
          mutate(share = n / total)

#g)
olympics_2016 %>% 
          distinct(id, .keep_all = T) %>% 
          filter(sex == "F" & age %in% c(20:30) & sport == "Volleyball") %>%
          #filter(sex == "F" & age >= 30 & age <= 30)
          #filter(sex == "F" & between(age, 20, 30))
          mutate(bmi = weight / (height/100)^2 ) %>%
          summarise(mean_bmi = mean(bmi))
