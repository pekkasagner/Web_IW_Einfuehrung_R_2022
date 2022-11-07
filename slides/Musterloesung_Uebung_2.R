#Autor: Pekka Sagner
#Übung 2: Einführung in das Tidyverse
#----------------------------------------------------

# install.packages("tidyverse")
library(tidyverse)

#d)/e)/f))
datensatz_1 <- table1
#tidy

datensatz_2 <- table2
#nicht tidy

datensatz_3 <- table3
#nicht tidy

#Aufgabe 2

#a) Klassiche Funktionsschreibweise

f(g(x))

lernen(fahren(trinken(anziehen(aufstehen(zeit = 6 Uhr), kleidung = c(Hose, Pullover)), 
        getränk = kaffee, koffein = viel), Auto = FALSE, Fahrrad = TRUE, 
       Ziel = Hochschule), Laune = herausragend)

#b)
kommilitone |> 
          aufstehen(zeit = 6 Uhr) |> 
          anziehen(kleidung = c(Hose, Pullover)) |> 
          trinken(getränk = kaffee, koffein = viel) |> 
          fahren(Auto = FALSE, Fahrrad = TRUE) |> 
          lernen(Laune = herausragend)


#Aufgabe 3
#b) Daten einlesen
olympics <- read_csv(file = "data/olympics.csv")
#c)
#271.116 Beobachtungen
#

#d)
olympics |> 
          filter(games == "2016 Summer")
#13688 Beobachtungen für Sommerspiele 2016

#e)
olympics_2016 <- olympics |> 
          filter(games == "2016 Summer") |> 
          select(id, name, sex, age, height, weight, team, sport, event, medal)

#f)
#Gesucht: der/die größte/kleinste AthletIn
olympics_2016 |> 
          filter(height == max(height, na.rm = T))

olympics_2016 |> 
          filter(sex == "F") |> 
          filter(height == max(height, na.rm = T))

###
olympics_2016 |> 
          group_by(sex) |> 
          filter(height == max(height, na.rm = T))

#max und min und gender in einem:
olympics_2016 |> 
          group_by(sex) |> 
          filter(height == max(height, na.rm = T) | 
                           height == min(height, na.rm = T)) |> 
          View()

#alternative mit summarize
olympics_2016 |> 
          group_by(sex) |> 
          summarise(min_height = min(height, na.rm = T),
                    max_height = max(height, na.rm = T)) |> 
          ungroup() |> 
          mutate(diff_height = max_height - min_height)

#iv) Durchschnitt nach Geschlecht
olympics_2016 |> 
          distinct(id, .keep_all = TRUE) |>
          group_by(sex) |> 
          summarise(mean_height = mean(height, na.rm = T))

#v) Durchschnitt nach Geschlecht und Sportart
olympics_2016 |> 
          group_by(sex, sport) |> 
          distinct(id, .keep_all = T) |> 
          summarise(mean_height = mean(height, na.rm = T)) |> 
          ungroup() |> 
          group_by(sex) |> 
          filter(mean_height == max(mean_height) | 
                           mean_height == min(mean_height))

#vi) Durchschnitt AthletInnen
olympics_2016 |> 
          group_by(sex) |> 
          distinct(id, .keep_all = T) |> 
          summarise(mean_height = mean(height, na.rm = T))

#Missings anzeigen
olympics_2016 |> 
          filter(is.na(height))

#vii) Zum Verständnis getrennt:
olympics_2016 |> 
          filter(sex == "F") |> 
          distinct(id, .keep_all = T) |> 
          group_by(sport) |> 
          summarise(mean_height = mean(height, na.rm = T))

olympics_2016 |> 
          filter(sex == "M") |> 
          distinct(id, .keep_all = T) |> 
          group_by(sport) |> 
          summarise(mean_height = mean(height, na.rm = T))

#viii)Wie viele Athleten > 180cm? 
olympics_2016 |> 
          filter(sex == "M") |> 
          distinct(id, .keep_all = T) 
#6145 Athleten insgesamt
olympics_2016 |> 
          filter(sex == "M") |> 
          distinct(id, .keep_all = T) |> 
          filter(height > 180)
#3319 Athleten größer als 180
3319 / 6145

#Eleganter mit count()
olympics_2016 |> 
          filter(sex == "M") |> 
          distinct(id, .keep_all = T) |> 
          drop_na(height) |> 
          count(height > 180) |> 
          mutate(total_M = sum(n)) |> 
          mutate(share = n / total_M)

#Für Frauen
olympics_2016_F_taller_160 <- olympics_2016 |> 
          filter(sex == "F") |> 
          distinct(id, .keep_all = T) |> 
          drop_na(height) |> 
          count(height > 160) |> 
          mutate(total_F = sum(n)) |> 
          mutate(share = n / total_F)

#g)
olympics_2016 |> 
          distinct(id, .keep_all = T) |> 
          filter(sex == "F" & 
                           sport == "Volleyball" & 
                           age >= 20 & 
                           age <= 30) |> 
          mutate(bmi = weight / (height / 100)^2) |> 
          summarise(mean_bmi = mean(bmi, na.rm = T))


#Beispiel für arrange()
olympics_2016 |> 
          filter(sex == "F") |> 
          arrange(desc(height))

