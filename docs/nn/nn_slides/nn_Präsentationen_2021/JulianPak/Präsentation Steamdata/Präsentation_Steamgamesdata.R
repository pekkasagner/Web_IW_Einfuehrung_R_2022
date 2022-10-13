#Header
#Autor: Julian Pak

library(tidyverse)
library(ggthemes)
library(readxl)
library(sf)

#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("gcookbook")
#install.packages ("dplyr")
#install.packages("ggridges")

library(ggridges)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gcookbook)
#Daten einladen
games <-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

#Überblick über die Daten verschaffen
games %>% 
  count(year)


new <-games %>% 
  group_by(year,month) %>% 
  summarise(n = n()) %>%
  arrange(desc(year), match(month,month.name))

#1 Visualisierung: avg. steam player darstellen über die Jahre -> Corona Ausmaß zeigen?

aktivespieler <- games %>% 
  group_by(year,month) %>% 
  summarise(Summe = sum(avg)/1000) %>%  
  mutate (date = ymd(paste(year, month, '15'))) %>% #workaround weil ich keinen Tag habe
  arrange(date)
        
aktivespieler %>% 
  ggplot(aes(x = date, y = Summe))+
  geom_line(aes(col = Summe), size = 2, show.legend = FALSE)+
  labs(title = "Anzahl an aktiven Spieler auf der Plattform Steam",
       subtitle = "im Zeitraum von 2012-2021",
       x="",
       y="Anzahl aktiver Spieler in Tausend")+
  theme(axis.title = element_text(color = "#004570", size = 20),
        axis.text = element_text(size = 15),
        title = element_text(color = "#004570", size = 25),
        panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                        linetype = "solid"))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  geom_vline(xintercept = as.Date("2020-03-11"),  col = "red")+ #11 März "Erklärung zur Pandemie in Deutschland"
  geom_text(aes(x= as.Date("2020-10-11"), y = 2000, label ="Coronabeginn"), col ="red", size = 9)+
  scale_color_viridis_c(direction = -1)
  
 
#2. Visualisierung: Anzahl an gamereleases pro Monat auf Steam

releases <- games %>% 
  mutate (date = ymd(paste(year, month, '15'))) %>%
  group_by(gamename) %>% 
  filter(date == min(date)) %>%
  group_by(date) %>% 
  mutate(releases_per_month = n()) %>% 
  filter(releases_per_month< 200)

releases %>% 
  ggplot(aes(x= date, y = releases_per_month, fill = releases_per_month))+
  geom_col()+
  scale_fill_viridis_b(direction = -1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.title = element_text(color = "#004570", size = 20),
        axis.text = element_text(size = 15),
        title = element_text(color = "#004570", size = 25),
        panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                        linetype = "solid"))+
  labs(title = "Anzahl der Spielreleases auf der Plattform Steam",
       x= "",
       y= "Anzahl Releases in Tausend",
       fill = "")
 

#3. Visualisierung: top 7 Games aus 2021 mit den meisten Spielern

games_2020 <- games %>%
  filter(year == 2020) %>% 
  group_by(gamename) %>%
  drop_na(avg) %>% 
  summarise(summe_spieler = sum (avg)/1000) %>% 
  slice_max(order_by = summe_spieler, n= 7)
  #test <-as.data.frame(summe_spieler)
  

  
games_2020 %>% 
  ggplot(aes(x= reorder(gamename, desc(summe_spieler)), y= summe_spieler, 
             fill = summe_spieler))+
  geom_col(show.legend = FALSE)+
  theme(axis.title = element_text(color = "#004570", size = 20),
        axis.text = element_text(size = 13),
        title = element_text(color = "#004570", size = 25,),
        panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                               linetype = "solid"))+
  labs(title = "Die 7 meinstgespielten Steamspiele 2020",
       x= "",
       y= "Anzahl der Spieler in Tausend")
  #geom_text(aes(label = summe_spieler), vjust = -0,2)
  




# Problem: Kann die Labels darüber nicht einfügen
# Weil es kein Data Frame ist
# die Funktion as.data.frame oder data.frame funktionieren aber nicht, weil er "summe_spieler" nicht findet

###################################### Test
# Above the top
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) +
  geom_col() +
  geom_text(aes(label = Weight), vjust = -0.2)

print(games_2020)
print(cabbage_exp)




 


#4. Visualisierung: Peak Spieler der 50 größten Peaks

peaks <- games %>% 
  slice_max(order_by = peak, n= 50) %>% 
  #levels(peak) <- c("PLAYERUNKNOWN'S BATTLEGROUNDS","Counters-Strike: Global Offensive", "Dota 2")
  #peak_reorder <- as.factor (peak)
  #mutate(peak_reorder = factor(peak, levels = c("PLAYERUNKNOWN'S BATTLEGROUNDS","Counters-Strike: Global Offensive", "Dota 2")))
  peak_reorder = factor(peak)
  
peaks %>% 
  ggplot(aes(x= peak/1000, y=gamename, fill = gamename))+
  geom_density_ridges()+
  theme(axis.title = element_text(color = "#004570", size = 20),
        axis.text = element_text(size = 15),
        title = element_text(color = "#004570", size = 25),
        panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                        linetype = "solid"))+
  labs(title = "Die 50 größten Peaks bei Spielerzahlen",
       x= "Peak Spieleranzahl in Tausend",
       y= "",
       fill = "Spiele")
  

###Problem: ich kann die levels nicht verschieben
#level ändern geht nur bei factor, ich darf aber nicht zu factor ändern
#oder weiß zumindest nicht wie






