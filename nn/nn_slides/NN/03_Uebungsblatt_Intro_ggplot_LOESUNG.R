library(tidyverse)

(dino_data <- read_tsv("data/The Datasaurus Dozen/DatasaurusDozen.tsv")
)


(dino_data_groups <- dino_data %>% 
          group_by(dataset) %>% 
          mutate(gruppe = cur_group_id()) %>% 
          mutate(gruppe = str_pad(gruppe, width = 2, side = "left", pad = "0")) %>% 
          ungroup() %>% 
          select(-dataset)
)


write_tsv(dino_data_groups, file = "data/datasaurus.tsv")



#Ab hier Lösung
dino_data_groups %>% 
          ggplot(aes(x,y)) + 
          geom_point() +
          facet_wrap(~gruppe)

dino_data_groups %>% 
          group_by(gruppe) %>% 
          summarise(across(where(is.numeric), 
                           list(mean, sd)))


dino_data_groups %>% 
          group_by(gruppe) %>% 
          summarise(mean_x = mean(x),
                    mean_y = mean(y),
                    std_x = sd(x),
                    std_y = sd(y))



#
olympics <- read_csv("data/olympics.csv")

(olympics_2016 <- olympics %>% 
          filter(games == "2016 Summer") %>% 
          drop_na(medal)
)

#b)
olympics_2016 %>% 
          count(sex) %>% 
          ggplot(aes(x = sex, y = n)) +
          geom_col()


olympics_2016 %>% 
          ggplot(aes(sex)) +
          geom_bar()


#c) #Varianten
#zuerstn
olympics_2016 %>% 
          count(team, sort = T) 

#und dann:
olympics_2016 %>% 
          filter(team == "United States") %>% 
          count(medal)

#oder zusammen:
olympics_2016 %>% 
          count(medal, team) %>% 
          group_by(team) %>% 
          mutate(total = sum(n)) %>% 
          ungroup() %>% 
          arrange(desc(total))


#grafisch
#Säulendiagramm
olympics_2016 %>% 
          filter(team == "United States") %>% 
          count(medal) %>% 
          ggplot(aes(x = medal, y = n)) +
          geom_col()


#etwas schicker
olympics_2016 %>% 
          filter(team == "United States") %>% 
          count(medal) %>% 
          mutate(medal = factor(medal, levels = c("Silver", "Gold", "Bronze"))) %>% 
          ggplot(aes(x = medal, y = n, fill = medal)) +
          geom_col() +
          # geom_label(aes(label = n), show.legend = F, nudge_y = -20) +
          scale_fill_manual(values = c("grey69", "gold", "darkorange4")) +
          labs(fill = "",
               x = "",
               y = "Anzahl gewonnener Medaillen",
               title = "Gewonnene Medaillen des US-Teams",
               subtitle = "Olympische Sommerspiele 2016",
               caption = "Quelle: TidyTuesday") +
          theme_minimal() +
          theme(legend.position = "none")

#http://sape.inf.usi.ch/quick-reference/ggplot2/colour


#d) #Basis
olympics %>% 
          group_by(games) %>% 
          distinct(id, .keep_all = T) %>% 
          ungroup() %>% 
          count(games) %>% 
          ggplot(aes(x = games, y = n)) +
          geom_col()

#etwas besser: getrennt nach Season
olympics %>% 
          group_by(games) %>% 
          distinct(id, .keep_all = T) %>% 
          ungroup() %>% 
          count(games, season) %>% 
          ggplot(aes(x = games, y = n, fill = season)) +
          geom_col()

#noch besser: Jahr auf x-Achse...außerdem Liniendiagramm
olympics %>% 
          group_by(games) %>% 
          distinct(id, .keep_all = T) %>% 
          ungroup() %>% 
          count(games, year, season) %>% 
          ggplot(aes(x = year, y = n, col = season)) +
          geom_line() +
          geom_point() 

#e)
olympics %>% 
          filter(games == "2016 Summer") %>% 
          group_by(sex) %>% 
          distinct(id, .keep_all = T) %>% 
          ungroup() %>% 
          ggplot(aes(x = sex, y = age)) +
          geom_boxplot()
