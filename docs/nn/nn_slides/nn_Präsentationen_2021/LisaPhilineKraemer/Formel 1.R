#Formel1
#Autor: Lisa Philine Krämer
#zuletzt bearbeitet: 10.01.22

#Header
library(tidyverse)

#Daten einlesen
tuesdata <- tidytuesdayR::tt_load('2021-09-07')


rennstrecke <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/circuits.csv')
constructor_results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_results.csv')
constructor_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_standings.csv')
constructors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructors.csv')
driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')
lap_times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/lap_times.csv')
pit_stops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/pit_stops.csv')
qualifying <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/qualifying.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')
results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/seasons.csv')
status <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/status.csv')


###############################################################################

#constructor points

constructor_results %>% 
  left_join(constructors, by = "constructorId") %>% 
  group_by(name) %>% 
  summarise(punkte_insg = sum(points)) %>% 
  filter(punkte_insg > 700) %>% 
  ggplot(aes(x = reorder(name, punkte_insg, desc), y = punkte_insg)) +
  geom_col()

#
constructor_results %>% 
  left_join(constructors, by = "constructorId") %>% 
  left_join(races, by = "raceId") %>% 
  select("constructorId", "raceId","name.x", "points", "year") %>% 
  filter(name.x %in% c("Ferrari", "Mercedes", "McLaren", "Red Bull", "Williams", 
                     "Renault","Force India", "Team Lotus", "Benetton", "Lotus F1")) %>% 
  group_by(name.x, year) %>% 
  summarise(points_sum = sum(points)) %>% 
  mutate(points_cumsum = cumsum(points_sum)) %>% 
  mutate(constr_reordered = factor(name.x, levels = c("Ferrari", "Mercedes", "McLaren", "Red Bull", "Williams", 
                                                      "Renault","Force India", "Team Lotus", "Benetton", "Lotus F1"))) %>% 
  ggplot(aes(x = year, y = points_cumsum, col = constr_reordered)) +
  geom_line() +
  labs(x = "", y = "Punkte",
       color = "Konstrukteur",
       title = "Kumulierte Punkteanzahl der Konstrukteure",
       subtitle = "von 1956 bis 2021",
       caption = "Quelle: TidyTuesday") +
  scale_x_continuous(breaks =
                       c(seq(from = 1950,
                             to = 2021,
                             by = 10))) +
  scale_y_continuous(breaks = 
                       c(seq(from = 0,
                             to = 10000,
                             by = 1000)),
                             expand = c(0, 0),
                     limits = c(0, 9000)) +
  theme(axis.line.x  = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(size = rel(2.5)),
        panel.grid = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "grey95"),
        axis.title.y = element_text(size = rel(1.25), color = "firebrick"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.key = element_rect( fill = "white"),
        legend.title = element_text(size = rel(1.25)),
        legend.text = element_text(size = rel(1)))


###############################################################################

#status
    
races %>% 
  left_join(results, by = "raceId") %>% 
  filter(circuitId == "6",
         year == "2014") %>% view()

races %>% 
  left_join(results, by = "raceId") %>% 
  filter(circuitId == "6",
         year == "2012") %>% view()


#Montecarlo
races %>% 
  left_join(results, by = "raceId") %>% 
  filter(circuitId == "6",
         year %in% c("2014", "2016")) %>% 
  left_join(status, by = "statusId") %>% 
  select("status", "year", "driverId") %>% 
  group_by(year) %>% 
  count(status) %>% 
  mutate(status_reordered = factor(status, levels = c("Finished", "+1 Lap", "+2 Laps",
                                                      "+3 Laps", "+4 Laps", "Accident",
                                                      "Collision", "Engine", "Mechanical",
                                                      "Retired", "Turbo", "Withdrew",
                                                      "Collision damage", "Gearbox",
                                                      "Power Unit", "Puncture"))) %>% 
  ggplot(aes(x = "", y = n, fill = status_reordered)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  facet_wrap(~ year) +
  scale_fill_manual(values = c("limegreen", "green2", "lightgreen", "lightgoldenrod1",
                               "orange1","darkorange", "coral1", "tomato", 
                               "red", "deeppink", "violet", "plum",
                               "slateblue", "royalblue", "blue", "blue4")) +
  scale_y_continuous(breaks = 
                       c(seq(from = 0,
                             to = 30,
                             by = 1)),
                     expand = c(0, 0)) +
  labs(x = "", y = "",
       title = "Status der Rennen in Montecarlo",
       subtitle = "in den Jahren 2014 und 2016",
       caption = "Quelle: TidyTuesday") +
  theme(plot.title = element_text(size = rel(2.5)),
        panel.grid = element_line(color = "grey95"),
        plot.background = element_rect(fill = "grey95"),
        legend.key = element_rect( fill = "white"),
        axis.ticks.y = element_blank(),
        legend.title = element_text(size = rel(0)),
        legend.text = element_text(size = rel(1)),
        strip.background = element_rect( fill = "grey85"),
        strip.text = element_text(color = "firebrick", 
                                  size = rel(1.25), face = "bold"))


#Spa

races %>% 
  left_join(results, by = "raceId") %>% 
  filter(circuitId == "13",
         year %in% c("2014", "2016")) %>% 
  left_join(status, by = "statusId") %>% 
  select("status", "year", "driverId") %>% 
  group_by(year) %>% 
  count(status) %>% 
  mutate(status_reordered = factor(status, levels = c("Finished", "+1 Lap", "+2 Laps",
                                                      "+3 Laps", "+4 Laps", "Accident",
                                                      "Collision", "Engine", "Mechanical",
                                                      "Retired", "Turbo", "Withdrew",
                                                      "Collision damage", "Gearbox",
                                                      "Power Unit", "Puncture"))) %>%
  ggplot(aes(x = "", y = n, fill = status_reordered)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  facet_wrap(~ year) +
  scale_fill_manual(values = c("limegreen", "green2","darkorange", 
                               "red", "deeppink",
                               "slateblue", "royalblue", "blue", "blue3")) +
  scale_y_continuous(breaks = 
                       c(seq(from = 0,
                             to = 30,
                             by = 1)),
                     expand = c(0, 0)) +
  labs(x = "", y = "",
       title = "Status der Rennen in Spa Francorchamps",
       subtitle = "in den Jahren 2014 und 2016",
       caption = "Quelle: TidyTuesday") +
  theme(plot.title = element_text(size = rel(2.5)),
        panel.grid = element_line(color = "grey95"),
        plot.background = element_rect(fill = "grey95"),
        legend.key = element_rect( fill = "white"),
        legend.title = element_text(size = rel(0)),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = rel(1)),
        strip.background = element_rect( fill = "grey85"),
        strip.text = element_text(color = "firebrick", 
                                  size = rel(1.25), face = "bold"))



#
races %>% 
  left_join(results, by = "raceId") %>% 
  filter(circuitId == "6",
         year == "2014") %>% view()
#1. Nico Rosberg, 2.Lewis Hamilton, 3.Daniel Ricciardo

###############################################################################

#Fahrer mit den meisten Grand Prix Siege
  
results %>% 
  left_join(drivers, by = "driverId") %>% 
  filter(position == "1") %>% 
  select("raceId", "driverId", "code", "position", "forename", "surname") %>%
  unite(name, c(forename, surname), sep = " ") %>% 
  group_by(name) %>% 
  mutate(wins_cumsum = cumsum(position)) %>% 
  summarise(max = max(wins_cumsum)) %>% 
  filter(name %in% c("Lewis Hamilton", "Michael Schumacher", "Sebastian Vettel", 
                       "Alain Prost", "Ayrton Senna", "Fernando Alonso", "Nigel Mansell",
                       "Jackie Stewart", "Jim Clark", "Niki Lauda", "Juan Fangio",
                       "Nelson Piquet", "Nico Rosberg")) %>% 
  ggplot(aes(x = name, y = max, fill = "firebrick")) +
  geom_col() +
  scale_y_continuous(breaks = 
                       c(seq(from = 0,
                             to = 110,
                             by = 20)),
                     expand = c(0, 0),
                     limits = c(0, 110)) +
  labs(title = "Fahrer mit den meisten GrandPrix Siegen",
       x = "", y = "Anzahl Siege",
       caption = "*HAM: 103
       Quelle: TidyTuesday") +
  theme(axis.line.x  = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(size = rel(2.5)),
        panel.grid = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "grey95"),
        axis.title.y = element_text(size = rel(1.25), color = "firebrick"),
        axis.title.x = element_text(size = rel(1.25), color = "firebrick"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")
  
  
###############################################################################

#lap

races %>% 
  left_join(rennstrecke, by = "circuitId") %>% 
  left_join(lap_times, by = "raceId") %>% 
  select("circuitId", "name.x", "year", "name.y", "driverId", "lap", "milliseconds") %>% 
  filter(circuitId %in% c("1", "6", "8", "9", "34", "13", "14", "20")) %>% 
  group_by(circuitId, year) %>%
  mutate(lap_min = min(milliseconds)) %>% 
  filter(lap_min == milliseconds) %>%
  left_join(drivers, by = "driverId") %>% 
  unite(name_driver, c(forename, surname), sep = " ") %>% 
  select("circuitId", "name.x", "year", "name.y", "driverId", "lap", "milliseconds",
         "lap_min", "name_driver") %>% 
  ggplot(aes(x = name.y, y = lap_min, color = "firebrick")) +
  geom_boxplot() +
  scale_y_continuous(breaks = 
                       c(seq(from = 0,
                             to = 250000,
                             by = 20000)),
                     expand = c(0, 0),
                     limits = c(70000, 220000)) +
  labs(x = "Rennstrecke", y = "Zeit in ms",
       title = "Die schnellsten Laps nach Rennstrecke",
       subtitle = "über die Jahre",
       caption = "Quelle: TidyTuesday") +
  theme(axis.line.x  = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(size = rel(2.5)),
        panel.grid = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "grey95"),
        axis.title.y = element_text(size = rel(1.25), color = "firebrick"),
        axis.title.x = element_text(size = rel(1.25), color = "firebrick"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")


#########

races %>% 
  left_join(rennstrecke, by = "circuitId") %>% 
  left_join(lap_times, by = "raceId") %>% 
  select("circuitId", "name.x", "year", "name.y", "driverId", "lap", "milliseconds") %>% 
  filter(circuitId == "13") %>% 
  group_by(circuitId, year) %>%
  mutate(lap_min = min(milliseconds)) %>% 
  filter(lap_min == milliseconds) %>%  view()
#
races %>% 
  left_join(results, by = "raceId") %>% 
  filter(circuitId == "13",
         year == "2021") %>% 
  left_join(status, by = "statusId") %>% 
  select("status", "year", "driverId")

races %>% 
  filter(circuitId == "13", year == "2021")

results %>% 
  filter(raceId == "1063") %>% view()


###############################################################################

#Wer hat die meisten schnellsten Runden

races %>% 
  left_join(rennstrecke, by = "circuitId") %>% 
  left_join(lap_times, by = "raceId") %>% 
  select("circuitId", "name.x", "year", "name.y", "driverId", "lap", "milliseconds") %>% 
  group_by(circuitId, year) %>%
  mutate(lap_min = min(milliseconds)) %>% 
  filter(lap_min == milliseconds) %>%
  left_join(drivers, by = "driverId") %>% 
  unite(name_driver, c(forename, surname), sep = "
        ") %>% 
  group_by(name_driver) %>% 
  count(name_driver) %>% 
  filter(n > 7) %>% 
  ggplot(aes(x = name_driver, y = n, color = "firebrick")) +
  geom_point() +
  labs(title = "Fahrer mit den meisten schnellsten Runden",
       x = "", y = "Anzahl schnellster Laps",
       caption = "Quelle: TidyTuesday") +
  theme(axis.line.x  = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(size = rel(2.5)),
        panel.grid = element_line(color = "lightgrey"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "grey95"),
        axis.title.y = element_text(size = rel(1.25), color = "firebrick"),
        axis.title.x = element_text(size = rel(1.25), color = "firebrick"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")


###############################################################################
