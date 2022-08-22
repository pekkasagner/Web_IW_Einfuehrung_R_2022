library(tidyverse)


(test <- read_delim("data/regional_averages_tm_12.txt", delim = ";", 
                    col_types = cols(.default = "c")) %>% 
                    janitor::clean_names() %>% 
                    select(jahr, monat, nordrhein_westfalen, deutschland) %>% 
                    mutate(across(where(is.character), str_trim)) %>% 
                    mutate(across(c(nordrhein_westfalen, deutschland), as.numeric))
)


files <- list.files(".", pattern = "*.txt", full.names = T, recursive = T)

(daten <- files %>% 
          map_df(read_delim, delim = ";", col_types = cols(.default = "c")) %>% 
          janitor::clean_names() %>% 
          select(jahr, monat, deutschland) %>% 
          mutate(across(where(is.character), str_trim)) %>% 
          mutate(across(c(deutschland), as.numeric)) %>% 
          mutate(date = as.Date(paste(jahr, "-", monat, "-01", sep = ""))) %>% 
          arrange(date) %>% 
          mutate(jahreszeit = case_when(monat %in% c("03","04","05") ~ "Frühling",
                                        monat %in% c("06","07","08") ~ "Sommer",
                                        monat %in% c("09","10","11") ~ "Herbst",
                                        monat %in% c("12","01","02") ~ "Winter"))
)         



daten %>% 
          filter(jahr >= 2015) %>% 
          ggplot(aes(date, deutschland)) +
          # geom_line() +
          geom_point(aes(col = jahreszeit), size = 10)
          
          
