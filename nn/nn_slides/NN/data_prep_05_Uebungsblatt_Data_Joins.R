library(tidyverse)
library(janitor)



(studis_insg <- read_csv2("data/Studierende_Kreise_Basis.csv", skip = 1) %>% 
                    janitor::clean_names() %>% 
                    rename(nr = x1,
                           name = x2,
                           typ = x3) %>% 
                    select(nr:x2017_26) %>% 
                    pivot_longer(cols = x1995:x2017_26,
                                 names_to = "jahr",
                                 values_to = "anz_studierende") %>% 
                    mutate(typ = "insgesamt") %>% 
                    mutate(jahr = str_remove_all(jahr, "x")) %>% 
                    mutate(jahr = str_remove_all(jahr, "\\_.*")) %>% 
                    mutate(jahr = as.numeric(jahr)) %>% 
                    select(-typ) %>% 
                    rename(studierende_insg = anz_studierende) 
)

write_csv("data/studis_insg.csv", x = studis_insg)

(studis_insg_2017 <- studis_insg %>% 
                    filter(jahr == 2017) %>% 
                    select(-jahr)
)

write_csv("data/studis_insg_2017.csv", x = studis_insg_2017)

(studis_fh <- read_csv2("data/Studierende_Kreise_Basis.csv", skip = 1) %>% 
                    janitor::clean_names() %>% 
                    rename(nr = x1,
                           name = x2,
                           typ = x3) %>% 
                    select(nr, name, typ, x1998_27:x2017_46) %>% 
                    pivot_longer(cols = x1998_27:x2017_46,
                                 names_to = "jahr",
                                 values_to = "anz_studierende") %>% 
                    mutate(typ = "fh") %>% 
                    mutate(jahr = str_remove_all(jahr, "x")) %>% 
                    mutate(jahr = str_remove_all(jahr, "\\_.*")) %>% 
                    mutate(jahr = as.numeric(jahr)) %>% 
                    select(-typ) %>% 
                    rename(studierende_fh = anz_studierende) 
)

write_csv("data/studis_fh.csv", x = studis_fh)

(studis_fh_2017 <- studis_fh %>% 
                    filter(jahr == 2017) %>% 
                    select(-jahr)
)

write_csv("data/studis_fh_2017.csv", x = studis_fh_2017)





