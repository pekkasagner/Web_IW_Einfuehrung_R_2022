library(tidyverse)
library(sf)

corona <- read_sf("data/RKI_Corona_Landkreise.geojson")


corona %>% 
          select(1:Shape__Length) %>% 
          write_sf("data/Geometrie_Corona_Landkreise.geojson")

corona %>% 
          ggplot(aes(fill = cases7_per_100k)) +
          geom_sf() +
          scale_fill_viridis_c(direction = -1)


corona %>% 
          as_tibble() %>% 
          select(-geometry) %>% 
          View()


corona %>% 
          select()