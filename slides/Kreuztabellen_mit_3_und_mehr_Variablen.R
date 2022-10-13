library(tidyverse)
(beispieldaten <- palmerpenguins::penguins)

beispieldaten |> 
          group_by(year, species) |> 
          summarise(mean_bill_depth_mm = mean(bill_depth_mm, na.rm = T)) |> 
          ungroup() |> 
          pivot_wider(id_cols = "year", #<<
                      names_from = "species", #<<
                      values_from = "mean_bill_depth_mm") #<<

beispieldaten |> 
          group_by(year, species, sex) |> 
          summarise(mean_bill_depth_mm = mean(bill_depth_mm, na.rm = T)) |> 
          ungroup() 


beispieldaten |> 
          group_by(year, species, sex) |> 
          summarise(mean_bill_depth_mm = mean(bill_depth_mm, na.rm = T)) |> 
          ungroup() |> 
          pivot_wider(id_cols = c("year", "sex"), #<<
                      names_from = "species", #<<
                      values_from = "mean_bill_depth_mm") 


(data <- beispieldaten |> 
          group_by(year, species, sex) |> 
          summarise(mean_bill_depth_mm = mean(bill_depth_mm, na.rm = T)) |> 
          ungroup() |> 
          pivot_wider(id_cols = c("year", "sex"), #<<
                      names_from = "species", #<<
                      values_from = "mean_bill_depth_mm") |> 
          group_by(sex) |> 
          split(f = as.factor(.$Species))
          
)


data |> 
          map(as_tibble) |>

          set_names(str_c("DF, 1:3")) %>% 
          list2env(.GlobalEnv)

beispieldaten |> 
          count(species, year) |> 
          pivot_wider(names_from = "species",
                      id_cols = "year",
                      values_from = "n")



beispieldaten |> 
          count(species, year) |> 
          group_by(year) |> 
          mutate(share = n/sum(n))
          


beispieldaten |> 
          count(species, year) |>
          group_by(year) |> 
          mutate(anteil = n / sum(n)) |> 
          ungroup() |> 
          pivot_wider(names_from = "species",
                      id_cols = "year",
                      values_from = c("n", "anteil"))



beispieldaten |> 
          count(species) |> 
          write.table(file = "test.txt")

readr::write_csv(x = x, file = )

xlsx::write.xlsx(sheetName = )