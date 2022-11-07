#Übung 6 - Funktionen und Iteration
#Autor: Pekka Sagner


library(tidyverse)

(daten <- tibble(x = c(1, NA, 2, 1),
                 y = c(3, 4, 4, 4),
                 z = c(NA, NA, "A", "B"))
)


#b)
my_share_function <- function(data, vector) {
          data |> 
                    count({{ vector }}) |> 
                    mutate(share = n/sum(n))
}


daten |> 
          my_share_function(x)


#c)
#Vektor numerisch?
my_num_check <- function(data, vector) {
          if(is.numeric(data |> 
                        pull({{ vector }})) == TRUE) {
                    print("Vektor ist numerisch.")
          }
          
          if(is.numeric(data |> 
                        pull({{ vector }} )) == FALSE) {
                    print("Vektor ist nicht numerisch.")
          }
}

my_num_check(daten, z)


#d)

my_check_function <- function(data, vector) {
          
          if(is.numeric(data |> 
                        pull({{ vector }})) == TRUE) {
                    print("Vektor ist numerisch.")
                    
                    
                    if(any( data |> pull( {{ vector }} ) > 3) == TRUE)   {
                              print("Vektor enhält Werte größer 3.")
                              
                    }
                    
                    if(all( data |> pull( {{ vector }} ) <= 3) == TRUE | 
                              any(is.na(data |> pull( {{ vector }} ) <= 3)) )  {
                              print("Vektor enhält keine Werte größer 3. Achtung, Vektor enthält auch Missings.")
                              
                    }
          }
          
          if(is.numeric(data |> 
                        pull({{ vector }})) == FALSE) {
                    print("Vektor ist nicht numerisch.")
          }
}

my_check_function(daten, y)


#Aufgabe 2:

library(tidyverse)
# 
(gapminder <- gapminder::gapminder)
# 


#b)
# # Je Kontinent report: 
# #1 Liste mit Top-3 und 
# #2 Bottom-3 nach BIP pro Kopf
# #3 Korrelation von BIP-Pro-Kopf und Lebenserwartung pro Jahr
# 
top_three_gdp <- gapminder::gapminder |>
          filter(continent == "Europe") |>
          filter(year == max(year)) |>
          slice_max(gdpPercap, n = 3)
xlsx::write.xlsx(x = top_three_gdp, file = "Europe_top_three_gdp.xlsx")

bottom_three_gdp <- gapminder::gapminder |>
          filter(continent == "Europe") |>
          filter(year == max(year)) |>
          slice_min(gdpPercap, n = 3) |>
          arrange(desc(gdpPercap))
xlsx::write.xlsx(x = bottom_three_gdp, file = "Europe_bottom_three_gdp.xlsx")

correlation <- gapminder::gapminder |>
          filter(continent == "Europe") |>
          group_by(year) |>
          summarise(correlation_gdp_lifeExp = cor(gdpPercap, lifeExp))
xlsx::write.xlsx(x = correlation, file = "Europe_correlation.xlsx")



#c)
my_continent_report_function <- function(continent_name) {
          top_three_gdp <- gapminder::gapminder |> 
                    filter(continent == {{ continent_name }}) |> 
                    filter(year == max(year)) |> 
                    slice_max(gdpPercap, n = 3)
          
          xlsx::write.xlsx(x = top_three_gdp, file = paste0({{ continent_name }} , "_top_three_gdp.xlsx"))
          
          bottom_three_gdp <- gapminder::gapminder |> 
                    filter(continent == {{ continent_name }}) |> 
                    filter(year == max(year)) |> 
                    slice_min(gdpPercap, n = 3) |> 
                    arrange(desc(gdpPercap))
          
          xlsx::write.xlsx(x = bottom_three_gdp, file = paste0({{ continent_name }} , "_bottom_three_gdp.xlsx"))
          
          correlation <- gapminder::gapminder |>
                    filter(continent == {{ continent_name }}) |>
                    group_by(year) |>
                    summarise(correlation_gdp_lifeExp = cor(gdpPercap, lifeExp))
          xlsx::write.xlsx(x = correlation, file = paste0({{ continent_name }} , "_correlation.xlsx"))
}

(vec_continent_names <- gapminder::gapminder |> 
                    distinct(continent) |> 
                    pull(continent)
)

map(.x = vec_continent_names, .f = my_continent_report_function)



