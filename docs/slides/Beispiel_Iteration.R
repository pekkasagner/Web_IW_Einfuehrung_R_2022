library(tidyverse)
# 
# (gapminder <- gapminder::gapminder)
# 
# # Je Kontinent report: 
# #1 Liste mit Top-3 und 
# #2 Bottom-3 nach BIP pro Kopf
# #3 Korrelation von BIP-Pro-Kopf und Lebenserwartung pro Jahr
# 
# top_three_gdp <- gapminder::gapminder |> 
#           filter(continent == "Europe") |> 
#           filter(year == max(year)) |> 
#           slice_max(gdpPercap, n = 3)
# xlsx::write.xlsx(x = top_three_gdp, file = "Europe_top_three_gdp.xlsx")
# 
# bottom_three_gdp <- gapminder::gapminder |> 
#           filter(continent == "Europe") |> 
#           filter(year == max(year)) |> 
#           slice_min(gdpPercap, n = 3) |> 
#           arrange(desc(gdpPercap))
# xlsx::write.xlsx(x = bottom_three_gdp, file = "Europe_bottom_three_gdp.xlsx")
# 
# correlation <- gapminder::gapminder |>
#           filter(continent == "Europe") |>
#           group_by(year) |>
#           summarise(correlation_gdp_lifeExp = cor(gdpPercap, lifeExp))
# xlsx::write.xlsx(x = correlation, file = "Europe_correlation.xlsx")


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



