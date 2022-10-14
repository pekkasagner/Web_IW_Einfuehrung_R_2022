#Descriptives mit dem SOEP-Dummy


#a)
library(tidyverse)
#install.packages("haven")
library(haven)

#Daten einlesen
(soep_dummy <- read_dta("data/soep_dummy/soep_dummy.dta") |> 
                    mutate(region = as.factor(region))
)

# b)
#1 Summary
summary(soep_dummy)
#Hieraus könnte man schließen, dass "region" schlecht definiert ist --> mögliche Lösung mit as.factor()

glimpse(soep_dummy)

View(soep_dummy)

# c)
#Summary-Statistics berechnen:
soep_dummy |> 
          summarise(mean_income = mean(income))

#Weitere Summary-Statistics für numerische Variablen:

# mean(x, na.rm = FALSE)    Arithmetic mean
# sd(x)                     (Sample) Standard Deviation
# var(x)                    (Sample) Variance
# 
# median(x)                 Median
# quantile(x, probs, type)  Quantile of x.  probs: vector with probabilities
# min(x)                    Minimum value of x
# max(x)                    Maximum value of x
# range(x)                  x_min and x_max


soep_dummy |> 
          group_by(jahr) |> 
          summarise(mean_income = mean(income)) |> 
          ungroup()

soep_dummy |> 
          group_by(jahr, region) |> 
          summarise(mean_income = mean(income)) |> 
          ungroup()


library(spatstat.geom)

#Summary-Statistics berechnen:
soep_dummy |> 
          summarise(mean_income = weighted.mean(income, w = weight))

#Weitere Summary-Statistics für numerische Variablen:

# mean(x, na.rm = FALSE)    Arithmetic mean
# sd(x)                     (Sample) Standard Deviation
# var(x)                    (Sample) Variance
# 
# median(x)                 Median
# quantile(x, probs, type)  Quantile of x.  probs: vector with probabilities
# min(x)                    Minimum value of x
# max(x)                    Maximum value of x
# range(x)                  x_min and x_max


soep_dummy |> 
          group_by(jahr) |> 
          summarise(mean_income = weighted.mean(income, w = weight)) |> 
          ungroup()

soep_dummy |> 
          group_by(jahr, region) |> 
          summarise(mean_income = weighted.mean(income, w = weight)) |> 
          ungroup()



# e)
soep_dummy |> 
          summarise(across(.cols = c(income, bildung, anz_kind), 
                           .fns = mean, 
                           na.rm = T))

#f)
#Anwendung mehrerer Funktionen auf alle numerischen Variablen:
soep_dummy |> 
          summarise(across(.cols = where(is.numeric), 
                           .fns = list(median = median, sd = sd), 
                           na.rm = T, 
                           .names = "{.fn}_{.col}"))


#Aufgabe 2
# a)
soep_dummy |> 
          filter(jahr == 2002) |> 
          count(sex) |> 
          mutate(share = n/sum(n))

#b)
soep_dummy |> 
          count(sex, jahr) |>
          group_by(jahr) |> 
          mutate(share = n/sum(n)) |> 
          ungroup()

#c)
soep_dummy |> 
          filter(jahr == 2002) |> 
          count(sex, wt = weight) |> 
          mutate(share = n/sum(n))

soep_dummy |> 
          count(sex, jahr, wt = weight) |>
          group_by(jahr) |> 
          mutate(share = n/sum(n)) |> 
          ungroup()

#d)
soep_dummy |> 
          count(jahr, eink_bed = income > 10000, wt = weight) |> 
          group_by(jahr) |> 
          mutate(share = n/sum(n)) |> 
          ungroup()

(speich <- soep_dummy |> 
          count(jahr, eink_bed = income > 10000, wt = weight) |> 
          group_by(jahr) |> 
          mutate(share = n/sum(n)) |> 
          ungroup() |> 
          select(-n) |>
          pivot_wider(names_from = jahr,
                      values_from = share,
                      id_cols = eink_bed) 
)

#e)
xlsx::write.xlsx(x = speich, file = "hallo.xlsx")

# XXX
# 
# 
# #Anwendung auf alle numerischen Variablen:
# soep_dummy |> 
#           summarise(across(.cols = where(is.numeric), .fns = weighted.mean, na.rm = T, w = weight))
# 
# #Anwendung zweier Funktionen auf eine Variable
# soep_dummy |> 
#           summarise(mean_income = mean(income), sd_income = sd(income))
# 
# 
# #Anwendung mehrerer Funktionen auf alle numerischen Variablen:
# soep_dummy |> 
#           summarise(across(.cols = where(is.numeric), 
#                            .fns = list(median = median, sd = sd), 
#                            na.rm = T, 
#                            .names = "{.fn}_{.col}"))
# 
# #Häufigkeiten mit count()
# soep_dummy |> 
#           count(jahr)
# 
# #Anteile mit count() und sum()
# soep_dummy |> 
#           count(jahr) |> 
#           mutate(total = sum(n)) |> 
#           mutate(share = n/total) |> 
#           mutate(share_2 = n/sum(n))
# 
# #Kreuztabellen
# soep_dummy |> 
#           count(jahr, region) |> #Übergang zu besser lesbarer Tabelle --> vorsicht, nicht mehr tidy!
#           pivot_wider(names_from = "region", values_from = "n", id_cols = "jahr")
# 
# #Kreuztabellen mit Anteilen
# soep_dummy |> 
#           count(jahr, region) |>
#           group_by(jahr) |> 
#           mutate(share = n/sum(n)) |> 
#           ungroup()
#           
# #Kreuztabellen mit Anteilen und besser lesbar/bzw. zum rausschreiben nach Excel
# soep_dummy |> 
#           count(jahr, region) |>
#           group_by(jahr) |> 
#           mutate(share = n/sum(n)) |> 
#           ungroup() |> 
#           pivot_wider(names_from = "region", id_cols = "jahr", values_from = "share")
# 
# 
# 
# 
