#Descriptives mit dem SOEP-Dummy

library(tidyverse)
#install.packages("haven")
library(haven)

#Daten einlesen
(soep_dummy <- read_dta("data/soep_dummy/soep_dummy.dta") |> 
                    mutate(region = as.factor(region))
)


#1 Summary
summary(soep_dummy)
#Hieraus könnte man schließen, dass "region" schlecht definiert ist --> mögliche Lösung mit as.factor()

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


#Anwendung auf alle numerischen Variablen:
soep_dummy |> 
          summarise(across(.cols = where(is.numeric), .fns = mean, na.rm = T))

#Anwendung zweier Funktionen auf eine Variable
soep_dummy |> 
          summarise(mean_income = mean(income), sd_income = sd(income))


#Anwendung mehrerer Funktionen auf alle numerischen Variablen:
soep_dummy |> 
          summarise(across(.cols = where(is.numeric), 
                           .fns = list(mean = mean, sd = sd), 
                           na.rm = T, 
                           .names = "{.fn}_{.col}"))

#Häufigkeiten mit count()
soep_dummy |> 
          count(jahr)

#Anteile mit count() und sum()
soep_dummy |> 
          count(jahr) |> 
          mutate(total = sum(n)) |> 
          mutate(share = n/total) |> 
          mutate(share_2 = n/sum(n))

#Kreuztabellen
soep_dummy |> 
          count(jahr, region) |> #Übergang zu besser lesbarer Tabelle --> vorsicht, nicht mehr tidy!
          pivot_wider(names_from = "region", values_from = "n", id_cols = "jahr")

#Kreuztabellen mit Anteilen
soep_dummy |> 
          count(jahr, region) |>
          group_by(jahr) |> 
          mutate(share = n/sum(n)) |> 
          ungroup()
          
#Kreuztabellen mit Anteilen und besser lesbar/bzw. zum rausschreiben nach Excel
soep_dummy |> 
          count(jahr, region) |>
          group_by(jahr) |> 
          mutate(share = n/sum(n)) |> 
          ungroup() |> 
          pivot_wider(names_from = "region", id_cols = "jahr", values_from = "share")




