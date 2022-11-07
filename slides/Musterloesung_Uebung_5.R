#Übung 5 - Deskriptive Statistiken
#Autor: Pekka Sagner

#Descriptives mit dem SOEP-Dummy


#a)
library(tidyverse)
#install.packages("haven")
library(haven)

#Daten einlesen
(soep_dummy <- read_dta("data/soep_dummy/soep_dummy.dta") %>% 
                    mutate(region = as.factor(region)) #hier schon as.factor() angepasst
)

# b)
#1 Summary
summary(soep_dummy)
#Hieraus könnte man schließen, dass "region" schlecht definiert ist --> mögliche Lösung mit as.factor()

glimpse(soep_dummy)

View(soep_dummy)

# c)
#Summary-Statistics berechnen:
#durschn. Einkommen.
soep_dummy %>% 
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

#durschn. Einkommen pro Jahr
soep_dummy %>% 
          group_by(jahr) %>% 
          summarise(mean_income = mean(income)) %>% 
          ungroup()


#durschn. Einkommen pro Jahr und Region
soep_dummy %>% 
          group_by(jahr, region) %>% 
          summarise(mean_income = mean(income)) %>% 
          ungroup()


#d) 
#Gewichtete Statistiken bestimmen
#install.packages(spatstat.geom)
library(spatstat.geom)

#Summary-Statistics berechnen:
soep_dummy %>% 
          summarise(mean_income = weighted.mean(income, w = weight))


soep_dummy %>% 
          group_by(jahr) %>% 
          summarise(mean_income = weighted.mean(income, w = weight)) %>% 
          ungroup()

soep_dummy %>% 
          group_by(jahr, region) %>% 
          summarise(mean_income = weighted.mean(income, w = weight)) %>% 
          ungroup()



# e)
#Summary Statistics mit across()
soep_dummy %>% 
          summarise(across(.cols = c(income, bildung, anz_kind), 
                           .fns = mean, 
                           na.rm = T))

#f)
#Anwendung mehrerer Funktionen auf alle numerischen Variablen:
soep_dummy %>% 
          summarise(across(.cols = where(is.numeric), 
                           .fns = list(median = median, sd = sd), 
                           na.rm = T, 
                           .names = "{.fn}_{.col}"))


#Aufgabe 2
# a)
soep_dummy %>% 
          filter(jahr == 2002) %>% 
          count(sex) %>% 
          mutate(share = n/sum(n))

#b)
soep_dummy %>% 
          count(sex, jahr) %>%
          group_by(jahr) %>% 
          mutate(share = n/sum(n)) %>% 
          ungroup()

#c) mit Gewichtung
soep_dummy %>% 
          filter(jahr == 2002) %>% 
          count(sex, wt = weight) %>% 
          mutate(share = n/sum(n))

soep_dummy %>% 
          count(sex, jahr, wt = weight) %>%
          group_by(jahr) %>% 
          mutate(share = n/sum(n)) %>% 
          ungroup()

#d)

soep_dummy %>% 
          count(jahr, eink_bed = income > 10000, wt = weight) %>% 
          group_by(jahr) %>% 
          mutate(share = n/sum(n)) %>% 
          ungroup()

#überführen in Kreuztabelle und Speichern in Excel
(speich <- soep_dummy %>% 
          count(jahr, eink_bed = income > 10000, wt = weight) %>% 
          group_by(jahr) %>% 
          mutate(share = n/sum(n)) %>% 
          ungroup() %>% 
          select(-n) %>%
          pivot_wider(names_from = jahr,
                      values_from = share,
                      id_cols = eink_bed) 
)

#e)
xlsx::write.xlsx(x = speich, file = "hallo.xlsx")

