library(tidyverse)
library(haven)

(ppathl <- read_dta("data/soep_dummy/ppathl.dta"))
(pl <- read_sav("data/soep_dummy/pl.sav"))
(phealth <- read_dta("data/soep_dummy/phealth.dta"))



(soep_dummy <- left_join(ppathl, pl, by = c("persnr", "jahr")) |> 
          left_join(phealth, by = c("persnr", "jahr")) 
          )


write_dta(soep_dummy, "data/soep_dummy/soep_dummy.dta")


