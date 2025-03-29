#matching crime and immoscout data

#PACKAGES
library(tidyverse)


# prepare violent data
data_violent <- data_pks_2020 |>
  filter(
    Schluessel == 892000,
    Gemeindeschluessel %in% data_immo_2020$kid2019
  )

# merge immo data with violent data
data_immo_violent_2020 <- data_immo_2020 |>
  left_join(data_violent, by = c("kid2019" = "Gemeindeschluessel"))




# ANALYSIS 
# research question: Which impact hat die Umgebung einer Stadt auf die Aufklärungsquote bei Gewaltkriminalität?
# aV: Aufklärungsquote
# nach Stadt und innerhalb der Stadt sind Statteile (Postleitzahlen genestet) --> MLM



