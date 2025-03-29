#matching crime and immoscout data

#PACKAGES
library(tidyverse)
library(readr)

# load data crime pks 2020
data_pks_2020 <- read_delim(
  "data/pks/KR-F-01-T01-Kreise-Faelle-HZ_csv.csv",
  delim = ";",
  skip = 1,
  locale = locale(encoding = "Latin1")
)

violent <- data_pks_2020 %>% filter(Schluessel == 892000)

immo_p2020 <- read_delim("C:/Users/HP/Downloads/DataFest/HiDrive/panel/panel/CampusFile_HK_cities_2020.csv")

#filter collected Gemeinden
table(immo_p2020$kid2019) #gemeindeschlüssel
table(violent$Gemeindeschluessel) #gemeindeschlüssel

violent_short <- violent %>% filter(Gemeindeschluessel %in% c(2000 , 3241,4011, 5111, 5112,5113,5315,5913,6412,8111,9162, 9564, 11000, 14612, 14713))



# merge data by kid2019
violent_short <- violent_short %>%
  rename(kid2019 = Gemeindeschluessel)


immo_p2020_merged <- immo_p2020 %>%
  left_join(violent_short, by = "kid2019")




# ANALYSIS 
# research question: Which impact hat die Umgebung einer Stadt auf die Aufklärungsquote bei Gewaltkriminalität?
# aV: Aufklärungsquote
# nach Stadt und innerhalb der Stadt sind Statteile (Postleitzahlen genestet) --> MLM



