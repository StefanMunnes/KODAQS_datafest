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

data_immo <- read_delim("C:/Users/HP/Downloads/DataFest/HiDrive/panel/panel/CampusFile_HK_cities_2020.csv", del = ";")

data_immo_2020 <- filter(data_immo, stringr::str_detect(adat, "2020"))

#filter collected Gemeinden
table(data_immo_2020$kid2019) #gemeindeschlüssel
table(violent$Gemeindeschluessel) #gemeindeschlüssel

violent_short <- violent %>% filter(Gemeindeschluessel %in% c(2000, 3241,4011, 5111, 5112,5113,5315,5913,6412,8111,9162, 9564, 11000, 14612, 14713))



# merge data by kid2019
violent_short <- violent_short %>%
  rename(kid2019 = Gemeindeschluessel)

immo_merged <- data_immo_2020 %>%
  left_join(violent_short, by = "kid2019")




#### PREP
#gemeindeschlüssel
immo_merged$kid2019 <- as.character(immo_merged$kid2019)
summary(immo_merged$kid2019)

immo_merged %>% group_by(kid2019, plz) %>% summarise(n=n())

#buajahr
immo_merged %>% group_by(kid2019, plz) %>% 
  summarise(year = mean(baujahr, na.rm = TRUE)) 

summary(immo_merged$baujahr)

#objektzustand
class(immo_merged$objektzustand)
table(immo_merged$objektzustand)
# immo_merged$objektzustand[immo_merged$objektzustand == "Not specified"] <- "NA"
# immo_merged$objektzustand[immo_merged$objektzustand == "Dilapidated"] <- "1"
# immo_merged$objektzustand[immo_merged$objektzustand == "First occupancy"] <- "2"
# immo_merged$objektzustand[immo_merged$objektzustand == "Needs renovation"] <- "3"
# 
# immo_merged$objektzustand[immo_merged$objektzustand == "By arrangement"] <- "4"
# immo_merged$objektzustand[immo_merged$objektzustand == "Completely renovated"] <- "5"
# immo_merged$objektzustand[immo_merged$objektzustand == "First occupancy after reconstruction"] <- "6"
# immo_merged$objektzustand[immo_merged$objektzustand == "Like new"] <- "7"
# immo_merged$objektzustand[immo_merged$objektzustand == "Modernised"] <- "8"
# immo_merged$objektzustand[immo_merged$objektzustand == "Reconstructed"] <- "9"
# immo_merged$objektzustand[immo_merged$objektzustand == "Well-kept"] <- "10"

#immo_merged$objektzustand <- as.numeric(immo_merged$objektzustand)
immo_merged<- immo_merged %>%
  mutate(zustand_old_d = ifelse(objektzustand %in% c("Dilapidated", "First occupancy", "Needs renovation"), 1, 0))
class(immo_merged$zustand_old_d)

immo_merged$zustand_old_d <- as.numeric(immo_merged$zustand_old_d)

zustandobjekt <- immo_merged %>% 
  group_by(plz, zustand_old_d) %>% 
  summarise(n = n(),) %>%
  mutate(p = paste0(round(100 * n/sum(n), 2))) %>% 
  filter(zustand_old_d =="1") %>%
  rename(zustand_old_p = p) %>%
  select(-c(zustand_old_d, n))
immo_merged <- immo_merged %>%
  left_join(zustandobjekt, by="plz")

immo_merged$zustand_old_p <- as.numeric(immo_merged$zustand_old_p)
immo_merged <- immo_merged %>%  
  mutate(zustand_old_p=coalesce(zustand_old_p,0))


#To Do: Varianz innerhalb der Stadt zeigen




#letzte modernisierung
class(immo_merged$letzte_modernisierung)
table(immo_merged$letzte_modernisierung)


### kaufpreis ------------------------------------------------------------------
class(immo_merged$kaufpreis)
immo_merged %>% count(kaufpreis)

summary(immo_merged$kaufpreis)

# detect outlier
m1 <- lm(Aufklaerungsquote ~ kaufpreis, data = immo_merged)
summary(m1)

ggplot(data = immo_merged, aes(y = Aufklaerungsquote, x = kaufpreis)) +
  theme_minimal() + # schwarz/weiß für Hintergrund
  geom_point(color = "navy") + # Scatterplot mit blauen Punkten
  geom_smooth(method = "lm", se =F) + # Konfidenzintervall nicht einzeichnen
  xlab("Kaufpreis") + # x Achse beschriften
  ylab("Aufklaerungsquote") + # y Achse beschriften
  theme(aspect.ratio = .8 ) # Höhe/Breite-Verhältnis

immo_merged$kaufpreis[immo_merged$kaufpreis==159898988] <- NA
immo_merged$kaufpreis[immo_merged$kaufpreis==111111113] <- NA
immo_merged$kaufpreis[immo_merged$kaufpreis==74624225] <- NA

summary(immo_merged$kaufpreis)

mergdata <- immo_merged %>% group_by(plz) %>% 
  summarise(mittelwert_preis = mean(kaufpreis, na.rm = TRUE)) 

#to do mergen
immo_merged <- immo_merged %>%
  left_join(mergdata, by="plz")





### baujahr  -------------------------------------------------------------------
summary(immo_merged$baujahr_kat)
immo_merged %>% count(baujahr_kat)

immo_merged <- immo_merged %>%
  mutate(baujahr_kat = case_when(
    baujahr < 1949 ~ "Altbau",
    baujahr >= 1949 & baujahr <= 1977 ~ "Nachkriegsbau",
    baujahr >= 1978 & baujahr <= 1994 ~ "Standardbau",
    baujahr >= 1995 ~ "Neubau",
    TRUE ~ NA_character_ 
  ))

prop_neubau_plz <- immo_merged %>% 
  group_by(plz, baujahr_kat) %>% 
  summarise(n = n(),) %>%
  mutate(p = paste0(round(100 * n/sum(n), 2))) %>%
  filter(baujahr_kat =="Neubau") %>%
  rename(baujahr_kat_p = p) %>%
  select(-c(baujahr_kat, n))

immo_merged <- immo_merged %>%
  left_join(prop_neubau_plz, by="plz")   #merge variable prop_neubau_plz

class(immo_merged$baujahr_kat_p)
immo_merged$baujahr_kat_p <- as.numeric(immo_merged$baujahr_kat_p)


# prep data for analysis
df_finish <- immo_merged %>%
  select(obid, plz, kaufpreis,zustand_old_p,letzte_modernisierung, baujahr_kat_p, mittelwert_preis, Aufklaerungsquote)


# ANALYSIS 
# research question: Which impact hat die Umgebung einer Stadt auf die Aufklärungsquote bei Gewaltkriminalität?
# aV: Aufklärungsquote
# nach Stadt und innerhalb der Stadt sind Statteile (Postleitzahlen genestet) --> MLM

m1 <- lm(Aufklaerungsquote ~ kaufpreis + zustand_old_p + baujahr_kat_p + mittelwert_preis, data = df_finish)
summary(m1)

test <- lmer(Aufklaerungsquote ~ kaufpreis + zustand_old_p + baujahr_kat_p + (1 | kid2019), data = immo_merged)  # als multi-level Modell

