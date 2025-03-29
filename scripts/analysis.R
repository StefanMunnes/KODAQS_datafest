library(dplyr)
library(stringr)

options(scipen = 100) # get rid of exponential notation (problem as character)


# aggregate municipal data on "kreis" level (create mean/sum values forgemeinden)

data_kreis_pks_2022 <- read.csv("data/data_kreis_pks_2022.csv")

###### PREP -------------------------------------------------------------------

# problems
# change_pc_bs_mean -> value Inf!
# kreisart: RV 1 observation -> summarise?
# share_larger_HH_2022: bis effects (linear regression eclude variable?)

summary(data_kreis_pks_2022)

##### Regression ANALYSIS ------------------------------------------------------
m0 <- lm(haeufigkeitszahl ~ 1, data = data_kreis_pks_2022)
summary(m0) 

#mit kreisart
m1 <- lm(haeufigkeitszahl ~ pop20_MEAN + mean_FLAT_size_2022 + 
         lupp20_MEAN + vac_MEAN_muni_2022 + 
         Rent_m2_EUR_2022 + share_big_WHG_2022 + POP_60_plus_._2022 + kreisart , data = data_kreis_pks_2022)
summary(m1)     


#ohne kreisart
library(ggfortify)
m2 <- lm(haeufigkeitszahl ~ pop20_MEAN + mean_FLAT_size_2022 + #change_pc_bs_mean +
         lupp20_MEAN + vac_MEAN_muni_2022 + 
         Rent_m2_EUR_2022 + share_big_WHG_2022 + POP_60_plus_._2022, data = data_kreis_pks_2022)
summary(m2)     

autoplot(m2, label.size=3)




# Koeffizienten extrahieren & visualisieren
library(dotwhisker)

coef <- dwplot(m2) +
  theme_minimal() +
  ggtitle("Coefficientplot Frequency Figure for Violent Crime") +
  theme(legend.position = "none") 
ggsave("graph/coef.png", width = 8, height = 6, dpi = 300)



##### MLM Anlysis mit Cluster --------------------------------------------------
test <- lmer(haeufigkeitszahl ~ pop20_MEAN + mean_FLAT_size_2022 + #change_pc_bs_mean +
             lupp20_MEAN + vac_MEAN_muni_2022 + 
             Rent_m2_EUR_2022 + share_big_WHG_2022 + POP_60_plus_._2022 + (1 | stadt_landkreis), data = data_kreis_pks_2022)  # als multi-level Modell
summary(test)
