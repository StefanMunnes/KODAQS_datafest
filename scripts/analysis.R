library(dplyr)
library(stringr)

options(scipen = 100) # get rid of exponential notation (problem as character)


# aggregate municipal data on "kreis" level (create mean/sum values forgemeinden)

data_kreis_pks_2022 <- read.csv("data/data_kreis_pks_2022.csv")
data_kreis_pks_2022_cluster <- read.csv("data/data_kreis_pks_2022_cluster.csv")




##### Regression ANALYSIS ------------------------------------------------------
library(margins)
m0 <- lm(haeufigkeitszahl ~ 1, data = data_kreis_pks_2022)
summary(m0) 

#mit kreisart
m1 <- lm(haeufigkeitszahl ~ pop20_MEAN + mean_FLAT_size_2022 + 
         vac_MEAN_muni_2022 + st_einnkr + 
         Rent_m2_EUR_2022 + POP_60_plus_._2022 + kreisart , data = data_kreis_pks_2022)
summary(m1)     

ame_m1 <- summary(margins(m1))


#ohne kreisart
library(ggplot2)
library(ggfortify)
m2 <- lm(haeufigkeitszahl ~ pop20_MEAN + mean_FLAT_size_2022 + #change_pc_bs_mean +
         vac_MEAN_muni_2022 + st_einnkr + 
         Rent_m2_EUR_2022 + POP_60_plus_._2022, data = data_kreis_pks_2022)
summary(m2)     
ame_m2 <- summary(margins(m2))



ggplot(ame_m2, aes(x=AME, y= factor,xmin= lower, xmax= upper))+ 
  geom_point(position= position_dodge(0.4), size=10)+
  geom_errorbarh(position= position_dodge(0.4), height= 0.2)+
  scale_y_discrete(limits= rev(levels(ame_m2$factor)))+
  labs(y= lab_vars,x= "AMEs (DV: Skip at least 1 scenario-based question)")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position= "none",
        axis.title.x = element_text(size=15),
        axis.text.y = element_text(size = 30),
        axis.text.x= element_text(size=30))



write.csv(ame_m2, "ame_m2.csv")

#
autoplot(m2, label.size=3)




# Koeffizienten extrahieren & visualisieren
library(dotwhisker)

plot(margins(ame_m2))


coef <- dwplot(ame_m2,
               vline = geom_vline(
                 xintercept = 0,
                 colour = "grey60",
                 linetype = 2
               )) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = c(0.007, 0.01),
        variable.names=lab_vars) + 
  ggtitle("Coefficientplot Frequency Figure for Violent Crime") +
  theme(legend.position = "none") 
ggsave("graph/coef.png", width = 8, height = 6, dpi = 300)


### Regression including clusters 
table(data_kreis_pks_2022_cluster$cluster)
data_kreis_pks_2022_cluster$cluster <- as.character(data_kreis_pks_2022_cluster$cluster)

m3_0 <- lm(haeufigkeitszahl ~ cluster , data = data_kreis_pks_2022_cluster)
summary(m3_0)   

m3_1 <- lm(haeufigkeitszahl ~ pop20_MEAN + mean_FLAT_size_2022 + #change_pc_bs_mean +
           vac_MEAN_muni_2022 + st_einnkr + 
           Rent_m2_EUR_2022 + POP_60_plus_._2022 + cluster , data = data_kreis_pks_2022_cluster)

m3_2 <- lm(haeufigkeitszahl ~ pop20_MEAN + mean_FLAT_size_2022 + #change_pc_bs_mean +
             vac_MEAN_muni_2022 + st_einnkr + 
             Rent_m2_EUR_2022 + POP_60_plus_._2022 , data = data_kreis_pks_2022_cluster)

screenreg(list(m3_0,m3_2,m3_1), digits=4, custom.model.names = c("Only Cluster","Full model without clusters", "full model with cluster"), variable.names = lab_vars) 


m1_df <-
  broom::tidy(m3_0) |> filter(term != "(Intercept)") |> mutate(model = "Model 1")
m2_df <-
  broom::tidy(m3_2) |> filter(term != "(Intercept)") |> mutate(model = "Model 2")
m3_df <-
  broom::tidy(m3_1) |> filter(term != "(Intercept)") |> mutate(model = "Model 3")
three_models <- rbind(m1_df, m2_df,m3_df)

dwplot(three_models)


dwplot(m2,
               vline = geom_vline(
                 xintercept = 0,
                 colour = "grey60",
                 linetype = 2
               )) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = c(0.007, 0.01),
        names = lab_vars) + 
  ggtitle("Coefficientplot Frequency Figure for Violent Crime")
ggsave("graph/coef.png", width = 8, height = 6, dpi = 300)




