##### packages -----------------------------------------------------------------
library(dplyr)
library(stringr)
library(broom)
library(ggplot2)
library(ggfortify)

##### options
options(scipen = 100) # get rid of exponential notation (problem as character)

##### load data  ---------------------------------------------------------------
data_kreis_pks_2022 <- read.csv("data/data_kreis_pks_2022.csv")
data_kreis_pks_2022_cluster <- read.csv("data/data_kreis_pks_2022_cluster.csv")


##### Regression ANALYSIS ------------------------------------------------------
m0 <- lm(haeufigkeitszahl ~ 1, data = data_kreis_pks_2022)
summary(m0) 

#mit kreisart
m1 <- lm(haeufigkeitszahl ~
         pop20_MEAN + 
         mean_FLAT_size_2022 + 
         vac_MEAN_muni_2022 + st_einnkr + 
         Rent_m2_EUR_2022 + 
         POP_60_plus_._2022 + 
         kreisart , data = data_kreis_pks_2022)
summary(m1)     

#ohne kreisart
m2 <- lm(haeufigkeitszahl ~ pop20_MEAN + mean_FLAT_size_2022 + #change_pc_bs_mean +
         vac_MEAN_muni_2022 + st_einnkr + 
         Rent_m2_EUR_2022 + POP_60_plus_._2022, data = data_kreis_pks_2022)
summary(m2)

# regression assumptions
autoplot(m2, label.size=3)

# Extract & visualise coefficients of model m2
coef_m2 <- tidy(m2) %>%
  filter(term != "(Intercept)") %>%  # Intercept ausblenden
  ggplot(aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(size = 3, color = "0070C0", shape= 16) +  
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, 
                     xmax = estimate + 1.96 * std.error), 
                 height = 0.2, color = "black") +
  geom_vline(xintercept= 0, linetype = "dotted", color = "dark grey")+
  scale_y_discrete(labels = c("mean_FLAT_size_2022" = "flatsize 22 (mean)",
                              "POP_60_plus_._2022"= "Old population",
                              "pop20_MEAN" = "pop_20 (mean)",
                              "Rent_m2_EUR_2022" = "Rent/m2 22 (€)",
                              "st_einnkr" = "tax/capita 22 (€)",
                              "vac_MEAN_muni_2022" = "vacancy rate/municipality (mean)"))+
  labs(title = "Coefficient Plot", x = "estimates (95% CI)", y = "predictors") +
  theme_minimal()

ggsave("graph/coef_m2.png", width = 8, height = 6, dpi = 300)





##### Regression including clusters  -------------------------------------------
table(data_kreis_pks_2022_cluster$cluster)
data_kreis_pks_2022_cluster$cluster <- as.character(data_kreis_pks_2022_cluster$cluster)

m3_0 <- lm(haeufigkeitszahl ~ cluster , data = data_kreis_pks_2022_cluster)
summary(m3_0)   

m3_1 <- lm(haeufigkeitszahl ~ pop20_MEAN + mean_FLAT_size_2022 +
             vac_MEAN_muni_2022 + st_einnkr + 
             Rent_m2_EUR_2022 + POP_60_plus_._2022 , data = data_kreis_pks_2022_cluster)

m3_2 <- lm(haeufigkeitszahl ~ pop20_MEAN + mean_FLAT_size_2022 +
           vac_MEAN_muni_2022 + st_einnkr + 
           Rent_m2_EUR_2022 + POP_60_plus_._2022 + cluster , data = data_kreis_pks_2022_cluster)


library(texreg)
screenreg(list(m3_0,m3_1,m3_2), 
          digits=4, 
          custom.model.names = c("Only Cluster","Full model without clusters", "full model with cluster"), 
          var.names = lab_vars) 


#
model_data <- bind_rows(
  tidy(m3_0) %>% mutate(Model = "m3_0"),
  tidy(m3_1) %>% mutate(Model = "m3_1"),
  tidy(m3_2) %>% mutate(Model = "m3_2")) %>%
  filter(term != "(Intercept)")  # Intercept ausblenden


# m1_df <-
#   broom::tidy(m3_0) |> filter(term != "(Intercept)") |> mutate(model = "Model 1")
# m2_df <-
#   broom::tidy(m3_1) |> filter(term != "(Intercept)") |> mutate(model = "Model 2")
# m3_df <-
#   broom::tidy(m3_2) |> filter(term != "(Intercept)") |> mutate(model = "Model 3")
# three_models <- rbind(m1_df, m2_df,m3_df)
# 
# dwplot(three_models)
# 
# tidy(m2) %>%
#   filter(term != "(Intercept)") %>%  # Intercept ausblenden
ggplot(model_data, aes(x = estimate, y = reorder(term, estimate), color = Model)) +
  geom_point(size = 3, color = "0070C0", shape= 16, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, 
                     xmax = estimate + 1.96 * std.error), 
                 height = 0.2, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept= 0, linetype = "dotted", color = "dark grey")+
  scale_y_discrete(labels = c("mean_FLAT_size_2022" = "flatsize 22 (mean)",
                              "POP_60_plus_._2022"= "Old population",
                              "pop20_MEAN" = "pop_20 (mean)",
                              "Rent_m2_EUR_2022" = "Rent/m2 22 (€)",
                              "st_einnkr" = "tax/capita 22 (€)",
                              "vac_MEAN_muni_2022" = "vacancy rate/municipality (mean)"))+
  labs(title = "Coefficient Plot", x = "estimates (95% CI)", y = "predictors") +
  theme_minimal()




#Regressionsoutput Word
model <- list(m3_0,m3_1,m3_2)


# Word-Datei speichern
library(pandoc)
library(modelsummary)

lab_vars <- c(
  "(Intercept)" = "Intercept",
  "pop20_MEAN" = "Mean population ",
  "mean_FLAT_size_2022" = "Flat size in m2",
  "vac_MEAN_muni_2022" = "Vacancy rate per district ",
  "Rent_m2_EUR_2022" = "Rent per m² in € ",
  "POP_60_plus_._2022" = "Population age 60+ ",
  "st_einnkr" = "Tax revenue per capita in € ",
  "cluster" = "Cluster"
)

modelsummary(model, coef_map = lab_vars,
             output = "tables/regressionstabelle.docx")
