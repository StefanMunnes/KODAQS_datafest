##### packages -----------------------------------------------------------------
library(dplyr)
library(stringr)
library(broom)
library(ggplot2)
library(ggfortify)
library(pandoc)
library(modelsummary)
library(texreg)
library(flextable)

##### options
options(scipen = 100) # get rid of exponential notation (problem as character)

##### load data  ---------------------------------------------------------------
data_kreis_pks_2022 <- read.csv("data/data_kreis_pks_2022.csv")
data_kreis_pks_2022_cluster <- read.csv("data/data_kreis_pks_2022_cluster.csv")


##### Regression analysis without clusters -------------------------------------
m0 <- lm(haeufigkeitszahl ~ 1, data = data_kreis_pks_2022)
summary(m0) 

m1 <- lm(haeufigkeitszahl ~ 
         pop20_MEAN + 
         mean_FLAT_size_2022 +
         vac_MEAN_muni_2022 + 
         st_einnkr + 
         Rent_m2_EUR_2022 + 
         POP_60_plus_._2022, 
         data = data_kreis_pks_2022)
summary(m1)

# regression assumptions
autoplot(m1, label.size=3)

# Extract & visualise coefficients of model m2
coef_m1 <- tidy(m1) %>%
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
ggsave("graph/coef_m1.png", width = 8, height = 6, dpi = 300)





##### Regressions including clusters  ------------------------------------------
table(data_kreis_pks_2022_cluster$cluster)
data_kreis_pks_2022_cluster$cluster <- as.character(data_kreis_pks_2022_cluster$cluster)

m2_0 <- lm(haeufigkeitszahl ~ 
           cluster,
           data = data_kreis_pks_2022_cluster)
summary(m2_0)   

m2_1 <- lm(haeufigkeitszahl ~ 
           pop20_MEAN +
           mean_FLAT_size_2022 +
           vac_MEAN_muni_2022 + 
           st_einnkr + 
           Rent_m2_EUR_2022 + 
           POP_60_plus_._2022, 
           data = data_kreis_pks_2022_cluster)

m2_2 <- lm(haeufigkeitszahl ~
           pop20_MEAN +
           mean_FLAT_size_2022 +
           vac_MEAN_muni_2022 + 
           st_einnkr + 
           Rent_m2_EUR_2022 + 
           POP_60_plus_._2022 +
           cluster,
           data = data_kreis_pks_2022_cluster)

modelsummary(list(m2_0,m2_1,m2_2), 
          digits=4) 


#
model_data <- bind_rows(
  tidy(m2_0) %>% mutate(Model = "model1"),
  tidy(m2_1) %>% mutate(Model = "model2"),
  tidy(m2_2) %>% mutate(Model = "model3")) %>%
  filter(term != "(Intercept)")  # Intercept ausblenden

coef_m2 <- ggplot(model_data, aes(x = estimate, y = reorder(term, estimate), color = Model)) +
  geom_point(size = 3, shape= 16, position = position_dodge(width = 0.5)) +
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
  labs(title = "Coefficient Plot Regression Analysis", x = "estimates (95% CI)", y = "predictors") +
  theme_minimal()
ggsave("graph/coef_m2.png", width = 8, height = 6, dpi = 300)




#Regressionsoutput Word
model <- list("Only Cluster" = m2_0,
              "Full model\nwithout clusters" = m2_1,
              "Full model\nwith cluster" = m2_2)

lab_vars <- c(
  "(Intercept)" = "Intercept",
  "pop20_MEAN" = "Mean population ",
  "mean_FLAT_size_2022" = "Flat size in m2",
  "vac_MEAN_muni_2022" = "Vacancy rate per district ",
  "Rent_m2_EUR_2022" = "Rent per m² in € ",
  "POP_60_plus_._2022" = "Population age 60+ ",
  "st_einnkr" = "Tax revenue per capita in € ",
  "cluster1" = "Cluster 1",
  "cluster2" = "Cluster 2: Dense, aging,\ncostly, shrinking housing",
  "cluster3" = "Cluster 3: Growing,\nspacious, rural family areas",
  "cluster4" = "Cluster 4: Wealthy,\nyoung, dense urban cores"
)

regtab <- modelsummary(model, 
             coef_map = lab_vars,
             output = "flextable",
             gof_omit = "IC|F|L",stars = T)
regtab2 <- autofit(regtab)
save_as_docx(regtab2,path = "tables/regressionstabelle.docx")

