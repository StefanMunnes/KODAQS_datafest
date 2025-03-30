library(dplyr)
library(cluster)
library(factoextra)


set.seed(161) # For reproducibility

source("scripts/misc.R")


data_kreis_pks_2022 <- read.csv("data/data_kreis_pks_2022.csv")

# scale chosenvariables for cluster analysis
data_scaled <- data_kreis_pks_2022 |>
  select(all_of(varlist_analysis)) |>
  scale()


# if wanted: check correlation matrix to choose variables
# source("scripts/plot_corr_variables.R")

# Choose number of clusters by visual inspection
factoextra::fviz_nbclust(data_scaled, kmeans, method = "wss")

k <- 4

km_res <- kmeans(data_scaled, centers = k, nstart = 25)


# Add cluster factor with labels back to data
data_kreis_pks_2022$cluster <- factor(
  data_kreis_pks_2022$cluster,
  labels = lab_cluster
)


# Save data for further analysis
write.csv(
  data_kreis_pks_2022,
  "data/data_kreis_pks_2022_cluster.csv",
  row.names = FALSE
)
