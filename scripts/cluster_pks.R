library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(gridExtra)
library(cowplot)
library(GGally)
library(sf)

set.seed(161) # For reproducibility

data_kreis_pks_2022 <- read.csv("data/data_kreis_pks_2022.csv")

data_analysis <- data_kreis_pks_2022 |>
  select(
    change_pc_bs_mean,
    vac_MEAN_muni_2022,
    Rent_m2_EUR_2022,
    share_larger_HH_2022,
    mean_FLAT_size_2022,
    st_einnkr,
    POP_60_plus_._2022,
    pop20_MEAN
  )


data_scaled <- scale(data_analysis)


# Calculate correlation matrix
cor_matrix <- cor(data_scaled, use = "pairwise.complete.obs")


# Melt the correlation matrix into long format
melted_cor <- reshape2::melt(cor_matrix)

# Plot heatmap using ggplot2
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1),
    name = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)
  ) +
  coord_fixed()


# Choose number of clusters
factoextra::fviz_nbclust(data_scaled, kmeans, method = "wss")

k <- 4

km_res <- kmeans(data_scaled, centers = k, nstart = 25)

# Add cluster labels back to your spatial data
data_kreis_pks_2022$cluster <- as.factor(km_res$cluster)

write.csv(
  data_kreis_pks_2022,
  "data/data_kreis_pks_2022_cluster.csv",
  row.names = FALSE
)


data_analysis$cluster <- as.factor(km_res$cluster)

data_analysis |>
  group_by(cluster) |>
  summarise(across(everything(), mean, na.rm = TRUE))

cluster_means <- data_analysis |>
  mutate_at(vars(-cluster), ~ scale(.)[, 1]) |>
  group_by(cluster) |>
  summarize_all(mean) |>
  tidyr::pivot_longer(-cluster, names_to = "variable", values_to = "value")


# ---- 1. Cluster by variables ----
data_analysis |>
  mutate(
    change_pc_bs_mean = ifelse(change_pc_bs_mean >= 25, 25, change_pc_bs_mean)
  ) |>
  ggparcoord(
    columns = 1:(ncol(data_analysis) - 1),
    groupColumn = "cluster",
    scale = "std",
    alphaLines = 0.15
  ) +
  ungeviz::geom_hpline(
    data = cluster_means,
    aes(x = variable, y = value, colour = cluster),
    alpha = 0.8,
    size = 3,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, color = "grey20") +
  coord_flip() +
  scale_x_discrete(labels = lab_vars) +
  scale_color_manual(values = clr_cluster) +
  labs(
    y = "Scaled values",
    x = "",
    title = "Cluster by variables",
    color = "Cluster"
  ) +
  theme_minimal()


# ---- 1. Map ----

# prepare the shapefile
data_map <- data_shapefile |>
  filter(AGS != "09672") |> # remove Bad Kissing because of missing data
  mutate(AGS = as.numeric(AGS), cluster = km_res$cluster)


# Plot the map
ggplot(data_map) +
  geom_sf(aes(fill = as.factor(cluster)), color = "#747474", size = 0.02) +
  scale_fill_manual(values = clr_cluster, na.value = "grey50") +
  theme_minimal() +
  labs(
    title = "Cluster for Municipalities in Germany",
    fill = "Cluster"
  )
