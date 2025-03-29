library(dplyr)
library(corrplot)
library(cluster)
library(factoextra)
library(tmap)
library(GGally)

set.seed(161) # For reproducibility


data_analysis <- data_kreis_pks_2022 |>
  select(
    # haeufigkeitszahl,
    pop20_MEAN,
    mean_FLAT_size_2022,
    vac_MEAN_muni_2022,
    Rent_m2_EUR_2022,
    share_larger_HH_2022,
    POP_60_plus_._2022,
    st_einnkr,
    change_pc_bs_mean
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
  summarise(across(everything(), mean, na.rm = TRUE)) |>
  print()

colors <- RColorBrewer::brewer.pal(k, "Set1")

GGally::ggparcoord(
  data = data_analysis,
  columns = 1:(ncol(data_analysis) - 1),
  groupColumn = "cluster",
  scale = "std",
  alphaLines = 0.5
) +
  scale_color_manual(values = colors) +
  theme_minimal()
