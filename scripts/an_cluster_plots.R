library(dplyr)
library(ggplot2)
library(sf)
library(gridExtra)
library(cowplot)
library(GGally)


source("scripts/misc.R")

# load data
source("scripts/load_data.R")

# prepare analysis data
data_cluster <- read.csv("data/data_kreis_pks_2022_cluster.csv") |>
  select(all_of(varlist_analysis), cluster) |>
  mutate(
    cluster = factor(
      as.character(cluster),
      levels = c("2", "3", "4", "1"),
      labels = lab_cluster
    )
  )


# ---- 1. Map ----

# prepare the shapefile
data_map <- data_shapefile |>
  mutate(
    AGS = as.numeric(AGS),
    cluster = data_cluster$cluster
  )

# Plot the map
plot_map_cluster <- ggplot(data_map) +
  geom_sf(aes(fill = cluster), color = "#747474", size = 0.02) +
  scale_fill_manual(values = clr_cluster, na.value = "grey50") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  "output/plot_cluster_map.png",
  plot_map_cluster,
  width = 10,
  height = 10,
  dpi = 300
)


# ---- 2. Cluster by variables ----

# calculate cluster means for analysis variables
cluster_means <- data_cluster |>
  mutate_at(vars(-cluster), ~ scale(.)[, 1]) |>
  group_by(cluster) |>
  summarize_all(mean) |>
  tidyr::pivot_longer(-cluster, names_to = "variable", values_to = "value")


plot_cluster_vars <- data_cluster |>
  # top-code variable for better visualization
  mutate(
    change_pc_bs_mean = ifelse(change_pc_bs_mean >= 25, 25, change_pc_bs_mean)
  ) |>
  ggparcoord(
    columns = 1:(ncol(data_cluster) - 1),
    groupColumn = "cluster",
    scale = "std",
    alphaLines = 0.25
  ) +
  geom_hline(yintercept = 0, color = "grey20") +
  ungeviz::geom_hpline(
    data = cluster_means,
    aes(x = variable, y = value, colour = cluster),
    alpha = 0.9,
    size = 3,
    inherit.aes = FALSE
  ) +
  coord_flip() +
  scale_x_discrete(labels = lab_vars) +
  scale_color_manual(values = clr_cluster) +
  labs(
    y = "Scaled values",
    x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(size = 12))


ggsave(
  "output/plot_cluster_vars.png",
  plot_cluster_vars,
  width = 7,
  height = 6,
  dpi = 300
)


# ---- 3. Legend ----

legend <- get_legend(
  ggplot(data_cluster, aes(st_einnkr, fill = cluster)) +
    geom_bar() +
    scale_fill_manual(values = clr_cluster) +
    theme(legend.position = "right", legend.title = element_blank())
)

ggsave(
  "output/plot_cluster_legend.png",
  legend,
  width = 12,
  height = 6,
  dpi = 300
)

# ---- 4. Combine plots and with legend ----
