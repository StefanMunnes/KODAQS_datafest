library(dplyr)
library(ggplot2)


data_analysis <- read.csv("data/data_kreis_pks_2022_cluster.csv") |>
  select(haeufigkeitszahl, all_of(varlist_analysis), cluster) |>
  mutate(
    cluster = factor(
      as.character(cluster),
      levels = c("2", "3", "4", "1"),
      labels = lab_cluster
    )
  )


plot_scatter <- ggplot(data = data_analysis) +
  geom_point(
    aes(
      x = mean_FLAT_size_2022,
      y = haeufigkeitszahl,
      color = cluster
    ),
    size = 3,
    alpha = 0.8
  ) +
  scale_color_manual(values = clr_cluster) +
  labs(
    y = "Crime rate / 100'000 people",
    x = "Flat size (m2)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


ggsave(
  "output/plot_scatter.png",
  plot_scatter,
  width = 7,
  height = 7,
  dpi = 300
)
