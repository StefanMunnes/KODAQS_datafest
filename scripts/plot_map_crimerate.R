library(dplyr)
library(ggplot2)


data_crime_rate <- read.csv("data/data_kreis_pks_2022_cluster.csv") |>
  pull(haeufigkeitszahl)

data_map_crimerate <- data_shapefile |>
  mutate(
    AGS = as.numeric(AGS),
    crime_rate = data_crime_rate
  )

# Plot the map
plot_map_crimerate <- ggplot(data_map_crimerate) +
  geom_sf(aes(fill = crime_rate), color = "#747474", size = 0.02) +
  scale_fill_gradient(low = "#c2d9fc", high = "#2f5a9e") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Crime rate")) +
  theme(legend.position = "right")

ggsave(
  "output/plot_map_crimerate.png",
  plot_map_crimerate,
  width = 10,
  height = 10,
  dpi = 300
)
