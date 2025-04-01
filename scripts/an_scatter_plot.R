library(dplyr)
library(ggplot2)
library(ggpubr)

# load function, labels, and colors
source("scripts/misc.R")

# load and prepare data (with clusters)
data_analysis <- read.csv("data/data_kreis_pks_2022_cluster.csv") |>
  select(haeufigkeitszahl, all_of(varlist_analysis), cluster) |>
  mutate(
    cluster = factor(
      as.character(cluster),
      levels = c("2", "3", "4", "1"),
      labels = lab_cluster
    )
  )


# function for scatter plot over different x variables
draw_scatter <- function(x_var) {
  ggplot(data = data_analysis) +
    geom_point(
      aes(
        x = .data[[x_var]],
        y = haeufigkeitszahl,
        color = cluster
      ),
      size = 2.8,
      alpha = 0.7
    ) +
    scale_color_manual(values = clr_cluster) +
    labs(
      x = lab_vars[x_var],
      y = lab_vars["haeufigkeitszahl"]
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

# draw scatter plots for four cluster/analysis variables
plots_scatter <- lapply(
  c(
    "Rent_m2_EUR_2022",
    "mean_FLAT_size_2022",
    "st_einnkr",
    "POP_60_plus_._2022"
  ),
  draw_scatter
)

ggsave(
  "output/plot_scatter_flatsize.png",
  plots_scatter[[2]],
  width = 7,
  height = 7,
  dpi = 300
)

plot_scatter_4 <- ggarrange(plotlist = plots_scatter, ncol = 2, nrow = 2)

ggsave(
  "output/plot_scatter_4.png",
  plot_scatter_4,
  width = 14,
  height = 14,
  dpi = 300,
  scale = 0.9
)
