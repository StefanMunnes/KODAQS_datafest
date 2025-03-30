
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
