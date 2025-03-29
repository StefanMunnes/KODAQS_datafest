top_code_income <- function(income_vector, percentile = 0.99) {
  threshold <- quantile(income_vector, probs = percentile, na.rm = TRUE)
  income_vector[income_vector > threshold] <- threshold
  return(income_vector)
}

lab_vars <- c(
  "Haeufigkeitszahl" = "Crime rate per capita",
  "pop20_MEAN" = "Mean population ",
  "mean_FLAT_size_2022" = "Flat size in m2",
  "vac_MEAN_muni_2022" = "Vacancy rate per district ",
  "Rent_m2_EUR_2022" = "Rent per m² in € ",
  "share_larger_HH_2022" = "Share of large households ",
  "POP_60plus._2022" = "Population age 60+ ",
  "St_einnkr" = "Tax revenue per capita in € ",
  "Change_pc_bs_mean" = "Built-up area change in % (2005–2020)"
)

lab_cluster <- c()


colors <- sapply(
  RColorBrewer::brewer.pal(k, "Set1"),
  function(col) {
    rgb_val <- col2rgb(col)
    rgb(
      rgb_val[1],
      rgb_val[2],
      rgb_val[3],
      maxColorValue = 255,
      alpha = 0.8 * 255
    )
  },
  USE.NAMES = FALSE
)
