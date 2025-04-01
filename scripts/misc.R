# function to top-code income variable
top_code_income <- function(income_vector, percentile = 0.99) {
  threshold <- quantile(income_vector, probs = percentile, na.rm = TRUE)
  income_vector[income_vector > threshold] <- threshold
  return(income_vector)
}


# list of variables for analysis
varlist_analysis <- c(
  "change_pc_bs_mean",
  "vac_MEAN_muni_2022",
  "Rent_m2_EUR_2022",
  "share_larger_HH_2022",
  "mean_FLAT_size_2022",
  "st_einnkr",
  "POP_60_plus_._2022",
  "pop20_MEAN"
)


# add labels vor variables and clusters
lab_vars <- c(
  "Haeufigkeitszahl" = "Crime rate per 10'000 capita",
  "pop20_MEAN" = "Population density",
  "mean_FLAT_size_2022" = "Flat size in m²",
  "vac_MEAN_muni_2022" = "Vacancy rate",
  "Rent_m2_EUR_2022" = "Rent per m² in €",
  "share_larger_HH_2022" = "Share of large households",
  "POP_60_plus_._2022" = "Share of population age 60+",
  "st_einnkr" = "Tax revenue per capita in €",
  "change_pc_bs_mean" = "Built-up area change in % ('05–'20)"
)

lab_cluster <- c(
  "2" = "Poor, aging, vacant, low-density areas",
  "3" = "Growing, spacious, rural family areas",
  "4" = "Dense, aging, costly, shrinking housing",
  "1" = "Wealthy, young, dense urban cores"
)


# define colors for cluster visualization
clr_cluster <- c(
  "#FBCA7B",
  "#A1C6EA",
  "#B4E7B0",
  "#F4A7B9"
)

names(clr_cluster) <- lab_cluster
