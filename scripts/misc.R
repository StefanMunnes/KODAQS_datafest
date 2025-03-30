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
  "Haeufigkeitszahl" = "Crime rate per capita",
  "pop20_MEAN" = "Mean population ",
  "mean_FLAT_size_2022" = "Flat size in m2",
  "vac_MEAN_muni_2022" = "Vacancy rate per district ",
  "Rent_m2_EUR_2022" = "Rent per m² in € ",
  "share_larger_HH_2022" = "Share of large households ",
  "POP_60_plus_._2022" = "Population age 60+ ",
  "st_einnkr" = "Tax revenue per capita in € ",
  "change_pc_bs_mean" = "Built-up area change in % ('05–'20)"
)

lab_cluster <- c(
  "2" = "Poor, aging, vacant, low-density areas",
  "3" = "Growing, spacious, rural family areas",
  "4" = "Dense, aging, costly, shrinking housing",
  "1" = "Wealthy, young, dense urban cores"
)


# define colors for cluster visualization
# clr_cluster <- c(
#   "#A9A9A9",
#   "#1E90FF",
#   "#32CD32",
#   "#FF8C00"
# )

# clr_cluster <- c(
#   "#696969",
#   "#4682B4",
#   "#228B22",
#   "#B22222"
# )

clr_cluster <- c(
  "Poor, aging, vacant, low-density areas" = "#FBCA7B",
  "Dense, aging, costly, shrinking housing" = "#A1C6EA",
  "Growing, spacious, rural family areas" = "#B4E7B0",
  "Wealthy, young, dense urban cores" = "#F4A7B9"
)

# clr_cluster <- c(
#   "#FBCA7B",
#   "#F4A7B9",
#   "#A1C6EA",
#   "#B4E7B0"
# )
