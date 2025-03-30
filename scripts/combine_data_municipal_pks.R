library(dplyr)
library(stringr)

# get rid of exponential notation (problem as character)
options(scipen = 100)

source("scripts/misc.R")


# load data
source("scripts/load_data.R")

# aggregate municipal data on "kreis" level (create mean/sum values forgemeinden)
data_municipal_aggr <- data_municipal |>
  full_join(data_taxpower_2022, by = c("AGS" = "GKZ1222")) |>
  filter(!is.na(AGS)) |>
  mutate(
    AGS_short = str_remove(as.character(AGS), "[0-9]{3,3}$") |> as.numeric(),
    .after = AGS
  ) |>
  mutate(
    change_pc_bs_mean = ifelse(
      is.infinite(change_pc_bs_mean),
      NA,
      change_pc_bs_mean
    )
  ) |>
  group_by(AGS_short) |>
  summarize(
    gemeinden_n = n(),
    across(
      c(
        pop20_MEAN,
        mean_FLAT_size_2022,
        vac_MEAN_muni_2022,
        Rent_m2_EUR_2022,
        share_larger_HH_2022,
        POP_60_plus_._2022,
        change_pc_bs_mean,
        st_einnkr,
      ),
      ~ mean(.x, na.rm = TRUE)
    )
  ) |>
  mutate(st_einnkr = top_code_income(st_einnkr))


# merge aggregated municipal data with PKS violent data (filter by Schluessel)
data_kreis_pks_2022 <- data_pks_2022 |>
  filter(schluessel == "892000") |>
  right_join(
    data_municipal_aggr,
    by = c("gemeindeschluessel" = "AGS_short")
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, digits = 4)))


write.csv(
  data_kreis_pks_2022,
  "data/data_kreis_pks_2022.csv",
  row.names = FALSE
)
