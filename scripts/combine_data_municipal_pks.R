library(dplyr)
library(stringr)

options(scipen = 100) # get rid of exponential notation (problem as character)

# aggregate municipal data on "kreis" level (create mean/sum values forgemeinden)
data_municipal_short <- data_municipal |>
  filter(!is.na(AGS)) |>
  mutate(
    AGS_short = str_remove(as.character(AGS), "[0-9]{3,3}$") |> as.numeric(),
    .after = AGS
  ) |>
  group_by(AGS_short) |>
  summarize(
    gemeinden_n = n(),
    across(
      c(
        pop20_MEDIAN,
        bs20_MEAN,
        lupp20_MEAN,
        mean_FLAT_size_2022,
        livespace_MEAN_grid_2022,
        vac_MEAN_muni_2022,
        Rent_m2_EUR_2022,
        share_larger_HH_2022,
        share_big_WHG_2022,
        POP_60_plus_._2022,
        Tax_pp_2020,
        change_pc_bs_median,
        change_pc_flat_size,
        change_pc_livespace_pp,
        change_pc_income_tax_pp,
      ),
      ~ mean(.x, na.rm = TRUE)
    ),
    across(
      pop_20,
      ~ sum(.x)
    )
  )

# prepate pks criminal data (keep just violent crime)
data_pks_2020 <- data_pks_2020 |>
  filter(Schluessel == "892000") 


data_kreis_pks_2020 <- data_pks_2020 |>
  semi_join(data_municipal_short, by = c("Gemeindeschluessel" = "AGS_short"))
