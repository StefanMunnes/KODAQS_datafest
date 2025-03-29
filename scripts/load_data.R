data_municipal <- readr::read_csv("data/municipal/municipal_main.csv")

data_pks_2022 <- readr::read_delim(
  "data/pks/2022/KR-F-01-T01-Kreise-Faelle-HZ_csv.csv",
  delim = ";",
  skip = 1,
  na = c("", "NA", "------"),
  locale = locale(encoding = "Latin1")
) |>
  janitor::clean_names()


data_immo <- readr::read_csv("data/immoscout/panel/CampusFile_HK_cities.csv")

data_immo_2020 <- dplyr::filter(data_immo, stringr::str_detect(adat, "2020"))
