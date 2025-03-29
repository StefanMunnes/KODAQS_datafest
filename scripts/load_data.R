library(readr)

data_municipal <- read_csv("data/municipal/municipal_main.csv")

data_pks_2020 <- read_delim(
  "data/pks/KR-F-01-T01-Kreise-Faelle-HZ_csv.csv",
  delim = ";",
  skip = 1,
  na = c("", "NA", "------"),
  locale = locale(encoding = "Latin1")
)


data_immo <- read_csv("data/immoscout/panel/CampusFile_HK_cities.csv")

data_immo_2020 <- filter(data_immo, stringr::str_detect(adat, "2020"))
