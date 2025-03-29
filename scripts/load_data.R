library(readr)

data_municipal <- read_csv("data/municipal/municipal_main.csv")

data_pks_2020 <- read_delim(
  "data/pks/KR-F-01-T01-Kreise-Faelle-HZ_csv.csv",
  delim = ";",
  skip = 1,
  locale = locale(encoding = "Latin1")
)

