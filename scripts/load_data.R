library(readr)


data_pks <- read_delim(
  "data/pks/KR-F-01-T01-Kreise-Faelle-HZ_csv.csv",
  delim = ";",
  skip = 1
)

