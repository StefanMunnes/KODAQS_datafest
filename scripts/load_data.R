# load municipal data on "gemeinde" level: N = 10955
data_municipal <- readr::read_csv("data/municipal/municipal_main.csv")


# load crime statistics PKS 2022: N = 16400 (for different crimes)
data_pks_2022 <- readr::read_delim(
  "data/pks/2022/KR-F-01-T01-Kreise-Faelle-HZ_csv.csv",
  delim = ";",
  skip = 1,
  na = c("", "NA", "------"),
  locale = readr::locale(encoding = "Latin1")
) |>
  janitor::clean_names()


# Deutschlandatlas: Steuereinnahmekraft (st_einnkr)
data_taxpower_2022 <- readxl::read_xlsx(
  "data/deutschlandatlas/Deutschlandatlas-Daten.xlsx",
  sheet = "Deutschlandatlas_GEM1222"
) |>
  select(GKZ1222, st_einnkr) |>
  mutate(st_einnkr = na_if(st_einnkr, -9999))


# Shape files for Germany on Kreis level
data_shapefile <- sf::st_read("data/shapefiles/EPSG_25832/VG250_KRS.shp") |>
  filter(row_number() == 1, .by = AGS)
