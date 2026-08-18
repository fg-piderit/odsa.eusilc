# Script para armar tablas lookup

load("R/sysdata.rda")

tabla_isco <- readxl::read_xlsx("data-raw/xlsx/tabla_isco.xlsx")
tabla_pd03 <- readxl::read_xlsx("data-raw/xlsx/tabla_pd03.xlsx")
tabla_pl01 <- readxl::read_xlsx("data-raw/xlsx/tabla_pl01.xlsx")
tabla_pl20 <- readxl::read_xlsx("data-raw/xlsx/tabla_pl20.xlsx")
tabla_pl21 <- readxl::read_xlsx("data-raw/xlsx/tabla_pl21.xlsx")

usethis::use_data(
  etiquetas_,
  tabla_ppa_,
  tabla_isco,
  tabla_pd03,
  tabla_pl01,
  tabla_pl20,
  tabla_pl21,
  paises_probados,
  internal = TRUE,
  overwrite = TRUE
)