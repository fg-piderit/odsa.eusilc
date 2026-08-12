# Script para armar la tabla de factores de conversión a unidades de PPA
# interna y externa

library(tidyverse)
library(readxl)

load("R/sysdata.rda")

tabla_ppa <- readxl::read_xlsx("data-raw/xlsx/tabla_ppa.xlsx")

tabla_ppa <-
  tabla_ppa |>
  tidyr::pivot_longer(
    cols = -PB020,
    names_to = "PB010",
    values_to = "ppa_factor",
    names_transform = list(PB010 = as.integer)
  ) |>
  dplyr::relocate(PB020)

ppa_us <-
  tabla_ppa |>
  filter(PB020 == "US") |>
  select(-PB020)

tabla_ppa_ <-
  tabla_ppa |>
  left_join(ppa_us, by = "PB010", suffix = c("", "_us"))

usethis::use_data(tabla_ppa, overwrite = TRUE)
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
