# Script para armar el conjunto de datos y las listas anidadas de etiquetas
# para incluir en el paquete

library(tidyverse)
library(readxl)

load("R/sysdata.rda")

etiquetas <- readxl::read_xlsx("data-raw/xlsx/tabla_etiquetas.xlsx")
etiquetas_ <- armar_etiquetas(etiquetas)

usethis::use_data(etiquetas, overwrite = TRUE)
usethis::use_data(
  etiquetas_,
  tabla_ppa,
  tabla_isco,
  tabla_pd03,
  tabla_pl01,
  tabla_pl20,
  tabla_pl21,
  paises_probados,
  internal = TRUE,
  overwrite = TRUE
)
