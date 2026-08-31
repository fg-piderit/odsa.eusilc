# Script para armar las tablas auxiliares públicas e internas del paquete

library(dplyr)
library(readxl)
library(tidyr)

devtools::load_all()

etiquetas <- read_xlsx("data-raw/xlsx/tabla_etiquetas.xlsx")
tabla_isco <- read_xlsx("data-raw/xlsx/tabla_isco.xlsx")
tabla_pd03 <- read_xlsx("data-raw/xlsx/tabla_pd03.xlsx")
tabla_pl01 <- read_xlsx("data-raw/xlsx/tabla_pl01.xlsx")
tabla_pl20 <- read_xlsx("data-raw/xlsx/tabla_pl20.xlsx")
tabla_pl21 <- read_xlsx("data-raw/xlsx/tabla_pl21.xlsx")
tabla_ppa <- read_xlsx("data-raw/xlsx/tabla_ppa.xlsx")
advertencias_estandarizacion_fuente <- read.csv(
  "data-raw/advertencias_estandarizacion.csv",
  check.names = FALSE,
  na.strings = ""
)
cobertura_advertencias_fuente <- read.csv(
  "data-raw/cobertura_advertencias.csv",
  check.names = FALSE,
  na.strings = ""
)

# Funciones ----------------------------------------------------------------

transformar_tabla_ppa <- function(.tabla_ppa) {
  tabla_ppa <-
    .tabla_ppa |>
    pivot_longer(
      cols = -PB020,
      names_to = "PB010",
      values_to = "ppa_factor",
      names_transform = list(PB010 = as.integer)
    ) |>
    relocate(PB020)

  ppa_us <-
    tabla_ppa |>
    filter(PB020 == "US") |>
    select(-PB020)

  tabla_ppa_ <-
    tabla_ppa |>
    left_join(ppa_us, by = "PB010", suffix = c("", "_us"))

  list(
    publica = tabla_ppa,
    interna = tabla_ppa_
  )
}

transformar_advertencias_estandarizacion <- function(.advertencias) {
  .advertencias |>
    rowwise() |>
    mutate(
      anio = list(seq.int(anio_desde, anio_hasta)),
      pais = list(strsplit(paises, ";", fixed = TRUE)[[1]]),
      variable = list(strsplit(variables, ";", fixed = TRUE)[[1]])
    ) |>
    ungroup() |>
    unnest(anio) |>
    unnest(pais) |>
    unnest(variable) |>
    select(
      id_advertencia,
      anio,
      pais,
      base,
      conjunto_origen,
      variable,
      tipo,
      advertencia,
      consecuencia,
      accion_paquete,
      fuente
    ) |>
    arrange(anio, pais, base, variable, id_advertencia)
}

transformar_cobertura_advertencias <- function(.cobertura) {
  .cobertura |>
    rowwise() |>
    mutate(
      anio = list(seq.int(anio_desde, anio_hasta)),
      pais = list(strsplit(paises, ";", fixed = TRUE)[[1]])
    ) |>
    ungroup() |>
    unnest(anio) |>
    unnest(pais) |>
    select(anio, pais, estado, fuentes) |>
    arrange(anio, pais)
}

# Transformar tablas -------------------------------------------------------

etiquetas_ <- armar_etiquetas(etiquetas)

tablas_ppa <- transformar_tabla_ppa(tabla_ppa)
tabla_ppa <- tablas_ppa$publica
tabla_ppa_ <- tablas_ppa$interna

advertencias_estandarizacion <- transformar_advertencias_estandarizacion(
  advertencias_estandarizacion_fuente
)
cobertura_advertencias <- transformar_cobertura_advertencias(
  cobertura_advertencias_fuente
)

paises_probados <- c("ES", "IT", "DE", "PL")

# Guardar ------------------------------------------------------------------

usethis::use_data(etiquetas, overwrite = TRUE)
usethis::use_data(tabla_ppa, overwrite = TRUE)
usethis::use_data(advertencias_estandarizacion, overwrite = TRUE)
usethis::use_data(cobertura_advertencias, overwrite = TRUE)
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
