#' Agrupa los años de nacimiento en grupos de cinco años
#'
#' La función agrupa los años según el criterio que se aplica en el conjunto de
#' datos de Alemania. Los años de nacimiento se registran desde 81 años atrás
#' hasta el presente; aquellos que nacieron antes se agrupan en el primer año.
#' Los grupos de cinco años se arman a partir del primer año registrado. P.e.,
#' en 2023 el primer año registrado fue 2023 - 81 = 1942, por lo cual los
#' grupos resultan ser 1942-1946, 1947-1951, 1951-1956, etc.
#'
#' @param .anio Año de la encuesta.
#' @param .nac Vector de años de nacimiento.
#'
#' @returns Vector de años de nacimiento agrupados.
agrupar_nac <- function(.anio, .nac) {
  desf <- (.anio - 1) %% 5
  nac_agrup <- .nac + (desf - .nac) %% 5
  nac_agrup <- dplyr::if_else(nac_agrup < .anio, nac_agrup, .anio)
  return(nac_agrup)
}

#' Title
#'
#' @param .PL130 PL130
#' @param .lmh Módulo LMH
#'
#' @returns Tamaño del establecimiento
calc_testablecimiento <- function(.PL130, .lmh = TRUE) {
  if (.lmh) {
    dplyr::case_when(
      # LOOKUP TABLE?
      .PL130 <= 5 ~ 1,
      .PL130 > 5 & .PL130 <= 9 ~ 2,
      .PL130 > 9 & .PL130 <= 11 ~ 3,
      .PL130 > 11 & .PL130 <= 13 ~ 4,
      .default = NA_integer_
    )
  } else {
    NA_integer_
  }
}

#' Title
#'
#' @param .PL040A PL040A
#' @param .pl06b pl06b
#' @param .pl08b pl08b
#' @param .lmh lmh
#'
#' @returns heterogeneidad
calc_heterogeneidad <- function(.PL040A, .pl06b, .pl08b, .lmh = TRUE) {

}

#' Title
#'
#' @param .PL051A PL051A
#' @param .PL040A PL040A
#' @param .PL150 PL150
#' @param .lmh lmh
#'
#' @returns egp
calc_egp <- function(.PL051A, .PL040A, .PL150, .lmh = TRUE) {

}

#' Title
#'
#' @param .PL040A PL040A
#' @param .PY030G PY030G
#' @param .PY035G PY035G
#'
#' @returns informalidad
calc_informalidad <- function(.PL040A, .PY030G, .PY035G) {

}
