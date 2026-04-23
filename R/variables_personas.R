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
#' @param .nivel Nivel de agregación.
#' @param .lmh Módulo LMH
#'
#' @returns Tamaño del establecimiento
calc_testablecimiento <- function(.PL130, .nivel, .lmh = TRUE) {
  rlang::arg_match(.nivel, c("a", "b"))

  if (!.lmh) {
    pl06 <- NA_integer_
  } else if (.nivel == "a") {
    pl06 <- dplyr::recode_values(
      .PL130, from = tabla_pl06$PL130, to = tabla_pl06$pl06a, default = NA_integer_
    )
  } else {
    pl06 <- dplyr::recode_values(
      .PL130, from = tabla_pl06$PL130, to = tabla_pl06$pl06b, default = NA_integer_
    )
  }

  return(pl06)
}

#' Title
#'
#' @param .PL040A PL040A
#' @param .pl06b pl06b
#' @param .pl08b pl08b
#' @param .nivel Nivel de agregación.
#' @param .lmh lmh
#'
#' @returns heterogeneidad
calc_heterogeneidad <- function(.PL040A, .pl06b, .pl08b, .nivel, .lmh = TRUE) {
  rlang::arg_match(.nivel, c("a", "b"))

  if (!.lmh) {
    pl09 <- NA_integer_
  } else if (.nivel == "a") {
    pl09 <- dplyr::case_when(
      PL040A == 1 & pl06b > 1 ~ 1,
      PL040A == 2 & pl08b == 1 ~ 2,
      pl02 == 1 & pl07 == 1 ~ 3,
      PL040A == 3 & pl07 == 2 & pl06b == 3 ~ 4,
      PL040A == 3 & pl07 == 2 & pl06b == 2 ~ 5,
      PL040A == 1 & pl06b == 1 ~ 6,
      PL040A == 2 & pl08b == 2 ~ 7,
      PL040A == 3 & pl07 == 2 & pl06b == 1 ~ 8,
      PL040A == 4 ~ 8,
      pl02 == 1 & pl05 == 8 ~ 9,
      .default = NA_integer_
    )
  } else {
    pl09 <- dplyr::case_when(
      pl09a == 3 ~ 1,
      pl09a %in% c(1, 2, 4, 5, 9) ~ 2,
      pl09a %in% c(7, 8) ~ 3,
      .default = NA_integer_
    )
  }

  return(pl09)
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
