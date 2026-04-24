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
#' @param .PL032 PL032
#' @param .pl05 pl05
#' @param .pl06b pl06b
#' @param .pl07 pl07
#' @param .pl08b pl08b
#' @param .nivel Nivel de agregación.
#' @param .lmh lmh
#'
#' @returns heterogeneidad
calc_heterogeneidad <- function(.PL040A, .PL032, .pl05, .pl06b, .pl07, .pl08b, .nivel, .lmh = TRUE) {
  rlang::arg_match(.nivel, c("a", "b"))

  if (!.lmh) {
    pl09 <- NA_integer_
  } else if (.nivel == "a") {
    pl09 <- dplyr::case_when(
      .PL040A == 1 & .pl06b > 1 ~ 1,
      .PL040A == 2 & .pl08b == 1 ~ 2,
      .PL032 == 1 & .pl07 == 1 ~ 3,
      .PL040A == 3 & .pl07 == 2 & .pl06b == 3 ~ 4,
      .PL040A == 3 & .pl07 == 2 & .pl06b == 2 ~ 5,
      .PL040A == 1 & .pl06b == 1 ~ 6,
      .PL040A == 2 & .pl08b == 2 ~ 7,
      .PL040A == 3 & .pl07 == 2 & .pl06b == 1 ~ 8,
      .PL040A == 4 ~ 8,
      .PL032 == 1 & .pl05 == 8 ~ 9,
      .default = NA_integer_
    )
  } else {
    pl09 <- dplyr::case_when(
      .PL040A == 1 & .pl06b > 1 ~ 2,
      .PL040A == 2 & .pl08b == 1 ~ 2,
      .PL032 == 1 & .pl07 == 1 ~ 1,
      .PL040A == 3 & .pl07 == 2 & .pl06b %in% 2:3 ~ 2,
      .PL040A == 1 & .pl06b == 1 ~ 3,
      .PL040A == 2 & .pl08b == 2 ~ 3,
      .PL040A == 3 & .pl07 == 2 & .pl06b == 1 ~ 3,
      .PL040A == 4 ~ 3,
      .PL032 == 1 & .pl05 == 8 ~ 2,
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
#'
#' @returns egp
calc_egp <- function(.PL051A, .PL040A, .PL150) {
  .pl10 <- dplyr::recode_values(
    .PL051A, from = tabla_isco$PL051, to = tabla_isco$.pl10, default = NA_integer_
  )
  pl10 <-  dplyr::case_when(
    .pl10 == 8 & .PL040A != 1 ~ 8,
    .pl10 > 1 & .PL040A == 1 ~ 2,
    .pl10 > 1 & .PL040A == 2 ~ 6,
    .pl10 > 1 & is.na(.PL040A) ~ NA_integer_,
    .pl10 > 2 & .PL150 == 1 ~ 7,
    .pl10 > 2 & is.na(.PL150) ~ NA_integer_,
    .default = .pl10
  )

  return(pl10)
}

#' Title
#'
#' @param .PL040A PL040A
#' @param .PY030G PY030G
#' @param .PY035G PY035G
#' @param .nivel nivel de agregación
#'
#' @returns informalidad
calc_informalidad <- function(.PL040A, .PY030G, .PY035G, .nivel) {
  rlang::arg_match(.nivel, c("a", "b"))

  if (.nivel == "a") {
    pl11 <- dplyr::case_when(
      .PL040A == 3 & .PY030G != 0 ~ 1,
      .PL040A == 3 & .PY030G == 0 ~ 2,
      .PL040A %in% 1:2 & !(.PY030G == 0 & .PY035G == 0) ~ 3,
      .PL040A %in% 1:2 & .PY030G == 0 & .PY035G == 0 ~ 4,
      .PL040A == 4 ~ 4,
      .default = NA_integer_
    )
  } else {
    pl11 <- dplyr::case_when(
      .PL040A == 3 & .PY030G != 0 ~ 1,
      .PL040A == 3 & .PY030G == 0 ~ 2,
      .PL040A %in% 1:2 & !(.PY030G == 0 & .PY035G == 0) ~ 1,
      .PL040A %in% 1:2 & .PY030G == 0 & .PY035G == 0 ~ 2,
      .PL040A == 4 ~ 2,
      .default = NA_integer_
    )
  }

  return(pl11)
}

#' Title
#'
#' @param .py10 py10
#' @param .pl09b pl09b
#' @param .sector sector
#' @param .lmh lmh
#'
#' @returns py13, py14 o py15
calc_y_sector <- function(.py10, .pl09b, .sector, .lmh = TRUE) {
  if (!.lmh) {
    py1x <- NA_real_
  } else {
    py1x <- dplyr::case_when(
      .py10 != 0 & is.na(.pl09b) ~ NA_real_,
      .py10 != 0 & .pl09b == .sector ~ .py10,
      .default = 0
    )
  }

  return(py1x)
}
