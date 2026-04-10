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
  nac_agrup <- if_else(nac_agrup < .anio, nac_agrup, .anio)
  return(nac_agrup)
}
