#' Cobertura de la revision de advertencias de estandarizacion
#'
#' Permite distinguir una operacion revisada sin hallazgos de una operacion que
#' todavia no fue examinada.
#'
#' @format ## `tabla_cobertura`
#' Un tibble con una fila por rango de años, pais y fuente:
#' \describe{
#'   \item{anio_desde}{Año inicial de la revisión.}
#'   \item{anio_hasta}{Año final de la revisión.}
#'   \item{pais}{Codigo de pais de dos letras.}
#'   \item{estado}{Estado de la revision documental.}
#'   \item{fuente}{Familias de documentos revisadas.}
#' }
#' @source Documentacion de EU-SILC disponible con las entregas UDB de
#'   Eurostat.
"tabla_cobertura"