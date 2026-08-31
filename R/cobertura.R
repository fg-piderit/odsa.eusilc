#' Cobertura de la revision de advertencias de estandarizacion
#'
#' Permite distinguir una operacion revisada sin hallazgos de una operacion que
#' todavia no fue examinada.
#'
#' @format ## `cobertura_advertencias`
#' Un tibble con una fila por anio y pais:
#' \describe{
#'   \item{anio}{Anio de la operacion EU-SILC.}
#'   \item{pais}{Codigo de pais de dos letras.}
#'   \item{estado}{Estado de la revision documental.}
#'   \item{fuentes}{Familias de documentos revisadas.}
#' }
#' @source Documentacion de EU-SILC disponible con las entregas UDB de
#'   Eurostat.
"cobertura_advertencias"