#' Title
#'
#' @param .datos datos
#' @param ... ...
#'
#' @returns datos etiquetados
#' @export
etiquetar_eusilc <- function(.datos, ...) {
  .datos <- labelled::set_variable_labels(.datos, .labels = etq_variables, .strict = FALSE)
  .datos <- labelled::set_value_labels(.datos, .labels = etq_valores, .strict = FALSE)

  return(.datos)
}
