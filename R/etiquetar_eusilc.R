#' Title
#'
#' @param .datos datos
#' @param ... ...
#'
#' @returns datos etiquetados
#' @export
etiquetar_eusilc <- function(.datos, ...) {
  base <- attr(.datos, "base")
  .datos <- labelled::set_variable_labels(
    .datos,
    .labels = etq[[base]]$variables,
    .strict = FALSE
  )
  .datos <- labelled::set_value_labels(
    .datos,
    .labels = etq[[base]]$valores,
    .strict = FALSE
  )

  return(.datos)
}
