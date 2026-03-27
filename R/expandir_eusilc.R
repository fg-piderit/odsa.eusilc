#' Title
#'
#' @param .datos descripcion
#' @param .base descripcion
#' @param ... descripcion
#' @param .individuos descripcion
#' @param .mantener descripcion
#'
#' @returns valor
#' @export
expandir_eusilc <- function(
    .datos,
    .base,
    ...,
    .individuos = NULL,
    .mantener = FALSE
) {
  if (!is.data.frame(.datos)) {
    rlang::abort("`.datos` debe ser un data.frame o tibble")
  }
  .base <- rlang::arg_match(.base, c("P", "H"))

  if (.base == "P") {

    modulo_lmh <- all(vars_lmh %in% names(.datos))
    if (!modulo_lmh) rlang::warn("No se encontro `PL130` o `PL230`.\nSe omiten las variables que dependen de ellas")

    .datos <- construir_variables_p(.datos, .pais, modulo_lmh, .mantener = .mantener)
    attr(.datos, "LMH") <- modulo_lmh

  } else {

    if (is.null(.individuos)) {
      rlang::abort("Para expandir la base tipo `H` se debe proveer `.individuos`.")
    }
    if (is.null(attr(.individuos, "expandida"))) {
      rlang::abort("`.individuos` debe ser una base P expandida con `expandir_eusilc().`")
    }
    if (attr(.individuos, "base") != "P") {
      rlang::abort("`.individuos` debe ser una base P expandida con `expandir_eusilc().`")
    }

    .datos <- construir_variables_h(.datos, .pais, .individuos, .mantener = .mantener)

  }

  # ------------------------------------------
  attr(.datos, "expandida") <- TRUE
  attr(.datos, "base") <- .base

  return(.datos)
}
