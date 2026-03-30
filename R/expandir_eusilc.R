#' Construir variables adicionales en los conjuntos de datos de la EU-SILC
#'
#' @param .datos Conjunto de datos de la EU-SILC. Debe ser el conjunto P o el conjunto H.
#' @param .base El tipo de conjunto de datos. Debe ser P o H.
#' @param .pais El pais al que corresponde el conjunto de datos.
#' @param ... ...
#' @param .individuos Conjunto de datos P expandido por `expandir_eusilc()`. Sólo si `.base` es H.
#' @param .mantener Conservar las variables originales en el conjunto de datos final o eliminarlas.
#'
#' @returns Conjunto de datos de la EU-SILC con variables adicionales de uso habitual.
#' @export
expandir_eusilc <- function(
    .datos,
    .base,
    .pais,
    ...,
    .individuos = NULL,
    .mantener = FALSE
) {

  if (!is.data.frame(.datos)) rlang::abort("`.datos` debe ser un data.frame o tibble")
  rlang::arg_match(.base, c("P", "H"))

  attr(.datos, "bloques") <- c(D = FALSE, R = FALSE, LMH = FALSE)
  attr(.datos, "base") <- .base

  if (.base == "P") {

    variables <- names(.datos)

    bloques <- c(
      D = "DB040" %in% variables,
      R = all(c("RB081", "RB082", "RB280", "RB290") %in% variables),
      LMH = all(c("PL130", "PL230") %in% variables)
    )
    if (!all(bloques)) {
      mensaje <- "No se encontraron: "
      if (!bloques["D"]) mensaje <- paste0(mensaje, "`DB040`; ")
      if (!bloques["R"]) mensaje <- paste0(mensaje, "`RB081`, `RB082`, `RB280` o `RB290`; ")
      if (!bloques["LMH"]) mensaje <- paste0("`PL130` o `PL230`; ")
      mensaje <- paste0(mensaje, "se omiten las variables que dependen de ellas.")

      rlang::warn(mensaje)
    }

    .datos <- construir_variables_p(.datos, .pais, bloques, .mantener = .mantener)

    attr(.datos, "bloques") <- bloques

  } else {

    if (is.null(.individuos)) {
      rlang::abort("Para expandir la base tipo `H` se debe proveer `.individuos`.")
    }
    if (is.null(attr(.individuos, "base"))) {
      rlang::abort("`.individuos` debe ser una base P expandida con `expandir_eusilc().`")
    }

    base <- attr(.individuos, "base")
    bloques <- attr(.individuos, "bloques")

    if (base != "P") {
      rlang::abort("`.individuos` debe ser una base P expandida con `expandir_eusilc().`")
    }
    if (!all(bloques)) {
      mensaje <- "No se encontraron en `.individuos`: "
      if (!bloques["D"]) mensaje <- paste0(mensaje, "DB040; ")
      if (!bloques["R"]) mensaje <- paste0(mensaje, "`RB081`, `RB082`, `RB280` o `RB290`; ")
      if (!bloques["LMH"]) mensaje <- paste0("`PL130` o `PL230`; ")
      mensaje <- paste0(mensaje, "se omiten las variables que dependen de ellas.")

      rlang::warn(mensaje)
    }

    .datos <- construir_variables_h(.datos, .pais, .individuos, .mantener = .mantener)

  }

  # ------------------------------------------
  return(.datos)
}
