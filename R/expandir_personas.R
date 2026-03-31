#' Construir variables adicionales en los conjuntos de datos P de la EU-SILC
#'
#' @param .datos Conjunto de datos P de la EU-SILC.
#' @param ... ...
#' @param .D Conjunto de datos D de la EU-SILC.
#' @param .R Conjunto de datos R de la EU-SILC.
#' @param .mantener Conservar las variables originales en el conjunto de datos final o eliminarlas.
#'
#' @returns Conjunto de datos de la EU-SILC con variables adicionales de uso habitual.
#' @export
expandir_personas <- function(
    .datos,
    ...,
    .D = NULL,
    .R = NULL,
    .mantener = FALSE
) {
  # Chequeos args ----------------------

  # Calcular vbles ---------------------
  bloques <- c(
    D = !is.null(.D),
    R = !is.null(.R),
    LMH = all(c("PL130", "PL230") %in% names(.datos))
  )

  datos <- construir_vbles_p(.datos, .pais)

  if (bloques["D"]) {
    D <- .D |>
      dplyr::select(DB010, DB020, DB030, DB040)
    datos <- datos |>
      dplyr::left_join(
        D, by = dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030)
      ) |>
      dplyr::rename(pi03 = DB040)
  }

  if (bloques["R"]) {
    R <- .R |>
      dplyr::select(RB010, RB020, RB030, RB081, RB082, RB280, RB290)
    datos <- datos |>
      dplyr::left_join(
        R, by = dplyr::join_by(PB010 == RB010, PB020 == RB020, PB030 == RB030)
      )
    datos <- datos |>
      dplyr::mutate(
        pd01a = RB082,
        pd01b = RB081,
        pd04 = dplyr::if_else(RB280 == pi02, 1, 2),
        pd05 = dplyr::if_else(RB290 == pi02, 1, 2),
        .keep = "all"
      )
  }

  if (bloques["LMH"]) {
    datos <- construir_vbles_p_lmh(datos, .pais)
  }

  if (!.mantener) {
    datos <- datos |>
      dplyr::select(-dplyr::any_of(c(names(.datos), names(.R))))
  }

  attr(datos, "bloques") <- bloques
  attr(datos, "base") <- "P"

  return(datos)
}
