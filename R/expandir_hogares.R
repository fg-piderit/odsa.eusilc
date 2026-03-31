#' Construir variables adicionales en los conjuntos de datos H de la EU-SILC
#'
#' @param .datos Conjunto de datos H de la EU-SILC.
#' @param .P Conjunto de datos P de la EU-SILC expandido por `expandir_eusilc()`.
#' @param ... ...
#' @param .D Conjunto de datos D de la EU-SILC.
#' @param .mantener Conservar las variables originales en el conjunto de datos final o eliminarlas.
#'
#' @returns Conjunto de datos de la EU-SILC con variables adicionales de uso habitual
#' @export
expandir_hogares <- function(
    .datos,
    .P,
    ...,
    .D = NULL,
    .mantener = FALSE
) {
  # Chequeos args ----------------------

  # Calcular vbles ---------------------
  bloques <- c(
    D = !is.null(.D), attr(.P, "bloques")["LMH"]
  )

  P <- agregar_personas(.P)
  datos <- .datos |>
    dplyr::left_join(
      P, by = dplyr::join_by(HB010 == pi01, HB020 == pi02, HB030 == pi04)
    )

  datos <- construir_vbles_h(datos, .pais, P)

  if (bloques["D"]) {
    D <- .D |>
      dplyr::select(DB010, DB020, DB030, DB040, DB090)
    datos <- datos |>
      dplyr::left_join(
        D, by = dplyr::join_by(HB010 == DB010, HB020 == DB020, HB030 == DB030)
      ) |>
      dplyr::rename(hi06 = DB090)
  }

  if (bloques["LMH"]) {
    datos <- datos |>
      dplyr::mutate(
        dplyr::across(
          .cols = c(hy01p:hy03p),
          .fns = \(y) y / 12, .names = "{.col}m"
        ),
        dplyr::across(
          .cols = c(hy01p:hy03p),
          .fns = \(y) y / hd01, .names = "{.col}c"
        ),
        hyxxq = "hy01p a hy03p / PPA correspondiente",
        .keep = "all"
      )
  }

  if (!.mantener) datos <- datos |> dplyr::select(-dplyr::all_of(names(.datos)))

  attr(datos, "bloques") <- bloques
  attr(datos, "base") <- "H"

  return(datos)
}
