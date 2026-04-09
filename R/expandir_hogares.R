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
  if (!is.data.frame(.datos)) {
    rlang::abort("`.datos` debe ser un data.frame o tibble.")
  }
  if (!is.data.frame(.P)) {
    rlang::abort("`.P` debe ser un data.frame o tibble.")
  }
  if (is.null(attr(.P, "base"))) {
    rlang::abort("`.P` debe ser una base P expandida con `expandir_eusilc().`")
  }
  if (attr(.P, "base") != "P") {
    rlang::abort("`.P` debe ser una base P expandida con `expandir_eusilc().`")
  }
  if (!is.null(.D) & !is.data.frame(.D)) {
    rlang::abort("`.D` debe ser un data.frame o tibble.")
  }

  # Calcular vbles ---------------------
  bloques <- c(D = !is.null(.D), attr(.P, "bloques")["LMH"])

  P <- agregar_personas(.P)
  datos <- .datos |>
    dplyr::left_join(
      P, by = dplyr::join_by(HB010 == pi01, HB020 == pi02, HB030 == pi04)
    )

  datos <- construir_vbles_h(datos, P)

  if (bloques["D"]) {
    D <- .D |>
      dplyr::select(DB010, DB020, DB030, DB040, DB090)
    datos <- datos |>
      dplyr::left_join(
        D, by = dplyr::join_by(HB010 == DB010, HB020 == DB020, HB030 == DB030)
      ) |>
      dplyr::rename(hi06 = DB090)
  } else {
    rlang::warn("No se proporciono el conjunto D. Se omiten: `hi06`.")
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
  } else {
    rlang::warn("No se encontro `PL130` o `PL230` en `.P`. Se omiten: `hy01p`, `hy02p`, `hy03p`.")
  }

  # Arreglos y devolver ----------------
  if (!.mantener) {
    datos <- datos |> dplyr::select(-dplyr::all_of(c(names(.datos), names(.D))))
  }

  datos <- datos |>
    dplyr::relocate(
      dplyr::starts_with("hi"),
      dplyr::starts_with("hd"),
      dplyr::starts_with("hl"),
      dplyr::starts_with("hy"),
      dplyr::starts_with("hp"),
      dplyr::everything()
    )

  attr(datos, "bloques") <- bloques
  attr(datos, "base") <- "H"

  return(datos)
}
