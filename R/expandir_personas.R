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
  if (!is.data.frame(.datos)) {
    rlang::abort("`.datos` debe ser un data.frame o tibble.")
  }
  if (!is.null(.D) & !is.data.frame(.D)) {
    rlang::abort("`.D` debe ser un data.frame o tibble.")
  }
  if (!is.null(.R) & !is.data.frame(.D)) {
    rlang::abort("`.R` debe ser un data.frame o tibble.")
  }
  if (!is.logical(.mantener)) {
    rlang::abort("`.mantener` debe ser `TRUE` o `FALSE`.")
  }

  # Calcular vbles ---------------------
  bloques <- c(
    D = !is.null(.D),
    R = !is.null(.R),
    LMH = all(c("PL130", "PL230") %in% names(.datos))
  )

  datos <- construir_vbles_p(.datos)

  if (bloques["D"]) {
    D <- .D |>
      dplyr::select(DB010, DB020, DB030, DB040)
    datos <- datos |>
      dplyr::left_join(
        D, by = dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030)
      ) |>
      dplyr::rename(pi03 = DB040)
  } else {
    rlang::warn("No se proporciono el conjunto D. Se omiten: `pi03`.")
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
  } else {
    rlang::warn("No se proporciono el conjunto R. Se omiten: `pd01a`, `pd01b`, `pd04`, `pd05`.")
  }

  if (bloques["LMH"]) {
    datos <- construir_vbles_p_lmh(datos)
  } else {
    rlang::warn("No se encontro `PL130` o `PL230`. Se omiten: `pl06a`, `pl06b`, `pl07`, `pl09a`, `pl09b`, `py01`, `py02`, `py03`.")
  }

  # Arreglos y devolver ----------------
  if (!.mantener) {
    datos <- datos |> dplyr::select(-dplyr::any_of(c(names(.datos), names(.R))))
  }

  datos <- datos |>
    dplyr::relocate(
      dplyr::starts_with("pi"),
      dplyr::starts_with("pd"),
      dplyr::starts_with("pl"),
      dplyr::starts_with("py"),
      dplyr::everything()
    )

  attr(datos, "bloques") <- bloques
  attr(datos, "base") <- "P"

  return(datos)
}
