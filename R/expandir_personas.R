#' Construir variables adicionales en los conjuntos de datos P de la EU-SILC
#'
#' @param .datos Conjunto de datos P de la EU-SILC.
#' @param ... ...
#' @param .D Conjunto de datos D de la EU-SILC.
#' @param .R Conjunto de datos R de la EU-SILC.
#' @param .expandir Conservar las variables originales en el conjunto de datos final o eliminarlas.
#'
#' @returns Conjunto de datos de la EU-SILC con variables adicionales de uso habitual.
#' @export
expandir_personas <- function(
    .datos,
    ...,
    .D = NULL,
    .R = NULL,
    .expandir = FALSE
) {
  # Chequeos args ------------------------------------------------------------
  if (!is.data.frame(.datos)) {
    rlang::abort("`.datos` debe ser un data.frame o tibble.")
  }
  if (!is.null(.D) & !is.data.frame(.D)) {
    rlang::abort("`.D` debe ser un data.frame o tibble.")
  }
  if (!is.null(.R) & !is.data.frame(.R)) {
    rlang::abort("`.R` debe ser un data.frame o tibble.")
  }
  if (!is.logical(.expandir)) {
    rlang::abort("`.expandir` debe ser `TRUE` o `FALSE`.")
  }

  # Chequeos args ------------------------------------------------------------
  anio <- unique(.datos$PB010)
  bloques <- c(
    D = !is.null(.D),
    R = !is.null(.R),
    LMH = all(c("PL130", "PL230") %in% names(.datos))
  )

  if (anio <= 2021) {
    .datos <- dplyr::mutate(
      .datos,
      PE041 = PE040,
      PL032 = dplyr::case_when(
        PL031 %in% 1:4 ~ 1,
        PL031 %in% 5 ~ 2,
        PL031 %in% 6:11 ~ 3,
        .default = NA
      ),
      PL040A = PL040,
      PL051A = PL051,
      PL111A = PL111
    )
  }

  datos <- construir_vbles_p(.datos)

  if (bloques["D"]) {
    D <- dplyr::select(.D, DB010, DB020, DB030, DB040)
    datos <- dplyr::left_join(
      datos, D,
      by = dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030)
    )
    datos <- dplyr::rename(datos, pi03 = DB040)
  } else {
    rlang::warn("No se proporciono el conjunto D. Se omiten: `pi03`.")
  }

  if (bloques["R"]) {
    R <- dplyr::select(.R, RB010, RB020, RB030, RB080,
                       dplyr::any_of(c("RB081", "RB082", "RB280", "RB290")))
    datos <- dplyr::left_join(
      datos, R,
      by = dplyr::join_by(PB010 == RB010, PB020 == RB020, PB030 == RB030)
    )

    if (anio <= 2021) {
      datos <- dplyr::mutate(
        datos,
        pd01b = PB010 - RB080 - 1,
        pd01c = PB010 - agrupar_nac(PB010, RB080) - 1,
        .keep = "all"
      )
      rlang::warn("La base es anterior a 2021. Se omiten: `pd01a`, `pd04` y `pd05`.")
    } else {
      datos <- dplyr::mutate(
        datos,
        pd01a = RB082,
        pd01b = dplyr::if_else(!is.na(RB081), RB081, PB010 - RB080 - 1),
        pd01c = PB010 - agrupar_nac(PB010, RB080) - 1,
        pd04 = dplyr::if_else(RB280 == pi02, 1, 2),
        pd05 = dplyr::if_else(RB290 == pi02, 1, 2),
        .keep = "all"
      )
    }
  } else {
    rlang::warn("No se proporciono el conjunto R. Se omiten: `pd01a`, `pd01b`, `pd04`, `pd05`.")
  }

  if (bloques["LMH"]) {
    datos <- construir_vbles_p_lmh(datos)
  } else {
    rlang::warn("No se encontro `PL130` o `PL230`. Se omiten: `pl06a`, `pl06b`, `pl07`, `pl09a`, `pl09b`, `py01`, `py02`, `py03`.")
  }

  # Arreglos y devolver ------------------------------------------------------
  if (!.expandir) {
    datos <- dplyr::select(
      datos,
      dplyr::starts_with(c("pi", "pd", "pl", "py", "."), ignore.case = FALSE)
    )
  } else {
    datos <- dplyr::relocate(
      datos,
      dplyr::starts_with(c("pi", "pd", "pl", "py"), ignore.case = FALSE)
    )
  }

  attr(datos, "bloques") <- bloques
  attr(datos, "base") <- "P"

  return(datos)
}
