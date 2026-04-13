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

# ============================================================================
#' Construye variables en la base P de la EU-SILC.
#'
#' @param .datos Conjunto P de la EU-SILC.
#' @param ... ...
#'
#' @returns Conjunto de datos P de la EU-SILC con variables adicionales.
construir_vbles_p <- function(
    .datos,
    ...
) {
  datos <- .datos |>
    dplyr::mutate(
      # Bloque I -----------------------
      pi01 = PB010,
      pi02 = PB020,
      pi04 = PX030,
      pi05 = PB030,
      pi06 = PB040,
      # Bloque D -----------------------
      pd02 = PB150,
      pd03 = dplyr::case_when(
        # REVISAR CONSTRUCCION
        PE041 == 0   ~ 1,
        PE041 == 100 ~ 2,
        PE041 == 200 ~ 3,
        PE041 == 300 ~ 4,
        PE041 == 340 ~ 4,
        PE041 == 344 ~ 4,
        PE041 == 350 ~ 4,
        PE041 == 353 ~ 4,
        PE041 == 354 ~ 4,
        PE041 == 450 ~ 5,
        PE041 == 500 ~ 6,
        .default = NA
      ),
      # Bloque L -----------------------
      pl01 = "a definir",
      pl02 = dplyr::case_when(
        PL032 == 1 ~ 1,
        PL032 == 2 ~ 2,
        PL032 %in% 3:8 ~ 3,
        .default = NA
      ),
      pl03a = PL051A,
      pl03b = PL051A %/% 10,
      pl04 = PL040A,
      pl05 = dplyr::case_when(
        PL111A == "b - e" ~ 1,
        PL111A == "f" ~ 2,
        PL111A == "g" ~ 3,
        PL111A == "i" ~ 3,
        PL111A == "h" ~ 4,
        PL111A == "j" ~ 4,
        PL111A == "k" ~ 5,
        PL111A == "l - n" ~ 5,
        PL111A == "o" ~ 6,
        PL111A == "p" ~ 7,
        PL111A == "q" ~ 7,
        PL111A == "r - u" ~ 9,
        PL111A == "a" ~ 9,
        .default = NA
      ),
      pl08a = dplyr::case_when(
        PL051A %in% 11:13 | PL051A %/% 10 == 2 | PL051A == 1 ~ 1,
        PL051A == 14 | PL051A %/% 10 == 3 ~ 2,
        PL051A %/% 10 %in% 4:8 | PL051A == 2 ~ 3,
        PL051A %/% 10 == 9 | PL051A == 3 ~ 4,
        .default = NA
      ),
      pl08b = dplyr::case_when(
        PL051A == 2 | (PL051A >= 20 & PL051A <= 35) ~ 1,
        !is.na(PL051A) ~ 2,
        .default = NA
      ),
      .pl10 = dplyr::case_when(
        PL051A %in% c(1, 11:26) ~ 1,
        PL051A %in% c(2, 31:35) ~ 3,
        PL051A %in% c(3, 41:44) ~ 4,
        PL051A %in%   51:54 ~ 5,
        PL051A %in% c(61, 62, 92) ~ 10,
        PL051A %in%   71:83 ~ 8,
        PL051A %in% c(91, 93:96) ~ 9,
        .default = NA
      ),
      pl10 = dplyr::case_when(
        .pl10 == 8 & PL040A != 1 ~ 8,
        .pl10 > 1 & PL040A == 1 ~ 2,
        .pl10 > 1 & PL040A == 2 ~ 6,
        .pl10 > 1 & is.na(PL040A) ~ NA,
        .pl10 > 2 & PL150 == 1 ~ 7,
        .pl10 > 2 & is.na(PL150) ~ NA,
        .default = .pl10
      ),
      pl11a = dplyr::case_when(
        PL040A == 3 & PY030G != 0 ~ 1,
        PL040A == 3 & PY030G == 0 ~ 2,
        PL040A %in% 1:2 & !(PY030G == 0 & PY035G == 0) ~ 3,
        PL040A %in% 1:2 & PY030G == 0 & PY035G == 0 ~ 4,
        PL040A == 4 ~ 4,
        .default = NA
      ),
      pl11b = dplyr::case_when(
        pl11a %in% c(1, 3) ~ 1,
        pl11a %in% c(2, 4) ~ 2,
        .default = NA
      ),
      # Bloque Y -----------------------
      py04 = PY010N,
      py05 = PY050N,
      py06 = PY100N,
      py07 = PY080N,
      py08 = PY090N,
      py09 = PY110N + PY120N + PY130N + PY140N,
      py10 = py06 + py07,
      py11 = py04 + py05,
      py12 = py08 + py09 + py10,
      py13 = py11 + py12,
      .haa = (PL073 + PL074) * PL060 * 4.2,
      .han = (PL075 + PL076) * PL060 * 4.2,
      py04h = py04 / .haa,
      py05h = py05 / .han,
      dplyr::across(py04:py13, \(y) y / 12, .names = "{.col}m"),
      pyxxq = "py01 a py13 (h y m) / PPA correspondiente",
      .keep = "all"
    )

  # ------------------------------------------
  return(datos)
}

# ============================================================================
#' Construye variables dependientes del módulo Labour Market and Housing de
#' la base P de la EU-SILC
#'
#' @param .datos Conjunto P de la EU-SILC expandido con [construir_vbles_p()].
#'
#' @returns Conjunto P de la EU-SILC con variables adicionales.
construir_vbles_p_lmh <- function(.datos) {
  datos <- .datos |>
    dplyr::mutate(
      # Bloque L -----------------------
      pl06a = dplyr::case_when(
        PL130 <= 5 ~ 1,
        PL130 > 5 & PL130 <= 9 ~ 2,
        PL130 > 9 & PL130 <= 11 ~ 3,
        PL130 > 11 & PL130 <= 13 ~ 4,
        .default = NA
      ),
      pl06b = dplyr::case_when(
        PL130 <= 5 ~ 1,
        PL130 > 5 & PL130 <= 11 ~ 2,
        PL130 > 11 & PL130 <= 13 ~ 3,
        .default = NA
      ),
      pl07 = dplyr::case_when(
        PL230 == 1 ~ 1,
        PL230 == 2 ~ 2,
        PL230 == 3 ~ 3,
        .default = NA
      ),
      pl09a = dplyr::case_when(
        PL040A == 1 & pl06b > 1 ~ 1,
        PL040A == 2 & pl08b == 1 ~ 2,
        pl02 == 1 & pl07 == 1 ~ 3,
        PL040A == 3 & pl07 == 2 & pl06b == 3 ~ 4,
        PL040A == 3 & pl07 == 2 & pl06b == 2 ~ 5,
        PL040A == 1 & pl06b == 1 ~ 6,
        PL040A == 2 & pl08b == 2 ~ 7,
        PL040A == 3 & pl07 == 2 & pl06b == 1 ~ 8,
        PL040A == 4 ~ 8,
        pl02 == 1 & pl05 == 8 ~ 9,
        .default = NA
      ),
      pl09b = dplyr::case_when(
        pl09a == 3 ~ 1,
        pl09a %in% c(1, 2, 4, 5, 9) ~ 2,
        pl09a %in% c(7, 8) ~ 3,
        .default = NA
      ),
      # Bloque Y -----------------------
      py01 = dplyr::if_else(py11 != 0 & pl09b == 1, py11, 0),
      py02 = dplyr::if_else(py11 != 0 & pl09b == 2, py11, 0),
      py03 = dplyr::if_else(py11 != 0 & pl09b == 3, py11, 0),
      dplyr::across(py01:py03, \(y) y / 12, .names = "{.col}m"),
      dplyr::across(py01:py03, \(y) y / .haa, .names = "{.col}h"),
      .keep = "all"
    )

  # ------------------------------------------
  return(datos)
}
