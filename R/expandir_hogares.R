#' Construir variables adicionales en los conjuntos de datos H de la EU-SILC
#'
#' @param .datos Conjunto de datos H de la EU-SILC.
#' @param .P Conjunto de datos P de la EU-SILC expandido por `expandir_eusilc()`.
#' @param ... ...
#' @param .D Conjunto de datos D de la EU-SILC.
#' @param .expandir Conservar las variables originales en el conjunto de datos final o eliminarlas.
#'
#' @returns Conjunto de datos de la EU-SILC con variables adicionales de uso habitual
#' @export
expandir_hogares <- function(
    .datos,
    .P,
    ...,
    .D = NULL,
    .expandir = FALSE
) {
  # Chequeos args ------------------------------------------------------------
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

  # Calcular vbles -----------------------------------------------------------
  bloques <- c(D = !is.null(.D), attr(.P, "bloques")["LMH"])

  P <- agregar_personas(.P)
  datos <- dplyr::left_join(
    .datos, P,
    by = dplyr::join_by(HB010 == pi01, HB020 == pi02, HB030 == pi04)
  )

  datos <- construir_vbles_h(datos, P)

  if (bloques["D"]) {
    D <- dplyr::select(.D, DB010, DB020, DB030, DB040, DB090)
    datos <- dplyr::left_join(
      datos, D, by = dplyr::join_by(HB010 == DB010, HB020 == DB020, HB030 == DB030)
    )
    datos <- dplyr::rename(datos, hi06 = DB090)
  } else {
    rlang::warn("No se proporciono el conjunto D. Se omiten: `hi06`.")
  }

  if (bloques["LMH"]) {
    datos <- datos |>
      dplyr::mutate(
        dplyr::across(
          .cols = c(py01:py03),
          .fns = \(y) y / 12, .names = "{.col}m"
        ),
        dplyr::across(
          .cols = c(py01:py03),
          .fns = \(y) y / hd01, .names = "{.col}c"
        ),
        hyxxq = "py01 a py03 / PPA correspondiente",
        .keep = "all"
      )
  } else {
    rlang::warn("No se encontro `PL130` o `PL230` en `.P`. Se omiten: `py01`, `py02`, `py03`.")
  }

  # Arreglos y devolver ------------------------------------------------------
  if (!.expandir) {
    datos <- dplyr::select(
      datos,
      dplyr::starts_with(c("hi", "hd", "hl", "py", "hy", "hp"), ignore.case = FALSE)
    )
  } else {
    datos <- dplyr::relocate(
      datos,
      dplyr::starts_with(c("hi", "hd", "hl", "py", "hy", "hp"), ignore.case = FALSE),
      dplyr::everything()
    )
  }

  attr(datos, "base") <- "H"
  attr(datos, "bloques") <- bloques
  attr(datos, "expandida") <- .expandir

  return(datos)
}

# ============================================================================
#' Agrega variables de ingreso de la base P de la EU-SILC a nivel hogar.
#'
#' @param .personas Conjunto P de la EU-SILC expandido con [construir_vbles_p()] y, opcionalmente, con [construir_vbles_p_lmh()].
#'
#' @returns Conjunto de datos con ingresos individuales agregados a nivel hogar.
agregar_personas <- function(.personas) {
  personas <- .personas |>
    dplyr::summarise(
      # Bloque Y -----------------------
      dplyr::across(c(dplyr::any_of(c("py01", "py02", "py03")), py04:py13), sum),
      # Bloque P -----------------------
      dplyr::across(c(dplyr::any_of(c("py01", "py02", "py03")), py04:py13), \(y) sum(y != 0), .names = "x{.col}"),
      .by = c(pi01, pi02, pi04)
    )
  personas <- personas |>
    dplyr::rename_with(.cols = dplyr::starts_with("xpy"), .fn = \(n) sub("xpy", "hp", n))

  # ------------------------------------------
  return(personas)
}

# ============================================================================
#' Construye variables en la base H de la EU-SILC.
#'
#' @param .datos Conjunto H de la EU-SILC.
#' @param ... ...
#'
#' @returns Conjunto H de la EU-SILC con variables adicionales.
construir_vbles_h <- function(
    .datos,
    ...
) {
  hogares <- .datos |>
    dplyr::mutate(
      # Bloque I -----------------------
      hi01 = HB010,
      hi02 = HB020,
      hi04 = HB030,
      # Bloque D -----------------------
      hd01 = HX040,
      hd02a = "A definir",
      hd02b = "A definir",
      # Bloque L -----------------------
      # Bloque Y -----------------------
      hy14 = HY040N + HY090N,
      hy15 = HY050N + HY060N + HY070N,
      hy16 = HY080N + HY110N,
      hy17 = hy14 + hy16,
      hy18 = py08 + py09 + hy15,
      hy19 = py10 + hy18,
      hy20 = py12 + hy14 + hy15 + hy16,
      hy21 = py13 + hy14 + hy15 + hy16,
      dplyr::across(
        .cols = c(dplyr::any_of(c("py01", "py02", "py03")), py04:py13, hy14:hy21),
        .fns = \(y) y / 12, .names = "{.col}m"
      ),
      dplyr::across(
        .cols = c(dplyr::any_of(c("py01", "py02", "py03")), py04:py13, hy14:hy21),
        .fns = \(y) y / hd01, .names = "{.col}pc"
      ),
      hyxxq = "py01 a hy21 / PPA correspondiente",
      .keep = "all"
    )

  # ------------------------------------------
  return(hogares)
}
