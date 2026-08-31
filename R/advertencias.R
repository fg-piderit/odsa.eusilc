# ============================================================================
obtener_advertencias <- function(.base, .anio, .pais) {
  advertencias <- dplyr::filter(
    tabla_advertencias,
    base == .base,
    anio_desde <= .anio & anio_hasta >= .anio,
    pais == .pais
  )

  if (nrow(advertencias) > 0) {
    cantidad <- dplyr::n_distinct(advertencias$id_advertencia)
    variables <- paste(unique(advertencias$variable), collapse = ", ")

    cli::cli_bullets(c(
      "!" = "Se encontraron {cantidad} advertencia{?s} documentada{?s} para {(.pais)} en {(.anio)}.",
      "i" = "Variables afectadas: {variables}."
    ))
  } else {
    cobertura <- dplyr::filter(
      tabla_cobertura,
      anio_desde <= .anio & anio_hasta >= .anio,
      pais == .pais
    )

    if (nrow(cobertura) == 3 && all(cobertura$estado == "revisado")) {
      cli::cli_alert_success(
        "No hay advertencias documentadas para {(.pais)} en {(.anio)}."
      )
    } else {
      cli::cli_alert_warning(
        "No hay una revision documental completa para {(.pais)} en {(.anio)}."
      )
    }
  }

  return(advertencias)
}

# ============================================================================
informar_insumos_personas <- function(.P, .D, .R, .anio) {
  if (is.null(.D)) {
    cli::cli_bullets(c(
      "!" = "No se proporciono el conjunto D.",
      " " = "Se pierde pi03."
    ))
  }

  if (.anio >= 2021 && is.null(.R)) {
    cli::cli_bullets(c(
      "!" = "No se proporciono el conjunto R.",
      " " = "Se pierden pd01a, pd04 y pd05."
    ))
  }

  informar_disponibilidad_modulos(.P, .anio)
}

informar_insumos_hogares <- function(.D) {
  if (is.null(.D)) {
    cli::cli_bullets(c(
      "!" = "No se proporciono el conjunto D.",
      " " = "Se pierde hi06."
    ))
  }
}

# ============================================================================
informar_disponibilidad_modulos <- function(.P, .anio) {
  anio_lmh <- .anio >= 2020 && (.anio - 2020) %% 3 == 0
  esperada_pl130 <- .anio < 2021 || anio_lmh
  esperada_pl230 <- anio_lmh
  presente_pl130 <- "PL130" %in% names(.P)
  presente_pl230 <- "PL230" %in% names(.P)

  if (!presente_pl130) {
    if (esperada_pl130) {
      cli::cli_bullets(c(
        "!" = "No se encontro PL130 aunque corresponde a la operacion {(.anio)}.",
        " " = "Se pierden pl21a, pl21b, pl30, pl31, py13, py14 y py15."
      ))
    } else {
      cli::cli_bullets(c(
        "i" = "PL130 no corresponde a la operacion {(.anio)} por el calendario del modulo LMH.",
        " " = "pl21a, pl21b, pl30, pl31, py13, py14 y py15 quedaran ausentes."
      ))
    }
  }

  if (!presente_pl230) {
    if (esperada_pl230) {
      cli::cli_bullets(c(
        "!" = "No se encontro PL230 aunque corresponde a la operacion {(.anio)}.",
        " " = "Se pierden pl22, pl30, pl31, py13, py14 y py15."
      ))
    } else {
      cli::cli_bullets(c(
        "i" = "PL230 no corresponde a la operacion {(.anio)} por el calendario del modulo LMH.",
        " " = "pl22, pl30, pl31, py13, py14 y py15 quedaran ausentes."
      ))
    }
  }
}
