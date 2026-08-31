#' Advertencias documentadas para la estandarizacion de EU-SILC
#'
#' Contiene perdidas de informacion, problemas de la UDB y diferencias de
#' comparabilidad que afectan variables utilizadas por el paquete.
#'
#' @format ## `advertencias_estandarizacion`
#' Un tibble con una fila por advertencia, anio, pais y variable:
#' \describe{
#'   \item{id_advertencia}{Identificador estable de la advertencia.}
#'   \item{anio}{Anio de la operacion EU-SILC.}
#'   \item{pais}{Codigo de pais de dos letras.}
#'   \item{base}{Flujo de estandarizacion afectado: P o H.}
#'   \item{conjunto_origen}{Conjunto donde se encuentra la variable.}
#'   \item{variable}{Variable original afectada.}
#'   \item{tipo}{Clase de perdida o diferencia documentada.}
#'   \item{advertencia}{Descripcion breve del problema.}
#'   \item{consecuencia}{Resultados del paquete afectados.}
#'   \item{accion_paquete}{Tratamiento que aplica el paquete.}
#'   \item{fuente}{Documento general utilizado como fuente.}
#' }
#' @source Documentacion de EU-SILC disponible con las entregas UDB de
#'   Eurostat.
"advertencias_estandarizacion"

#' Cobertura de la revision de advertencias de estandarizacion
#'
#' Permite distinguir una operacion revisada sin hallazgos de una operacion que
#' todavia no fue examinada.
#'
#' @format ## `cobertura_advertencias`
#' Un tibble con una fila por anio y pais:
#' \describe{
#'   \item{anio}{Anio de la operacion EU-SILC.}
#'   \item{pais}{Codigo de pais de dos letras.}
#'   \item{estado}{Estado de la revision documental.}
#'   \item{fuentes}{Familias de documentos revisadas.}
#' }
#' @source Documentacion de EU-SILC disponible con las entregas UDB de
#'   Eurostat.
"cobertura_advertencias"

#' Consulta las advertencias aplicables a un conjunto EU-SILC
#'
#' @param .datos `data.frame` o `tibble`. Conjunto P o H, antes o despues de
#'   procesarlo con el paquete. Debe corresponder a un unico anio y pais.
#'
#' @returns `tibble`. Filas de [advertencias_estandarizacion] correspondientes
#'   al anio, pais y base de `.datos`.
#'
#' @export
consultar_advertencias <- function(.datos) {
  contexto <- obtener_contexto_advertencias(.datos)
  advertencias <- obtener_advertencias_estandarizacion(
    contexto$anio,
    contexto$pais,
    contexto$base
  )

  if (nrow(advertencias) == 0) {
    cobertura <- obtener_cobertura_advertencias(contexto$anio, contexto$pais)

    if (nrow(cobertura) == 1 && cobertura$estado == "revisado") {
      cli::cli_alert_success(
        "No hay advertencias documentadas para {contexto$pais} en {contexto$anio}."
      )
    } else {
      cli::cli_alert_warning(
        "No hay una revision documental completa para {contexto$pais} en {contexto$anio}."
      )
    }
  }

  advertencias
}

obtener_contexto_advertencias <- function(.datos) {
  if (!is.data.frame(.datos)) {
    rlang::abort(
      ".datos debe ser un data.frame o tibble.",
      class = "no_data_frame"
    )
  }

  identificadores <- list(
    P = list(c("PB010", "PB020"), c("pi01", "pi02")),
    H = list(c("HB010", "HB020"), c("hi01", "hi02"))
  )

  contexto <- NULL
  for (base in names(identificadores)) {
    for (columnas in identificadores[[base]]) {
      if (all(columnas %in% names(.datos))) {
        contexto <- list(
          base = base,
          anio = unique(.datos[[columnas[1]]]),
          pais = unique(.datos[[columnas[2]]])
        )
        break
      }
    }
    if (!is.null(contexto)) break
  }

  if (is.null(contexto)) {
    rlang::abort(
      "No se pudo identificar si .datos es una base P o H.",
      class = "base_desconocida"
    )
  }
  if (length(contexto$anio) != 1 || is.na(contexto$anio)) {
    rlang::abort(
      ".datos debe corresponder a un unico anio.",
      class = "varios_anios"
    )
  }
  if (length(contexto$pais) != 1 || is.na(contexto$pais)) {
    rlang::abort(
      ".datos debe corresponder a un unico pais.",
      class = "varios_paises"
    )
  }

  contexto
}

obtener_advertencias_estandarizacion <- function(.anio, .pais, .base) {
  tabla <- get("advertencias_estandarizacion", inherits = TRUE)
  tabla[
    tabla$anio == .anio & tabla$pais == .pais & tabla$base == .base,
    ,
    drop = FALSE
  ]
}

obtener_cobertura_advertencias <- function(.anio, .pais) {
  tabla <- get("cobertura_advertencias", inherits = TRUE)
  tabla[tabla$anio == .anio & tabla$pais == .pais, , drop = FALSE]
}

informar_advertencias_estandarizacion <- function(.datos) {
  contexto <- obtener_contexto_advertencias(.datos)
  advertencias <- obtener_advertencias_estandarizacion(
    contexto$anio,
    contexto$pais,
    contexto$base
  )

  if (nrow(advertencias) > 0) {
    cantidad <- dplyr::n_distinct(advertencias$id_advertencia)
    variables <- paste(unique(advertencias$variable), collapse = ", ")
    cli::cli_bullets(c(
      "!" = "Se encontraron {cantidad} advertencia{?s} documentada{?s} para {contexto$pais} en {contexto$anio}.",
      "i" = "Variables afectadas: {variables}.",
      "i" = "Usa consultar_advertencias() para inspeccionar el detalle."
    ))
    return(invisible(advertencias))
  }

  cobertura <- obtener_cobertura_advertencias(contexto$anio, contexto$pais)
  if (nrow(cobertura) == 1 && cobertura$estado == "revisado") {
    cli::cli_alert_success(
      "No hay advertencias documentadas para {contexto$pais} en {contexto$anio}."
    )
  } else {
    cli::cli_alert_warning(
      "No hay una revision documental completa para {contexto$pais} en {contexto$anio}."
    )
  }

  invisible(advertencias)
}

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
  invisible(NULL)
}

informar_insumos_hogares <- function(.D) {
  if (is.null(.D)) {
    cli::cli_bullets(c(
      "!" = "No se proporciono el conjunto D.",
      " " = "Se pierde hi06."
    ))
  }
  invisible(NULL)
}

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

  invisible(NULL)
}
