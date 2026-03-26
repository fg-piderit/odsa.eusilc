#' Title
#'
#' @param .datos descripcion
#' @param .base descripcion
#' @param ... descripcion
#'
#' @returns valor
#' @export
expandir_eusilc <- function(
    .datos,
    #.tipo = c("cross", "long"),
    .base,
    ...,
    .individuos = NULL
) {
  # Chequeos ---------------------------------
  if (!is.data.frame(.datos)) {
    rlang::abort("`.datos` debe ser un data.frame o tibble")
  }
  #.tipo <- rlang::arg_match(.tipo)
  .base <- rlang::arg_match(.base, c("P", "H"))

  if (.base == "P") {
    .datos <- construir_variables_p(.datos, .pais)
  } else {
    .datos <- construir_variables_h(.datos, .pais, .individuos)
  }
  #if (.base == "H" & is.null(.individuos)) {
  #  mensaje <- paste0("Para expandir la base tipo `H` se debe proveer `.individuos`.\n",
  #                    "`.individuos` debe ser una base tipo `P` expandida con `expandir_eusilc()`.")
  #  rlang::abort(mensaje)
  #}

  # ------------------------------------------
  return(.datos)
}

# Auxiliares -----------------------------------------------------------------
chequear_variables <- function(.variables, .tipos, .necesarias) {
  faltan <- .necesarias[!(.necesarias %in% .variables)]
  if (length(faltan) != 0) {
    mensaje <- paste0("Faltan las siguientes variables en .datos: `",
                      paste0(faltan, collapse = "`, `"), "`")
    rlang::abort(mensaje)
  }

  no_numericas <- .necesarias[!(.tipos[.necesarias] %in% c("numeric", "integer"))]
  no_numericas <- no_numericas[no_numericas != "PB020" & no_numericas != "HB020"]
  if (length(no_numericas) != 0) {
    mensaje <- paste0("Las siguientes variables requeridas deberian ser numericas y no lo son: `",
                      paste0(no_numericas, collapse = "`, `"), "`")
    rlang::abort(mensaje)
  }
}

chequear_pais_anno <- function(.pais, .anno) {
  if (length(.pais) > 1) {
    rlang::abort("Por ahora solo se admiten bases de un unico pais.")
  }
  if (length(.anno) > 1) {
    rlang::abort("Por ahora solo se admiten bases de un unico anno.")
  }
  if (!(.pais %in% paises_admitidos)) {
    mensaje <- paste0("La base debe ser de uno de los siguientes paises: `",
                      paste0(paises_admitidos, collapse = "`, `"),
                      ", no `", .pais, "`.")
    rlang::abort(mensaje)
  }
  if (.anno < 2021) {
    rlang::abort("Por ahora solo se admiten bases posteriores a 2021.")
  }
}
