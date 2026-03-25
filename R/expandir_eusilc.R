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
    ...
    #.individuos = NULL
) {
  # Chequeos ---------------------------------
  if (!is.data.frame(.datos)) {
    rlang::abort("`.datos` debe ser un data.frame o tibble")
  }
  #.tipo <- rlang::arg_match(.tipo)
  .base <- rlang::arg_match(.base, c("P", "H"))

  variables <- names(.datos)
  tipos_var <- purrr::map_chr(.datos, class)

  if (.base == "P") {
    if (any(!(c("PB010", "PB020") %in% variables))) {
      rlang::abort("Falta `PB010` o `PB020` (pais).")
    }
    if (!(tipos_var["PB010"] %in% c("numeric", "integer"))) {
      rlang::abort("`PB010` debe ser numerica.")
    }

    anno <- unique(.datos$PB010)
    pais <- unique(.datos$PB020)
    chequear_pais_anno(pais, anno)
    chequear_variables(variables, tipos_var, necesarias_p[[as.character(anno)]])

    .datos <- construir_variables_p(.datos, .pais)
  } else {
    #TODO
    rlang::warn("Todavia no prepare nada para las bases H")
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
